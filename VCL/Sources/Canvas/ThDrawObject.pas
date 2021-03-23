{
  Role
    Draw shapes using the mouse actions.
}

unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32, GR32_Polygons, GR32_VectorUtils,
  clipper,

  ThTypes, ThClasses, ThItemStyle,
  ThCanvasEventProcessor,
  ThItem, ThShapeItem, ThItemCollections;

type
  TThCustomDrawObject = class(TThInterfacedObject, IThDrawObject)
  private
    FDrawStyle: IThDrawStyle;
  protected
    FMouseDowned: Boolean;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); virtual;

    function GetItemInstance: IThItem; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  // 자유선으로 그리기 객체(Free draw object)
  TThPenDrawObject = class(TThCustomDrawObject)
  private
    FPenItem: TThPenItem;

    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TThPolyPoly;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetItemInstance: IThItem; override;
  end;

  // 지우개 베이스 클래스
  TThCustomEraserObject = class(TThCustomDrawObject)
  private
    FItemList: TThItemList;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  protected
    FPos: TFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThItemList); reintroduce;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
  end;

  // 지나간 객체 지우개(Passed objects eraser)
  TThObjectEraserObject = class(TThCustomEraserObject)
  public
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;
  end;

  // Shape (multi)select
    // Select > Move, Delete, Resize, Link
  TThShapeDrawObject = class(TThCustomDrawObject)
  private
    FDragState: TThShapeDragState;

    FMouseProcessor: TThShapeDrawMouseProcessor;

    FLastPoint: TFloatPoint;

    FItemList: TThItemList;
    FSelectedItems: TThSelectedItems;

    FSelectedItem: IThSelectableItem; // 필요한가?

    FShapeId: string;

    procedure SetShapeId(const Value: string);
    procedure SetDragState(const Value: TThShapeDragState);
    procedure SetSelectedItem(const Value: IThSelectableItem);

    property DragState: TThShapeDragState read FDragState write SetDragState;
    property SelectedItem: IThSelectableItem read FSelectedItem write SetSelectedItem;

//    procedure ProcessSelect(ASelectingItem: IThSelectableItem; AIsMultipleSelect: Boolean);
  public
    constructor Create(AItems: TThItemList); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure DeleteSelectedItems;

    property ShapeId: string read FShapeId write SetShapeId;
  end;

implementation

uses
//  Winapi.Windows, // ODS
  Vcl.Forms, System.UITypes,
  ThUtils,
  System.SysUtils,
  System.Math;

{ TThDrawObject }

constructor TThCustomDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

procedure TThCustomDrawObject.MouseDown;
begin
  FMouseDowned := True;
end;

procedure TThCustomDrawObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
begin
end;

procedure TThCustomDrawObject.MouseUp;
begin
  FMouseDowned := False;
end;

function TThCustomDrawObject.GetItemInstance: IThItem;
begin
  Result := nil;
end;

procedure TThCustomDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
end;

{ TThPenObject }

constructor TThPenDrawObject.Create(AStyle: IThDrawStyle);
begin
  inherited;

  FPath := TList<TFloatPoint>.Create;
end;

destructor TThPenDrawObject.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
var
  LPoly: TThPoly;
begin
  inherited;

  FPenItem := TThPenItem.Create;
  FPenItem.SetStyle(FDrawStyle);
  FPath.Add(APoint);

  LPoly := Circle(APoint, FPenItem.Thickness / 2);
  FPolyPoly := GR32_VectorUtils.PolyPolygon(LPoly);
  FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
end;

procedure TThPenDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastPt: TFloatPoint; // Adjusted point
begin
  inherited;

  if FMouseDowned then
  begin
    FPath.Add(APoint);

    LastPt := FPath.Items[FPath.Count-2];
    Poly := BuildPolyline([LastPt, APoint], FPenItem.Thickness, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly, 3);

    with TClipper.Create do
    try
      AddPaths(FPolyPolyPath, ptSubject, True);
      AddPath(PolyPath, ptClip, True);

      Execute(ctUnion, FPolyPolyPath, pftNonZero);
    finally
      Free;
    end;

    FPolyPoly := AAPoint2AAFloatPoint(FPolyPolyPath, 3);
  end;
end;

procedure TThPenDrawObject.MouseUp;
begin
  inherited;

  FPath.Clear;
  FPolyPolyPath := nil;
  FPolyPoly := nil;

  FPenItem := nil;
end;

procedure TThPenDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
  if Assigned(FPenItem) then
    FPenItem.DrawPoly(Bitmap, AScale, AOffset, FPath.ToArray, FPolyPoly);
end;

function TThPenDrawObject.GetItemInstance: IThItem;
begin
  Result := FPenItem;
  FPenItem := nil;
end;

{ TThEraserDrawObject }

constructor TThCustomEraserObject.Create(AStyle: IThDrawStyle;
  AItems: TThItemList);
begin
  if not Assigned(AStyle) then
    AStyle := TThEraserStyle.Create;
  inherited Create(AStyle);

  FItemList := AItems;
end;

procedure TThCustomEraserObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Circle(FPos, DrawStyle.Thickness / 2);
  PolylineFS(Bitmap, Poly, clBlack32, True);
end;

function TThCustomEraserObject.GetDrawStyle: TThEraserStyle;
begin
  Result := TThEraserStyle(FDrawStyle);
end;

{ TThObjectEraserObject }

procedure TThObjectEraserObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FPos := APoint;
  MouseMove(APoint, AShift);
end;

procedure TThObjectEraserObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Poly: TThPoly;
  LItems: TArray<IThItem>;
begin
  inherited;

  if FMouseDowned then
  begin
    FPos := APoint;
    Poly := Circle(APoint, DrawStyle.Thickness / 2);
    LItems := FItemList.GetPolyInItems(Poly);
    for I := 0 to Length(LItems) - 1 do
      TThPenItem(LItems[I]).IsDeletion := True;
  end;
end;

procedure TThObjectEraserObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Item: IThItem;
begin
  inherited;

  for I := FItemList.Count - 1 downto 0 do
  begin
    Item := FItemList[I];
    if TThPenItem(Item).IsDeletion then
      FItemList.Delete(I);
  end;
end;

{ TThShapeDrawObject }

constructor TThShapeDrawObject.Create(AItems: TThItemList);
begin
  inherited Create(TThShapeStyle.Create);

  FItemList := AItems;
  FSelectedItems := TThSelectedItems.Create;

  FMouseProcessor := TThShapeDrawMouseProcessor.Create(FItemList, FSelectedItems);
end;

destructor TThShapeDrawObject.Destroy;
begin
  FMouseProcessor.Free;
  FSelectedItems.Free;

  inherited;
end;

// Single(Click)
  // Canvas         : Clear Selections
  // Item           : Select Item
  // Selected Item  : None
  // Item Handle    : Start drag
// Multiple(Shift + Click)
  // Canvas         : None
  // Item           : Add to selection
  // Selected Item  : Remove from selection
  // Item Handle    : Start drag
//procedure TThSelectObject.ProcessSelect(ASelectingItem: IThSelectableItem; AIsMultipleSelect: Boolean);
//begin
//  FDragMode := dmNone;
//
//  if not Assigned(ASelectingItem) then
//  // Canvas click
//  begin
//    if not AIsMultipleSelect then
//      FItemList.ClearSelected;
////      ClearSelection;
//    FSelectedItem := nil;
//  end
//  else
//  // Item click
//  begin
//    if not ASelectingItem.Selected then
//    // Unselected Item click
//    begin
//      if not AIsMultipleSelect then
//        ClearSelection;
//
//      ASelectingItem.Selected := True;
////      FItemList2.Add(ASelectingItem);
////      FSelectedItems.Add(ASelectingItem);
//      FSelectedItem := ASelectingItem;
//
//      FDragMode := dmItemMove;
//    end
//    else
//    // Selected item click
//    begin
//      if not Assigned(ASelectingItem.Selection.HotHandle) then
//      // Item click
//      begin
//        if AIsMultipleSelect then
//        // Cancel select of Selecting item
//        begin
//          ASelectingItem.Selected := False;
////          FSelectedItems.Remove(ASelectingItem);
//          FSelectedItem := nil;
//        end
//        else
//        begin
//          FSelectedItem := ASelectingItem;
//
//          FDragMode := dmItemMove;
//        end;
//      end
//      else
//      // Handle click
//      begin
//        FDragMode := dmItemResize;
//        FSelectedItem := ASelectingItem;
//      end;
//    end;
//  end;
//end;

procedure TThShapeDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
var
  TargetItem: IThSelectableItem;
  PressedShift: Boolean;
//  ConnectionItem: IThConnectableItem;
begin
  inherited;

  FLastPoint := APoint;

  FMouseProcessor.MouseDown(APoint, AShift);

  if FDragState = dsItemAdd then
  begin
    SelectedItem := TThShapeItemFactory.GetShapeItem(FShapeId, FDrawStyle);
    SelectedItem.ResizeItem(FLastPoint, FLastPoint);
    FItemList.Add(SelectedItem);

    DragState := dsItemResize;
    FShapeId := '';
  end
  else if FDragState = dsNone then
  begin
    {
       Click
         Canvas         : Clear Selections
         Item           : Clear selection, Select Item
         Selected Item  : None
         Item Handle    : Start drag
       Shift + Click
         Canvas         : None
         Item           : Add to selection
         Selected Item  : Remove from selection
         Item Handle    : Don't show
    }
    // Select item(Unselect, Multi-select)
    TargetItem := FMouseProcessor.TargetItem;
    PressedShift := AShift * [ssShift, ssCtrl] <> [];

    // Canvas click
    if not Assigned(TargetItem) then
    begin
      if not PressedShift then
        FSelectedItems.Clear;
    end
    else
    begin
      if not TargetItem.Selected then
      // Non-selected item click
      begin
        if not PressedShift then
          FSelectedItems.Clear;
        FSelectedItems.Add(Targetitem);
      end
      else
      // Selected item click
      begin
        if PressedShift then
        begin
          TargetItem.Selected := False;
          FSelectedItems.Remove(TargetItem);
        end;
      end;
    end;

    if Assigned(TargetItem) and TargetItem.Selected then
      if Assigned(TargetItem.Selection.HotHandle) then
        DragState := dsItemResize
      else
        DragState := dsItemMove;


//    DragState := FMouseProcessor.CalcDragState(APoint);

    // DragState 결정
    // dsResize
    // dsMove
  end;



//  FItemList.MouseDown(APoint);

//  ProcessSelect(
//    FItemList.TargetItem,               // ASelectingItem
//    AShift * [ssShift, ssCtrl] <> []    // AIsMultipleSelect
//  );

//  if FDragMode = dmItemResize then
//  begin
//    if Supports(FSelectedItem, IThItemConnector) then
//    begin
//      ConnectionItem := FItemList.GetConnectionItem(APoint);
//      if Assigned(ConnectionItem) then
//      begin
//        ConnectionItem.ShowConnection;
//        ConnectionItem.MouseDown(APoint);
//      end;
//    end;
//  end;

end;

procedure TThShapeDrawObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  MovePoint: TFloatPoint;
  ConnectionItem: IThConnectableItem;
begin
  inherited;

  FMouseProcessor.MouseMove(APoint, AShift);

  if FMouseDowned then
  begin
    case FDragState of
    dsItemMove:
      begin
        MovePoint := APoint - FLastPoint;
        FSelectedItems.MoveItems(MovePoint);
        FLastPoint := APoint;
      end;
    dsItemResize:
      begin
        FSelectedItem.Selection.ResizeItem(APoint);
      end;
    end;
  end;

  // Resize

//  FItemList.MouseMove(APoint);

//  if FMouseDowned then
//  begin
//    if FDragMode = dmItemMove then
//    begin
//      MovePoint := APoint - FLastPoint;
//      FItemList.MoveSelectedItems(MovePoint);
////      FSelectedItems.MoveItems(MovePoint);
//      FLastPoint := APoint;
//    end
//    else if FDragMode = dmItemResize then
//    begin
//      FSelectedItem.Selection.ResizeItem(APoint);
//
//      { TODO : 현재 선택된 항목이 Connector이며,
//        포인트가 위치한 항목(크기 조정 중인 항목 제외)이 Connection 지원하는 경우
//        AnchorPoint  표시 }
//      if Supports(FSelectedItem, IThConnectorItem) then
//      begin
//        ConnectionItem := FItemList.GetConnectionItem(APoint);
//        if Assigned(ConnectionItem) then
//        begin
//          ConnectionItem.ShowConnection;
//          ConnectionItem.MouseMove(APoint);
//        end;
//      end;
//    end;
//  end
//  else
//  begin
//  end;
end;

procedure TThShapeDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
var
  ConnectionItem: IThConnectableItem;
begin
  inherited;

  FMouseProcessor.MouseUp(APoint, AShift);

  DragState := dsNone;

//  FItemList.MouseUp(APoint);
//
//  if Assigned(FSelectedItem) and (FItemList.TargetItem <> FSelectedItem) then
//    FSelectedItem.MouseUp(APoint);
//
//  if FDragMode = dmItemResize then
//  begin
//    if Supports(FSelectedItem, IThConnectorItem) then
//    begin
//      ConnectionItem := FItemList.GetConnectionItem(APoint);
//      if Assigned(ConnectionItem) then
//      begin
//        ConnectionItem.HideConnection;
//        ConnectionItem.MouseUp(APoint);
//      end;
//    end;
//  end;
//
//  FDragMode := dmNone;
//
//  inherited;
end;

procedure TThShapeDrawObject.SetDragState(const Value: TThShapeDragState);
begin
  FDragState := Value;

  case FDragState of
  dsNone:       ;
  dsItemAdd:    Screen.Cursor := crCross;
  dsItemMove:   ;
  dsItemResize: Screen.Cursor := crCross;
  end;
end;

procedure TThShapeDrawObject.SetSelectedItem(const Value: IThSelectableItem);
begin
  FSelectedItem := Value;

  FSelectedItems.Clear;
  if Assigned(Value) then
  begin
    Value.Selected := True;
    FSelectedItems.Add(Value);
  end;
end;

procedure TThShapeDrawObject.SetShapeId(const Value: string);
begin
  FShapeId := Value;

  if FShapeId = '' then
    DragState := dsNone
  else
    DragState := dsItemAdd;
end;

procedure TThShapeDrawObject.DeleteSelectedItems;
var
  I: Integer;
  Item: IThSelectableItem;
  Item1, Item2: IInterface;
begin
  FSelectedItem := nil;
  for Item in FSelectedItems do
  begin
    // FSelectedItems.Item(IThSelectableItem)으로
    // FItemList.Item(IThItem) 삭제(Remove) 시
    // 포인터가 달라 지워지지않음
    // 객체(또는 IInterface)로 전환 후 비교 후 삭제필요(ㅠㅜㅠㅜ)
      // https://blog.excastle.com/2008/05/10/interfaces-and-reference-equality-beware/
//    FItems.Remove(Item as IThItem);
    for I := FItemList.Count - 1 downto 0 do
    begin
      Item1 := FItemList[I] as IInterface;
      Item2 := Item as IInterface;

      if Item1 = Item2 then
        FItemList.Delete(I);
    end;
  end;
  FSelectedItems.Clear;
end;

end.
