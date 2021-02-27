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
  ThItem, ThItemCollections, ThShapeItem;

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

  /// Shape Objects base
  TThShapeDrawObject = class(TThCustomDrawObject)
  private
    FShapeId: string;
    FShapeItem: IThShapeItem;
  protected
    FStartPoint, FCurrPoint: TFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetItemInstance: IThItem; override;

    property ShapeId: string read FShapeId write FShapeId;
  end;

  // Shape (multi)select
    // Select > Move, Delete, Resize, Link
  TDragMode = (dmNone, dmItemMove, dmItemResize, dmMultSelect{Drag for select});
  TThSelectObject = class(TThCustomDrawObject)
  private
    FDragMode: TDragMode;

    FLastPoint: TFloatPoint;
    FItemList: TThItemList;
//    FItemList2: TThItemList;
    FSelected: TThShapeItem;
    FSelectedItems: TThSelectedItems;

    procedure ProcessSelect(ASelectingItem: TThShapeItem; AIsMultipleSelect: Boolean);
  public
    constructor Create(AItems: TThItemList); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure DeleteSelectedItems;

    procedure ClearSelection;
  end;

implementation

uses
  Winapi.Windows, // ODS
  ThUtils,
  System.SysUtils, System.Math, System.UITypes,
  Vcl.Forms;

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
    LItems := FItemList.PolyInItems(Poly);
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

constructor TThShapeDrawObject.Create(AStyle: IThDrawStyle);
begin
  if not Assigned(AStyle) then
    AStyle := TThShapeStyle.Create;
  inherited Create(AStyle);
end;

destructor TThShapeDrawObject.Destroy;
begin
  inherited;
end;

procedure TThShapeDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FStartPoint := APoint;
end;

procedure TThShapeDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  if FMouseDowned then
  begin
    if not Assigned(FShapeItem) then
      FShapeItem := TThShapeItemFactory.GetShapeItem(FShapeId, FDrawStyle);

    FCurrPoint := APoint;
  end;
end;

procedure TThShapeDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FStartPoint := EmptyPoint;
  FCurrPoint := EmptyPoint;
end;

procedure TThShapeDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  if Assigned(FShapeItem) then
    FShapeItem.DrawPoints(Bitmap, AScale, AOffset, FStartPoint, FCurrPoint);
end;

function TThShapeDrawObject.GetItemInstance: IThItem;
begin
  Result := nil;
  if not Assigned(FShapeItem) then
    Exit;

  Result := FShapeItem;
  FShapeItem := nil;
end;

{ TThSelectObject }

constructor TThSelectObject.Create(AItems: TThItemList);
begin
  FItemList := AItems;
  FSelectedItems := TThSelectedItems.Create;
//  FItemList2 := TThItemList.Create;
end;

destructor TThSelectObject.Destroy;
begin
  FSelectedItems.Free;
//  FItemList2.Free;

  inherited;
end;

procedure TThSelectObject.ClearSelection;
begin
  FSelectedItems.Clear;
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
procedure TThSelectObject.ProcessSelect(ASelectingItem: TThShapeItem; AIsMultipleSelect: Boolean);
begin
  FDragMode := dmNone;

  if not Assigned(ASelectingItem) then
  // Canvas click
  begin
    if not AIsMultipleSelect then
      ClearSelection;
    FSelected := nil;
  end
  else
  // Item click
  begin
    if not ASelectingItem.Selected then
    // Unselected Item click
    begin
      if not AIsMultipleSelect then
        ClearSelection;

      ASelectingItem.Selected := True;
//      FItemList2.Add(ASelectingItem);
      FSelectedItems.Add(ASelectingItem);
      FSelected := ASelectingItem;

      FDragMode := dmItemMove;
    end
    else
    // Selected item click
    begin
      if not Assigned(ASelectingItem.Selection.HotHandle) then
      // Item click
      begin
        if AIsMultipleSelect then
        // Cancel select of Selecting item
        begin
          ASelectingItem.Selected := False;
          FSelectedItems.Remove(ASelectingItem);
          FSelected := nil;
        end
        else
        begin
          FSelected := ASelectingItem;

          FDragMode := dmItemMove;
        end;
      end
      else
      // Handle click
      begin
        FDragMode := dmItemResize;
        FSelected := ASelectingItem;
      end;
    end;
  end;
end;

procedure TThSelectObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FItemList.MouseDown(APoint);

  FLastPoint := APoint;

  ProcessSelect(
    FItemList.TargetItem as TThShapeItem,   // ASelectingItem
    AShift * [ssShift, ssCtrl] <> []        // AIsMultipleSelect
  );
end;

procedure TThSelectObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  MovePoint: TFloatPoint;
begin
  inherited;

  FItemList.MouseMove(APoint);
//  TargetItem := FItemList.TargetItem;

  if FMouseDowned then
  begin
    if FDragMode = dmItemMove then
    begin
      MovePoint := APoint - FLastPoint;
      FSelectedItems.MoveItems(MovePoint);
      FLastPoint := APoint;
    end
    else if FDragMode = dmItemResize then
      FSelected.Selection.ResizeItem(APoint);
  end
  else
  begin

  end;
end;

procedure TThSelectObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FItemList.MouseUp(APoint);

  FDragMode := dmNone;
//  Screen.Cursor := crDefault;

  inherited;
end;

procedure TThSelectObject.DeleteSelectedItems;
var
  I: Integer;
  Item: IThSelectableItem;
  Item1, Item2: TThItem;
begin
  for Item in FSelectedItems do
  begin
    // FSelectedItems.Item(IThSelectableItem)으로
    // FItemList.Item(IThItem) 삭제(Remove) 시
    // 포인터가 달라 지워지지않음
    // 객체로 전환 후 비교 후 삭제(ㅠㅜㅠㅜ)
//    FItemList.Remove(SelItem as IThItem);
    for I := FItemList.Count - 1 downto 0 do
    begin
      Item1 := FItemList[I] as TThItem;
      Item2 := Item as TThItem;

      if Item1 = Item2 then
      begin
        Item1 := nil;
        FItemList.Delete(I);
      end;
    end;
  end;
  FSelectedItems.Clear;
end;

end.
