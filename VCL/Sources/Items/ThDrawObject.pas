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

  ThTypes, ThClasses, ThDrawStyle,
  ThDrawItem, ThShapeItem;

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

    function GetDrawItem: IThDrawItem; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  // 자유선으로 그리기 객체(Free draw object)
  TThPenDrawObject = class(TThCustomDrawObject)
  private
    FDrawItem: TThPenDrawItem;

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

    function GetDrawItem: IThDrawItem; override;
  end;

  // 지우개 베이스 클래스
  TThCustomEraserObject = class(TThCustomDrawObject)
  private
    FDrawItems: TThDrawItems;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  protected
    FPos: TFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThDrawItems); reintroduce;

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
    FShapeItem: TThShapeItem;
  protected
    FStartPoint, FCurrPoint: TFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;

    property ShapeId: string read FShapeId write FShapeId;
  end;

  // Shape (multi)select
    // Move, Delete, Resize
  TDragState = (dsNone, dsMove, dsResize);
  TThSelectObject = class(TThCustomDrawObject)
  private
    FDragState: TDragState;

    FLastPoint: TFloatPoint;
    FDrawItems: TThDrawItems;
    FSelected: TThShapeItem;
    FSelectedItems: TThSelectedItems;

    procedure ProcessSelections(ASelectingItem: TThShapeItem; AIsMultipleSelect: Boolean);
  public
    constructor Create(AItems: TThDrawItems); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure DeleteSelectedItems;

    procedure ClearSelection;
  end;

implementation

uses
  ThUtils, System.Math,
  Vcl.Forms, System.UITypes;

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

function TThCustomDrawObject.GetDrawItem: IThDrawItem;
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
  Poly: TThPoly;
begin
  inherited;

  FDrawItem := TThPenDrawItem.Create;
  FDrawItem.SetStyle(FDrawStyle);
  FPath.Add(APoint);

  Poly := Circle(APoint, FDrawItem.Thickness / 2);
  FPolyPoly := PolyPolygon(Poly);
  FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
end;

procedure TThPenDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
begin
  inherited;

  if FMouseDowned then
  begin
    FPath.Add(APoint);

    LastP := FPath.Items[FPath.Count-2];
    Poly := BuildPolyline([LastP, APoint], FDrawItem.Thickness, jsRound, esRound);
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

  FDrawItem := nil;
end;

procedure TThPenDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
  if Assigned(FDrawItem) then
    FDrawItem.DrawPoly(Bitmap, AScale, AOffset, FPath.ToArray, FPolyPoly);
end;

function TThPenDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := FDrawItem;
  FDrawItem := nil;
end;

{ TThEraserDrawObject }

constructor TThCustomEraserObject.Create(AStyle: IThDrawStyle;
  AItems: TThDrawItems);
begin
  if not Assigned(AStyle) then
    AStyle := TThEraserStyle.Create;
  inherited Create(AStyle);

  FDrawItems := AItems;
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

{ TThObjErsDrawObject }

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
  LDrawItems: TArray<TThDrawItem>;
begin
  inherited;

  if FMouseDowned then
  begin
    FPos := APoint;
    Poly := Circle(APoint, DrawStyle.Thickness / 2);
    LDrawItems := FDrawItems.PolyInItems(Poly);
    for I := 0 to Length(LDrawItems) - 1 do
      TThPenDrawItem(LDrawItems[I]).IsDeletion := True;
  end;
end;

procedure TThObjectEraserObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Item: TThDrawItem;
begin
  inherited;

  for I := FDrawItems.Count - 1 downto 0 do
  begin
    Item := FDrawItems[I];
    if TThPenDrawItem(Item).IsDeletion then
      FDrawItems.Delete(I);
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
//  FRect.TopLeft := APoint;
end;

procedure TThShapeDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  if FMouseDowned then
  begin
    if not Assigned(FShapeItem) then
      FShapeItem := TThShapeItemFactory.GetShapeItem(FShapeId, FDrawStyle);

    FCurrPoint := APoint;
//    FRect.BottomRight := APoint;
  end;
end;

procedure TThShapeDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FStartPoint := EmptyPoint;
  FCurrPoint := EmptyPoint;
//  FRect := EmptyRect;
end;

procedure TThShapeDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  if Assigned(FShapeItem) then
    FShapeItem.DrawPoints(Bitmap, AScale, AOffset, FStartPoint, FCurrPoint);
end;

function TThShapeDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := nil;
  if not Assigned(FShapeItem) then
    Exit;

  Result := FShapeItem;
  FShapeItem := nil;
end;

{ TThShapeSelectObject }

procedure TThSelectObject.ClearSelection;
var
  Item: TThShapeItem;
begin
  for Item in FSelectedItems do
    Item.Selected := False;
  FSelectedItems.Clear;
end;

constructor TThSelectObject.Create(AItems: TThDrawItems);
begin
  FDrawItems := AItems;
  FSelectedItems := TThSelectedItems.Create;
end;

destructor TThSelectObject.Destroy;
begin
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
procedure TThSelectObject.ProcessSelections(ASelectingItem: TThShapeItem; AIsMultipleSelect: Boolean);
begin
  FDragState := dsNone;

  // Canvas click
  if not Assigned(ASelectingItem) then
  begin
    if not AIsMultipleSelect then
      ClearSelection;
    FSelected := nil;
  end
  else
  begin
    // None selected Item click
    if not ASelectingItem.Selected then
    begin
      if not AIsMultipleSelect then
        ClearSelection;

      ASelectingItem.Selected := True;
      FSelectedItems.Add(ASelectingItem);
      FSelected := ASelectingItem;
      FDragState := dsMove;
    end
    else
    begin
      // Selected item click
      if not ASelectingItem.Selection.IsOverHandle  then
      begin
        if AIsMultipleSelect then
        begin
          ASelectingItem.Selected := False;
          FSelectedItems.Remove(ASelectingItem);
          FSelected := nil;
        end
        else
        begin
          FSelected := ASelectingItem;
          FDragState := dsMove;
        end;
      end
      // Selected Item Handle click
      else
      begin
        FDragState := dsResize;
        FSelected := ASelectingItem;
        ASelectingItem.Selection.MouseDown(FLastPoint);
      end;
    end;
  end;
end;

procedure TThSelectObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
var
  SelectingItem: TThShapeItem;
begin
  inherited;

  FLastPoint := APoint;

  SelectingItem := FDrawItems.PtInItem(APoint) as TThShapeItem;
  ProcessSelections(SelectingItem, AShift * [ssShift, ssCtrl] <> []);
end;

procedure TThSelectObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  P: TFloatPoint;
//  Item: TThShapeItem;
begin
  inherited;

  if FMouseDowned then
  begin
    if Assigned(FSelected) then
    begin
      if (FDragState = dsMove) then
      begin
        P := APoint - FLastPoint;
        FSelectedItems.MoveItem(P);
        FLastPoint := APoint;
        Screen.Cursor := crSizeAll;
      end
      else if (FDragState = dsResize) then
      begin
        FSelected.Selection.MouseMove(APoint);
      end;
    end;
  end
  else
  begin
    FDrawItems.MouseOver(APoint);
  end;
end;

procedure TThSelectObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FDragState := dsNone;
  Screen.Cursor := crDefault;

  inherited;
end;

procedure TThSelectObject.DeleteSelectedItems;
var
  Item: TThDrawItem;
begin
  for Item in FSelectedItems do
    FDrawItems.Remove(Item);
  FSelectedItems.Clear;
end;

end.
