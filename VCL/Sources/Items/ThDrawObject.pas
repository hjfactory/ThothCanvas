{
  Role
    Draw shapes with the mouse actions.
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
  public
    constructor Create(AStyle: IThDrawStyle); virtual;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState); virtual; abstract;
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
    procedure MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;
  end;

  // 지우개 베이스 클래스
  TThBaseEraserDrawObject = class(TThCustomDrawObject)
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
  TThObjErsDrawObject = class(TThBaseEraserDrawObject)
  public
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;
  end;

  /// Shape Objects base
  TThShapeDrawObject = class(TThCustomDrawObject)
  private
    FShapeId: string;
    FDrawItem: TThShapeItem;
  protected
    FRect: TFloatRect;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;

    property ShapeId: string read FShapeId write FShapeId;
  end;

  // Shape Select
  TThSelectObject = class(TThCustomDrawObject)
  private
    FLastPoint: TFloatPoint;
    FDrawItems: TThDrawItems;
    FSelected: TThShapeItem;
    FSelectedItems: TThShapeDrawItems;

    function CalcSelection(ASelectingItem: TThShapeItem; AShift: TShiftState): TThShapeItem;
    procedure SetSelected(const Value: TThShapeItem);
  public
    constructor Create(AItems: TThDrawItems); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    property Items: TThShapeDrawItems read FSelectedItems;
    property Selected: TThShapeItem read FSelected write SetSelected;
    procedure DeleteSelectedItems;

    procedure SetDrawItems(ADrawItems: TThDrawItems);

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
end;

procedure TThCustomDrawObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
begin
end;

procedure TThCustomDrawObject.MouseUp;
begin
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
  FDrawItem := TThPenDrawItem.Create(FDrawStyle);
  FPath.Add(APoint);

  Poly := Circle(APoint, FDrawItem.Thickness / 2);
  FPolyPoly := PolyPolygon(Poly);
  FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
end;

procedure TThPenDrawObject.MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
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

constructor TThBaseEraserDrawObject.Create(AStyle: IThDrawStyle;
  AItems: TThDrawItems);
begin
  if not Assigned(AStyle) then
    AStyle := TThEraserStyle.Create;
  inherited Create(AStyle);

  FDrawItems := AItems;
end;

procedure TThBaseEraserDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Circle(FPos, DrawStyle.Thickness / 2);
  PolylineFS(Bitmap, Poly, clBlack32, True);
end;

function TThBaseEraserDrawObject.GetDrawStyle: TThEraserStyle;
begin
  Result := TThEraserStyle(FDrawStyle);
end;

{ TThObjErsDrawObject }

procedure TThObjErsDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FPos := APoint;
  MouseDownMove(APoint, AShift);
end;

procedure TThObjErsDrawObject.MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Poly: TThPoly;
  LDrawItems: TArray<TThDrawItem>;
begin
  FPos := APoint;
  Poly := Circle(APoint, DrawStyle.Thickness / 2);
  LDrawItems := FDrawItems.PolyInItems(Poly);
  for I := 0 to Length(LDrawItems) - 1 do
    TThPenDrawItem(LDrawItems[I]).IsDeletion := True;
end;

procedure TThObjErsDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Item: TThDrawItem;
begin
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
  FRect.TopLeft := APoint;
end;

procedure TThShapeDrawObject.MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState);
begin
  if not Assigned(FDrawItem) then
    FDrawItem := TThShapeItemFactory.GetShapeItem(FShapeId, FDrawStyle);

  FRect.BottomRight := APoint;
end;

procedure TThShapeDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FRect := EmptyRect;
end;

procedure TThShapeDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  if Assigned(FDrawItem) then
    FDrawItem.DrawRect(Bitmap, AScale, AOffset, FRect);
end;

function TThShapeDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := nil;
  if not Assigned(FDrawItem) then
    Exit;

  Result := FDrawItem;
  FDrawItem := nil;
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
  FSelectedItems := TThShapeDrawItems.Create;
end;

procedure TThSelectObject.DeleteSelectedItems;
var
  Item: TThDrawItem;
begin
  for Item in FSelectedItems do
    FDrawItems.Remove(Item);
  FSelectedItems.Clear;
end;

destructor TThSelectObject.Destroy;
begin
  FSelectedItems.Free;

  inherited;
end;

procedure TThSelectObject.SetDrawItems(ADrawItems: TThDrawItems);
begin
  FDrawItems := ADrawItems;
  FSelectedItems := TThShapeDrawItems.Create;
end;

procedure TThSelectObject.SetSelected(const Value: TThShapeItem);
begin
  ClearSelection;

  FSelected := Value;
  Value.Selected := True;
  FSelectedItems.Add(Value);
end;

function TThSelectObject.CalcSelection(ASelectingItem: TThShapeItem; AShift: TShiftState): TThShapeItem;
begin
  Result := ASelectingItem;
  if Assigned(ASelectingItem) then
  begin
    // 선택되지 않은 경우 선택 처리(이미 선택된 경우 무시)
    if ASelectingItem.Selected then
    begin
      if ssShift in AShift then
      begin
        ASelectingItem.Selected := False;
        FSelectedItems.Remove(ASelectingItem);
        Result := nil;
      end;
    end
    else
    begin
      if not (ssShift in AShift)  then
        ClearSelection;

      ASelectingItem.Selected := True;
      FSelectedItems.Add(ASelectingItem);
    end;
  end
  else
  begin
    if not (ssShift in AShift) then
      ClearSelection;
  end;
end;

procedure TThSelectObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
var
  Item: TThDrawItem;
begin
  FLastPoint := APoint;

  Item := FDrawItems.PtInItem(APoint);

  FSelected := CalcSelection(Item as TThShapeItem, AShift);
end;

procedure TThSelectObject.MouseDownMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  P: TFloatPoint;
begin
  if Assigned(FSelected) then
  begin
    P := APoint - FLastPoint;
    FSelectedItems.Move(P);
    FLastPoint := APoint;
    Screen.Cursor := crSizeAll;
  end;
end;

procedure TThSelectObject.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  Item: TThDrawItem;
begin
  inherited;

  Item := FDrawItems.PtInItem(APoint);
  if Assigned(Item) then
    Screen.Cursor := crSizeAll;
end;

procedure TThSelectObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;
end;

end.
