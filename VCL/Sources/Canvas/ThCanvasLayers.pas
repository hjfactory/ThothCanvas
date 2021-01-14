unit ThCanvasLayers;

interface

uses
  System.Classes, System.Types, System.Math, System.Generics.Collections,
  Vcl.Controls,

  GR32,
  GR32_Layers,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes,
  ThDrawItem,
  ThDrawStyle,
  ThDrawObject;

type
  TThCustomViewLayer = class(TPositionedLayer)
  private
    FHitTest: Boolean;
    function GetOffset: TFloatPoint;
    function GetScale: TFloatPoint;
  protected
    FDrawItems: TObjectList<TThDrawItem>;

    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure Clear;

    property HitTest: Boolean read FHitTest write FHitTest;
    property Scale: TFloatPoint read GetScale;
    property Offset: TFloatPoint read GetOffset;
  end;

  TThCustomDrawLayer = class(TThCustomViewLayer)
  private
    procedure SetDrawStyle(const Value: IThDrawStyle);
  protected
    FDrawStyle: IThDrawStyle;
  public
    property DrawStyle: IThDrawStyle read FDrawStyle write SetDrawStyle;
  end;

  TShapeDrawLayer = class(TThCustomDrawLayer)
  private
    FMouseDowned: Boolean;

    FCanvasMode: TThCanvasMode;
    FShapeMode: TThShapeMode;

    FDrawItems: TObjectList<TThRectangleItem>;

    FDownPos, FCurrPos: TFloatPoint;
    procedure CreateDrawItem;

    procedure PaintDrawItem(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure PaintDrawItems(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
  protected
    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure SetCanvasMode(AMode: TThCanvasMode);
    property ShapeMode: TThShapeMode read FShapeMode write FShapeMode;

    procedure Clear;
  end;

  TBrushDrawLayer = class(TThCustomDrawLayer)
  private

    FThickness: Integer;
    FPenColor: TColor32;
    FPenAlpha: Byte;

    FCanvasMode: TThCanvasMode;
    FDrawMode: TThFreeDrawMode;

    FDrawObject: TThDrawObject;

    // Draw Points objects
    FMouseDowned: Boolean;
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;

    procedure SetThickness(const Value: Integer);

    procedure AddPenPoint(const X, Y: Integer);
    procedure AddEraserPoint(const X, Y: Integer);
    procedure CreateDrawItem;
    procedure DeleteDrawItems;

    procedure PaintDrawPoint(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
  protected
    function  DoHitTest(X, Y: Integer): Boolean; override;

    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure SetCanvasMode(AMode: TThCanvasMode);

    property Thickness: Integer read FThickness write SetThickness;
    property PenColor: TColor32 read FPenColor write FPenColor;
    property PenAlpha: Byte read FPenAlpha write FPenAlpha;

    property DrawMode: TThFreeDrawMode read FDrawMode write FDrawMode;

  end;

implementation

{ TFreeDrawLayer }

uses
  DebugForm,
  ThGraphicsUtils;


{ TThCustomViewLayer }

constructor TThCustomViewLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FDrawItems := TObjectList<TThDrawItem>.Create(True);
end;

destructor TThCustomViewLayer.Destroy;
begin
  FDrawItems.Clear;
  FDrawItems.Free;

  inherited;
end;

procedure TThCustomViewLayer.Clear;
begin
  FDrawItems.Clear;
  Update;
end;

function TThCustomViewLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  if not FHitTest then
    Exit(False);

  Result := inherited;
end;

function TThCustomViewLayer.GetOffset: TFloatPoint;
begin
  LayerCollection.GetViewportShift(Result.X, Result.Y);
end;

function TThCustomViewLayer.GetScale: TFloatPoint;
begin
  LayerCollection.GetViewportScale(Result.X, Result.Y);
end;

procedure TThCustomViewLayer.Paint(Buffer: TBitmap32);
var
  Item: TThDrawItem;
begin
  inherited;

  Buffer.BeginUpdate;
  for Item in FDrawItems do
    Item.Draw(Buffer, Scale, Offset);
  Buffer.EndUpdate;
end;

{ TThCustomDrawLayer }

procedure TThCustomDrawLayer.SetDrawStyle(const Value: IThDrawStyle);
begin
  FDrawStyle := Value;
end;

{ TBrushDrawLayer }

constructor TBrushDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FPath := TList<TFloatPoint>.Create;
//  FDrawItems := TObjectList<TThFreeDrawItem>.Create(True);

  FDrawMode := fdmPen;
  FThickness := 10;
  FPenColor := clBlue32;
  FPenAlpha := 255;

  FMouseDowned := False;

  MouseEvents := True;
  Scaled := True;
end;

destructor TBrushDrawLayer.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TBrushDrawLayer.AddEraserPoint(const X, Y: Integer);
var
  P: TFloatPoint;
  Item: TThDrawItem;
  Poly: TThPoly;
  PolyRect, DestRect: TFloatRect;
  EraserPath: TPath;
  ItemPaths, DestPaths: TPaths;
begin
  P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  Poly := Circle(P, FThickness / 2);
  PolyRect := PolygonBounds(Poly);
  for Item in FDrawItems do
  begin
    IntersectRect(DestRect, PolyRect, Item.Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    EraserPath := AAFloatPoint2AAPoint(Poly);
    ItemPaths := AAFloatPoint2AAPoint(Item.PolyPoly);
    with TClipper.Create do
    begin
      StrictlySimple := True;
      AddPaths(ItemPaths, ptSubject, True);
      AddPath(EraserPath, ptClip, True);

      Execute(ctIntersection, DestPaths, pftNonZero);
    end;

//    if Length(DestPaths) > 0 then
//      Item.IsToDelete := True;
  end;
end;

procedure TBrushDrawLayer.AddPenPoint(const X, Y: Integer);
var
  CurrP, LastP: TFloatPoint; // Adjusted point
  Poly: TThPoly;
  PolyPath: TPath;
begin
  // Viewport to Local
  CurrP := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  DebugMousePos('FreeDraw', PointF(X, Y));

  // Add to Path
  FPath.Add(CurrP);

  { TODO : Paint 시 FPath로 동적 처리 검토 }
  if FPath.Count = 1 then
  begin
    Poly := Circle(CurrP, FThickness / 2);
    FPolyPoly := PolyPolygon(Poly);
    FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
  end
  else
  begin
    LastP := FPath.Items[FPath.Count-2];
    Poly := BuildPolyline([LastP, CurrP], Thickness, jsRound, esRound);
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

procedure TBrushDrawLayer.CreateDrawItem;
var
  Item: TThPenDrawItem;
begin
  Item := TThPenDrawItem.Create(FDrawStyle, FPath.ToArray, FPolyPoly);

  FDrawItems.Add(Item);

  FPath.Clear;
  FPolyPoly := nil;
  FPolyPolyPath := nil;
end;

procedure TBrushDrawLayer.DeleteDrawItems;
var
  I: Integer;
  Item: TThPenDrawItem;
begin
  for I := FDrawItems.Count - 1 downto 0 do
  begin
//    Item := FDrawItems[I];
//    if Item.IsToDelete then
//      FDrawItems.Delete(I);
  end;
end;

function TBrushDrawLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := (FCanvasMode = cmFreeDraw);
end;

procedure TBrushDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmFreeDraw then
    Exit;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    FPath.Clear;
    if FDrawMode = fdmPen then
    begin
      Cursor := crCross;
      AddPenPoint(X, Y);
    end
    else if FDrawMode = fdmEraser then
    begin
      AddEraserPoint(X, Y);
    end;

    Update;
  end;
end;

procedure TBrushDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmFreeDraw then
    Exit;

  if FMouseDowned then
  begin
    if FDrawMode = fdmPen then
      AddPenPoint(X, Y)
    else if FDrawMode = fdmEraser then
      AddEraserPoint(X, Y);
    Update;
  end;
end;

procedure TBrushDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmFreeDraw then
    Exit;

  if FMouseDowned then
  begin
    Cursor := crDefault;
    FMouseDowned := False;
    if FDrawMode = fdmPen then
      CreateDrawItem
    else if FDrawMode = fdmEraser then
      DeleteDrawItems;
    Update;
  end;
end;

procedure TBrushDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;

  if FPath.Count > 0 then
    PaintDrawPoint(Buffer, Scale, Offset);
end;

procedure TBrushDrawLayer.PaintDrawPoint(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Color: TColor32;
  PolyPoly: TThPolyPoly;
begin
  Color := FPenColor;
  ModifyALpha(Color, FPenAlpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Buffer, PolyPoly, Color);
end;

procedure TBrushDrawLayer.SetCanvasMode(AMode: TThCanvasMode);
begin
  FCanvasMode := AMode;
end;

procedure TBrushDrawLayer.SetThickness(const Value: Integer);
begin
  FThickness := Value;
  Update;
end;

{ TThShapeDrawLayer }

constructor TShapeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  MouseEvents := True;
  Scaled := True;

  FDrawItems := TObjectList<TThRectangleItem>.Create(True);
end;

procedure TShapeDrawLayer.CreateDrawItem;
var
  Poly: TThPoly;
  Item: TThRectangleItem;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  Item := TThRectangleItem.Create(FloatRect(FDownPos, FCurrPos), Poly, 6, clGreen32, 200);
  FDrawItems.Add(Item);
end;

destructor TShapeDrawLayer.Destroy;
begin
  FDrawItems.Clear;
  FDrawItems.Free;

  inherited;
end;

procedure TShapeDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmSelection then
    Exit;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    FDownPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  end;
end;

procedure TShapeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CurrP, LastP: TFloatPoint; // Adjusted point
begin
  if FCanvasMode <> cmSelection then
    Exit;

  if FMouseDowned then
  begin
    // Viewport to Local
    CurrP := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    DebugMousePos('ShapeDraw', PointF(X, Y));

    FCurrPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    Update;
  end;

end;

procedure TShapeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmSelection then
    Exit;

  if FMouseDowned then
  begin
    FMouseDowned := False;
    CreateDrawItem;
  end;
end;

procedure TShapeDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;
//  Buffer.ClipRect := GetAdjustedLocation.Rect;

  if FMouseDowned then
    PaintDrawItem(Buffer, Scale, Offset);
end;

procedure TShapeDrawLayer.PaintDrawItem(Buffer: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  ScalePolygonInplace(Poly, AScale.X, AScale.Y);
  TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

  PolygonFS(Buffer, Poly, clBlue32);

  PolylineFS(Buffer, Poly, clGray32, True, 6);
end;

procedure TShapeDrawLayer.PaintDrawItems(Buffer: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Color: TColor32;
  Alpha: Byte;
  Item: TThRectangleItem;
  Poly: TThPoly;
begin
  Buffer.BeginUpdate;
  for Item in FDrawItems do
  begin
    Color := Item.Color;
    Alpha := Item.Alpha;
    ModifyAlpha(Color, Alpha);

    Poly := ScalePolygon(Item.Poly, AScale.X, AScale.Y);
    TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

    PolygonFS(Buffer, Poly, Item.Color);

    PolylineFS(Buffer, Poly, clGray32, True, 6);
  end;
  Buffer.EndUpdate;

end;

procedure TShapeDrawLayer.SetCanvasMode(AMode: TThCanvasMode);
begin
  FCanvasMode := AMode;
end;

procedure TShapeDrawLayer.Clear;
begin

end;

end.
