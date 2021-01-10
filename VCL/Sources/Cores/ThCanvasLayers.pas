unit ThCanvasLayers;

interface

uses
  System.Classes, System.Math, System.Generics.Collections,
  Vcl.Controls,

  GR32,
  GR32_Layers,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes,
  ThItem;

type
  TThFreeDrawItem = class(TThItem)
  private
    FPath: TArray<TFloatPoint>;
    FPolyPoly: TThPolyPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
    FBounds: TFloatRect;
    FIsToDelete: Boolean;
  public
    property Path: TThPath read FPath;
    property Bounds: TFloatRect read FBounds;
    property Alpha: Byte read FAlpha write FAlpha;

    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
    destructor Destroy; override;
  end;

  TThCanvasLayer = class(TPositionedLayer)
  end;

  TFreeDrawLayer = class(TThCanvasLayer)
  private
    FThickness: Integer;
    FPenColor: TColor32;
    FPenAlpha: Byte;

    FCanvasMode: TThCanvasMode;
    FDrawMode: TThFreeDrawMode;

    // Draw Points objects
    FMouseDowned: Boolean;
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;

    // Draw Paths objects
    FDrawItems: TObjectList<TThFreeDrawItem>;

    procedure SetThickness(const Value: Integer);

    procedure AddPenPoint(const X, Y: Integer);
    procedure AddEraserPoint(const X, Y: Integer);
    procedure CreateDrawItem;
    procedure DeleteDrawItems;

    procedure PaintDrawPoint(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
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

    property Thickness: Integer read FThickness write SetThickness;
    property PenColor: TColor32 read FPenColor write FPenColor;
    property PenAlpha: Byte read FPenAlpha write FPenAlpha;
    property DrawMode: TThFreeDrawMode read FDrawMode write FDrawMode;

    procedure Clear;
  end;

implementation

{ TFreeDrawLayer }

uses
  DebugForm,
  ThGrahicsUtils;

{ TFreeDrawLayer }

procedure TFreeDrawLayer.AddEraserPoint(const X, Y: Integer);
var
  P: TFloatPoint;
  Item: TThFreeDrawItem;
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
    ItemPaths := AAFloatPoint2AAPoint(Item.FPolyPoly);
    with TClipper.Create do
    begin
      StrictlySimple := True;
      AddPaths(ItemPaths, ptSubject, True);
      AddPath(EraserPath, ptClip, True);

      Execute(ctIntersection, DestPaths, pftNonZero);
    end;

    if Length(DestPaths) > 0 then
      Item.FIsToDelete := True;
  end;
end;

procedure TFreeDrawLayer.AddPenPoint(const X, Y: Integer);
var
  CurrP, LastP: TFloatPoint; // Adjusted point
  Poly: TThPoly;
  PolyPath: TPath;
begin
  // Viewport to Local
  CurrP := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  DebugMousePos(CurrP.X, CurrP.Y);

  // Add to Path
  FPath.Add(CurrP);

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

procedure TFreeDrawLayer.Clear;
begin
  FDrawItems.Clear;
  Update;
end;

constructor TFreeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FPath := TList<TFloatPoint>.Create;
  FDrawItems := TObjectList<TThFreeDrawItem>.Create(True);

  FDrawMode := fdmPen;

  FThickness := 10;
  FPenColor := clBlue32;
  FPenAlpha := 255;

  FMouseDowned := False;

  MouseEvents := True;
  Scaled := True;
end;

procedure TFreeDrawLayer.CreateDrawItem;
var
  Item: TThFreeDrawItem;
begin
  Item := TThFreeDrawItem.Create(FPath.ToArray, FPolyPoly, FThickness, FPenColor, FPenAlpha);

  FDrawItems.Add(Item);

  FPath.Clear;
  FPolyPoly := nil;
  FPolyPolyPath := nil;
end;

procedure TFreeDrawLayer.DeleteDrawItems;
var
  I: Integer;
  Item: TThFreeDrawItem;
begin
  for I := FDrawItems.Count - 1 downto 0 do
  begin
    Item := FDrawItems[I];
    if Item.FIsToDelete then
      FDrawItems.Delete(I);
  end;
end;

destructor TFreeDrawLayer.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TFreeDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
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

procedure TFreeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TFreeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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

procedure TFreeDrawLayer.Paint(Buffer: TBitmap32);
var
  Scale, Offset: TFloatPoint;
  ScaleX, ScaleY: TFloat;
  OffsetX, OffsetY: TFloat;
begin
  Buffer.ClipRect := GetAdjustedLocation.Rect;

  LayerCollection.GetViewportScale(Scale.X, Scale.Y);
  LayerCollection.GetViewportShift(Offset.X, Offset.Y);

  if FDrawItems.Count > 0 then
    PaintDrawItems(Buffer, Scale, Offset);
  if FPath.Count > 0 then
    PaintDrawPoint(Buffer, Scale, Offset);
end;

procedure TFreeDrawLayer.PaintDrawItems(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Color: TColor32;
  Alpha: Byte;
  Item: TThFreeDrawItem;
  PolyPoly: TThPolyPoly;
begin
  Buffer.BeginUpdate;
  for Item in FDrawItems do
  begin
    Color := Item.FColor;
    Alpha := Item.FAlpha;
    if Item.FIsToDelete then
      Alpha := Round(ALpha * 0.2);
    
    ModifyAlpha(Color, Alpha);

    PolyPoly := ScalePolyPolygon(Item.FPolyPoly, AScale.X, AScale.Y);
    TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

    PolyPolygonFS(Buffer, PolyPoly, Color);
  end;
  Buffer.EndUpdate;
end;

procedure TFreeDrawLayer.PaintDrawPoint(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
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

procedure TFreeDrawLayer.SetCanvasMode(AMode: TThCanvasMode);
begin
  FCanvasMode := AMode;
end;

procedure TFreeDrawLayer.SetThickness(const Value: Integer);
begin
  FThickness := Value;
  Update;
end;

{ TThFreeDrawItem }

constructor TThFreeDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;

  FBounds := PolypolygonBounds(FPolyPoly);
end;

destructor TThFreeDrawItem.Destroy;
begin

  inherited;
end;

end.
