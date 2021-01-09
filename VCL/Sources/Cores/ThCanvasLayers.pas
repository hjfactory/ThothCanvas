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
  public
    property Path: TThPath read FPath;

    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
  end;

  TFreeDrawLayer = class(TPositionedLayer)
  private
    FThickness: Integer;
    FPenColor: TColor32;
    FPenAlpha: Byte;

    // Draw Points
    FMouseDowned: Boolean;
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;

    // Draw Paths
    FDrawItems: TObjectList<TThFreeDrawItem>;

    procedure SetThickness(const Value: Integer);

    procedure AddPoint(const X, Y: Integer);
    procedure CreateDrawItem;

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

    property Thickness: Integer read FThickness write SetThickness;
    property PenColor: TColor32 read FPenColor write FPenColor;
    property PenAlpha: Byte read FPenAlpha write FPenAlpha;
  end;

implementation

{ TFreeDrawLayer }

uses
  DebugForm,
  ThGrahicsUtils;

{ TFreeDrawLayer }

procedure TFreeDrawLayer.AddPoint(const X, Y: Integer);
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
    FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly);
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

constructor TFreeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FPath := TList<TFloatPoint>.Create;
  FDrawItems := TObjectList<TThFreeDrawItem>.Create;

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

destructor TFreeDrawLayer.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TFreeDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    FPath.Clear;
    AddPoint(X, Y);
    Update;
  end;
end;

procedure TFreeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDowned then
  begin
    AddPoint(X, Y);
    Update;
  end;
end;

procedure TFreeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FMouseDowned then
  begin
    FMouseDowned := False;
    CreateDrawItem;
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
  Item: TThFreeDrawItem;
  PolyPoly: TThPolyPoly;
begin
  Buffer.BeginUpdate;
  for Item in FDrawItems do
  begin
    Color := Item.FColor;
    ModifyAlpha(Color, Item.FAlpha);

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
end;

end.
