unit ThCanvasLayers;

interface

uses
  System.Classes, System.Math,
  Vcl.Controls,

  GR32,
  GR32_Layers,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes;

type
  TFreeDrawLayer = class(TPositionedLayer)
  private
    FPath: TThPath;
    FMouseDowned: Boolean;

    FThickness: Integer;
    FPenColor: TColor32;

    procedure SetThickness(const Value: Integer);

    procedure AddPoint(const X, Y: Integer);
  protected
    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    property Thickness: Integer read FThickness write SetThickness;
  end;

implementation

{ TFreeDrawLayer }

uses
  DebugForm,
  ThGrahicsUtils;

{ TFreeDrawLayer }

function AdjustedPoint(const P: TFloatPoint; Shift: TFloatPoint; Scale: TFloatPoint): TFloatPoint;
begin

end;

procedure TFreeDrawLayer.AddPoint(const X, Y: Integer);
var
  P: TFloatPoint; // Adjusted point
begin
  P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);

  DebugMousePos(P.X, P.Y);

  FPath.Add(P);
end;

constructor TFreeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FPath := TThPath.Create;

  FThickness := 2;
  FPenColor := clRed32;

  FMouseDowned := False;

  MouseEvents := True;
  Scaled := True;
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
  end;
end;

procedure TFreeDrawLayer.Paint(Buffer: TBitmap32);
var
  I: Integer;
  LastPoint, Point: TFloatPoint;
  OffsetX, OffsetY: Single;

  Thickness: Single;
  ScaleX, ScaleY: TFloat;

  Poly: TArrayOfFloatPoint;
  PolyPath: TPath;
  PolyPolyPaths: TPaths;
begin
  if FPath.Count = 0 then
    Exit;

  Buffer.ClipRect := GetAdjustedLocation.Rect;

  LayerCollection.GetViewportScale(ScaleX, ScaleY);
  Thickness := FThickness * ScaleX;

  if FPath.Count = 1 then
  begin
    Poly := Circle(FPath[0], Thickness / 2);
    PolygonFS(Buffer, Poly, FPenColor, pfWinding);
  end
  else
  begin
    with TClipper.Create do
    try
      LastPoint := LayerCollection.LocalToViewport(FPath[0], True);
      for I := 1 to FPath.Count - 1 do
      begin
        Point := LayerCollection.LocalToViewport(FPath[I], True);
        Poly := BuildPolyline([LastPoint, Point], Thickness, jsRound, esRound);
        PolyPath := AAFloatPoint2AAPoint(Poly, 3); // TFloatPoint to TIntPoint
        LastPoint := Point;
        if I = 1 then
          AddPath(PolyPath, ptSubject, True)
        else
          AddPath(PolyPath, ptClip, True);
      end;
      Execute(ctUnion, PolyPolyPaths, pftNonZero);

      PolyPolygonFS(Buffer, AAPoint2AAFloatPoint(PolyPolyPaths, 3), FPenColor);
    finally
      Free;
    end;
  end;
end;

procedure TFreeDrawLayer.SetThickness(const Value: Integer);
begin
  FThickness := Value;
  Update;
end;

end.
