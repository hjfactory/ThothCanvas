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
  protected
    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  end;

implementation

{ TFreeDrawLayer }

uses ThGrahicsUtils;

{ TFreeDrawLayer }

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
    FPath.Add(X, Y);
    Update;
  end;
end;

procedure TFreeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDowned then
  begin
    FPath.Add(X, Y);
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
  Poly: TArrayOfFloatPoint;
  PolyPath: TPath;
  PolyPolyPaths: TPaths;
begin
  if FPath.Count = 0 then
    Exit;
  if FPath.Count = 1 then
  begin
    Poly := Circle(FPath[0], 12 / 2);
    PolygonFS(Buffer, Poly, FPenColor, pfWinding);
  end
  else
  begin
    with TClipper.Create do
    try
//      StrictlySimple := True;

      LastPoint := FPath[0];
      for I := 1 to FPath.Count - 1 do
      begin
        Point := FPath[I];
        Poly := BuildPolyline([LastPoint, Point], FThickness, jsRound, esRound);
        PolyPath := AAFloatPoint2AAPoint(Poly, 1); // TFloatPoint to TIntPoint
        LastPoint := Point;
        if I = 1 then
          AddPath(PolyPath, ptSubject, True)
        else
          AddPath(PolyPath, ptClip, True);
      end;
      Execute(ctUnion, PolyPolyPaths, pftNonZero);

      PolyPolygonFS(Buffer, AAPoint2AAFloatPoint(PolyPolyPaths, 1), FPenColor);
    finally
      Free;
    end;
  end;
end;

end.
