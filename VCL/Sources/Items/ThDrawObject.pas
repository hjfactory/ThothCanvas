unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes
  ;

type
  TThDrawObject = class
  public
    procedure Move(const X, Y: TFloat); overload; virtual;
    procedure Move(const APoint: TFloatPoint); overload; virtual; abstract;

    procedure Done; virtual; abstract;
  end;

  /// Brush Objects
  TThBrushObject = class(TThDrawObject)
  end;

  TThPenObject = class(TThBrushObject)
  private
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Move(const APoint: TFloatPoint); override;
  end;

  TThEraserObject = class(TThBrushObject)
  end;

  /// Shape Objects
  TThShapeObject = class(TThDrawObject)
  end;

//  TTHDraw

implementation

uses
  ThGraphicsUtils;

{ TThDrawObject }

procedure TThDrawObject.Move(const X, Y: TFloat);
begin
  Move(FloatPoint(X, Y));
end;

{ TThPenObject }

constructor TThPenObject.Create;
begin
  FPath := TList<TFloatPoint>.Create;

end;

destructor TThPenObject.Destroy;
begin

  inherited;
end;

procedure TThPenObject.Move(const APoint: TFloatPoint);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
  Thickness: Integer;
begin
  Thickness := 12;

  FPath.Add(APoint);

  if FPath.Count = 1 then
  begin
    Poly := Circle(APoint, Thickness / 2);
    FPolyPoly := PolyPolygon(Poly);
    FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
  end
  else
  begin
    LastP := FPath.Items[FPath.Count-2];
    Poly := BuildPolyline([LastP, APoint], Thickness, jsRound, esRound);
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

end.
