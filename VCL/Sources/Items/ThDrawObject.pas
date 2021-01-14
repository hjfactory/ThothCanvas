unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes,
  ThDrawStyle,
  ThDrawItem;

type
  TThDrawObject = class(TInterfacedObject, IThDrawObject)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;

    procedure Move(const X, Y: TFloat); overload; virtual;
    procedure Move(const APoint: TFloatPoint); overload; virtual; abstract;

    function CreateItem: TThItem; virtual;
    procedure Clear; virtual;
  end;

  /// Brush Objects
  TThBrushObject = class(TThDrawObject)
  private
    FBrushStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;
    property BrushStyle: IThDrawStyle read FBrushStyle write FBrushStyle;
  end;

  TThPenObject = class(TThBrushObject)
  private
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure Move(const APoint: TFloatPoint); override;
    function CreateItem: TThItem; override;
    procedure Clear; override;
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

procedure TThDrawObject.Clear;
begin

end;

function TThDrawObject.CreateItem: TThItem;
begin
  Result := nil;
end;

{ TThBrushObject }

constructor TThBrushObject.Create(AStyle: IThDrawStyle);
begin
  FBrushStyle := AStyle;
end;

{ TThPenObject }

constructor TThPenObject.Create(AStyle: IThDrawStyle);
begin
  inherited Create(AStyle);

  FPath := TList<TFloatPoint>.Create;
end;

function TThPenObject.CreateItem: TThItem;
begin
  Result := TThPenDrawItem.Create(FBrushStyle, FPath.ToArray, FPolyPoly);
end;

destructor TThPenObject.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  PenStyle: TThPenStyle;
  Color: TColor32;
  PolyPoly: TThPolyPoly;
begin
  PenStyle := TThPenStyle(FBrushStyle);
  Color := PenStyle.Color;
  ModifyALpha(Color, PenStyle.Alpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, Color);
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

procedure TThPenObject.Clear;
begin
  inherited;

  FPath.Clear;
  FPolyPolyPath := nil;
  FPolyPoly := nil;
end;

end.
