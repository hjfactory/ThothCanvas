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
  private
    FDrawStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;

    procedure Move(const X, Y: TFloat); overload; virtual;
    procedure Move(const APoint: TFloatPoint); overload; virtual; abstract;

    function CreateItem: TObject; virtual;
    procedure Clear; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  /// Brush Objects
  TThBrushDrawObject = class(TThDrawObject)
  end;

  TThPenBrushObject = class(TThBrushDrawObject)
  private
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
    function GetPenStyle: TThPenStyle;

    property PenStyle: TThPenStyle read GetPenStyle;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure Move(const APoint: TFloatPoint); override;
    function CreateItem: TObject; override;
    procedure Clear; override;
  end;

  TThEraserObject = class(TThBrushDrawObject)
  end;

  /// Shape Objects
  TThShapeDrawObject = class(TThDrawObject)
  end;

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

constructor TThDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

function TThDrawObject.CreateItem: TObject;
begin
  Result := nil;
end;

{ TThPenObject }

constructor TThPenBrushObject.Create(AStyle: IThDrawStyle);
begin
  inherited Create(AStyle);

  FPath := TList<TFloatPoint>.Create;
end;

destructor TThPenBrushObject.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenBrushObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Color: TColor32;
  PolyPoly: TThPolyPoly;
begin
  Color := PenStyle.Color;
  ModifyALpha(Color, PenStyle.Alpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, Color);
end;

procedure TThPenBrushObject.Move(const APoint: TFloatPoint);
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

procedure TThPenBrushObject.Clear;
begin
  inherited;

  FPath.Clear;
  FPolyPolyPath := nil;
  FPolyPoly := nil;
end;

function TThPenBrushObject.CreateItem: TObject;
begin
  Result := TThPenDrawItem.Create(FPath.ToArray, FPolyPoly,
    PenStyle.Thickness, PenStyle.Color, PenStyle.Alpha);
end;

function TThPenBrushObject.GetPenStyle: TThPenStyle;
begin
  Result := TThPenStyle(FDrawStyle);
end;

end.
