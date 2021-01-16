unit ThDrawItem;

interface

uses
  GR32, GR32_Polygons, GR32_VectorUtils,
  ThTypes;


type
  TThDrawItem = class(TThItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
  protected
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;
    property Bounds: TFloatRect read FBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThBrushDrawItem = class(TThDrawItem)
  end;

  TThPenDrawItem = class(TThBrushDrawItem)
  private
    FPath: TArray<TFloatPoint>;

    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;

    FIsToDelete: Boolean;
  public
    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly;
      AThickness: Integer; AColor: TColor32; AAlpha: Byte);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Path: TThPath read FPath;
    property IsToDelete: Boolean read FIsToDelete write FIsToDelete;
  end;

  TThShapeDrawItem = class(TThDrawItem)
  end;

  TThRectangleItem = class(TThShapeDrawItem)
  private
    FRect: TFloatRect;
    FPoly: TThPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
  public
    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single;
      AColor: TColor32; AAlpha: Byte);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Poly: TThPoly read FPoly;
    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;
  end;

implementation

{ TThFreeDrawItem }

constructor TThPenDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly;
  AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;

  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;

  FBounds := PolypolygonBounds(FPolyPoly);
end;

procedure TThPenDrawItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  LColor: TColor32;
  LAlpha: Byte;
  PolyPoly: TThPolyPoly;
begin
  LColor := FColor;
  LAlpha := FAlpha;
  if FIsToDelete then
    LAlpha := Round(LAlpha * 0.2);

  ModifyAlpha(LColor, LAlpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, LColor);
end;

{ TThRectangleItem }

constructor TThRectangleItem.Create(ARect: TFloatRect; APoly: TThPoly;
  AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FRect := ARect;
  FPoly := APoly;
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;


procedure TThRectangleItem.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  inherited;

end;

end.
