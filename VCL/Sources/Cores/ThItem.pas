unit ThItem;

interface

uses
  GR32, GR32_Polygons, GR32_VectorUtils,
  ThTypes;


type
  TThItem = class
  end;

  TThDrawItem = class(TThItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;
    property Bounds: TFloatRect read FBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThFreeDrawItem = class(TThDrawItem)
  private
    FPath: TArray<TFloatPoint>;
    FPolyPoly: TThPolyPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
    FBounds: TFloatRect;
    FIsToDelete: Boolean;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Path: TThPath read FPath;
    property Color: TColor32 read FColor;
    property Bounds: TFloatRect read FBounds;
    property Alpha: Byte read FAlpha write FAlpha;
    property IsToDelete: Boolean read FIsToDelete write FIsToDelete;
    property PolyPoly: TThPolyPoly read FPolyPoly;

    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
    destructor Destroy; override;
  end;

  TThShapeDrawItem = class(TThDrawItem)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;
  end;

  TThRectangleItem = class(TThShapeDrawItem)
  private
    FRect: TFloatRect;
    FPoly: TThPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Poly: TThPoly read FPoly;
    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;

    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
  end;


implementation

{ TThDrawItem }

{ TThRectangleItem }

constructor TThRectangleItem.Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
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

procedure TThFreeDrawItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
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

  PolyPolygonFS(Bitmap, PolyPoly, Color);
end;

end.
