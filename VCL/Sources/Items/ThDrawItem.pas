unit ThDrawItem;

interface

uses
  GR32, GR32_Polygons, GR32_VectorUtils,
  ThTypes, ThDrawStyle;


type
  TThItem = class
  end;

  TThDrawItem = class(TThItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
  protected
    FDrawStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle);
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;
    property Bounds: TFloatRect read FBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThBrushDrawItem = class(TThDrawItem)
  end;

  TThPenDrawItem = class(TThBrushDrawItem)
  private
    FPath: TArray<TFloatPoint>;
    FIsToDelete: Boolean;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Path: TThPath read FPath;
    property IsToDelete: Boolean read FIsToDelete write FIsToDelete;

    constructor Create(AStyle: IThDrawStyle; APath: TThPath; APolyPoly: TThPolyPoly);
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
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Poly: TThPoly read FPoly;
    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;

    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
  end;


implementation


{ TThFreeDrawItem }

constructor TThPenDrawItem.Create(AStyle: IThDrawStyle; APath: TThPath; APolyPoly: TThPolyPoly);
begin
  inherited Create(AStyle);

  FPath := APath;
  FPolyPoly := APolyPoly;

  FBounds := PolypolygonBounds(FPolyPoly);
end;

procedure TThPenDrawItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  LStyle: TThPenStyle;
  LColor: TColor32;
  LAlpha: Byte;
  PolyPoly: TThPolyPoly;
begin
  LStyle := TThPenStyle(FDrawStyle);
  LColor := LStyle.Color;
  LAlpha := LStyle.Alpha;
  if FIsToDelete then
    LAlpha := Round(LAlpha * 0.2);

  ModifyAlpha(LColor, LAlpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, LColor);
end;

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

{ TThDrawItem }

constructor TThDrawItem.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

end.
