unit ThShapeObject;

interface

uses
  System.Classes,
  GR32, GR32_Polygons, GR32_VectorUtils,

  ThTypes, ThClasses, ThAttributes,
  ThDrawStyle, ThDrawItem, ThDrawObject;

type

  [DrawObjAttr(210, 'Rect', TThShapeStyle, TThRectDrawItem)]
  TThRectDrawObject = class(TThShapeDrawObject)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
    procedure DrawItem(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint; AItem: IThDrawItem); override;

    function CreateItem: TObject; override;
  public
  end;

  [DrawObjAttr(220, 'RoundRect', TThShapeStyle, TThRoundRectDrawItem)]
  TThRoundRectDrawObject = class(TThShapeDrawObject)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
    procedure DrawItem(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint; AItem: IThDrawItem); override;

    function CreateItem: TObject; override;
  public
  end;


implementation

uses
  ThUtils, ThDrawObjectManager;

procedure RegistDrawObjectManager;
begin
  // Shape
  DOMgr.RegistDrawObj(TThRectDrawObject);
  DOMgr.RegistDrawObj(TThRoundRectDrawObject);
end;

{ TThRectDrawObject }

function TThRectDrawObject.CreateItem: TObject;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPt, FCurrPt));
  FLastObject := TThRectDrawItem.Create(FloatRect(FDownPt, FCurrPt), Poly,
      DrawStyle.Color, DrawStyle.BorderWidth, DrawStyle.BorderColor, 255);
  Result := FLastObject;
end;

procedure TThRectDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  if FCurrPt = EmptyPoint then
    FCurrPt := FDownPt + FloatPoint(160, 120);

  Poly := Rectangle(FloatRect(FDownPt, FCurrPt));
  ScalePolygonInplace(Poly, AScale.X, AScale.Y);
  TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

  PolygonFS(Bitmap, Poly, DrawStyle.Color);

  PolylineFS(Bitmap, Poly, DrawStyle.BorderColor, True, DrawStyle.BorderWidth);
end;

procedure TThRectDrawObject.DrawItem(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint; AItem: IThDrawItem);
var
  Item: TThRectDrawItem;
  PolyPoly: TThPolyPoly;
begin
  Item := TThRectDrawItem(AItem);
  PolyPoly := ScalePolyPolygon(Item.PolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  if Item.IsSelection then
    PolyPolygonFS(Bitmap, PolyPoly, clGray32)
  else
    PolyPolygonFS(Bitmap, PolyPoly, Item.Color);

  PolyPolylineFS(Bitmap, PolyPoly, Item.BorderColor, True, Item.BorderWidth);
end;

{ TThRoundRectDrawObject }

function TThRoundRectDrawObject.CreateItem: TObject;
var
  R: TFloatRect;
  Poly: TThPoly;
begin
  R := FloatRect(FDownPt, FCurrPt);
  Poly := RoundRect(R, Abs(R.Bottom - R.Top) / 2);
  FLastObject := TThRoundRectDrawItem.Create(R, Poly,
      DrawStyle.Color, DrawStyle.BorderWidth, DrawStyle.BorderColor, 255);
  Result := FLastObject;
end;

procedure TThRoundRectDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  R: TFloatRect;
  Poly: TThPoly;
begin
  if FCurrPt = EmptyPoint then
    FCurrPt := FDownPt + FloatPoint(160, 120);

  R := FloatRect(FDownPt, FCurrPt);

  Poly := RoundRect(R, Abs(R.Bottom - R.Top) / 2);
  ScalePolygonInplace(Poly, AScale.X, AScale.Y);
  TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

  PolygonFS(Bitmap, Poly, DrawStyle.Color);

  PolylineFS(Bitmap, Poly, DrawStyle.BorderColor, True, DrawStyle.BorderWidth);
end;

procedure TThRoundRectDrawObject.DrawItem(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint; AItem: IThDrawItem);
var
  Item: TThRectDrawItem;
  PolyPoly: TThPolyPoly;
begin
  Item := TThRectDrawItem(AItem);
  PolyPoly := ScalePolyPolygon(Item.PolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  if Item.IsSelection then
    PolyPolygonFS(Bitmap, PolyPoly, clGray32)
  else
    PolyPolygonFS(Bitmap, PolyPoly, Item.Color);

  PolyPolylineFS(Bitmap, PolyPoly, Item.BorderColor, True, Item.BorderWidth);
end;

initialization
  RegistDrawObjectManager;
finalization

end.
