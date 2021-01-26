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

    function GetDrawItem: IThDrawItem; override;
  public
  end;

  [DrawObjAttr(220, 'RoundRect', TThShapeStyle, TThRoundRectDrawItem)]
  TThRoundRectDrawObject = class(TThShapeDrawObject)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;
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

function TThRectDrawObject.GetDrawItem: IThDrawItem;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPt, FCurrPt));
  FLastObject := TThRectDrawItem.Create(FloatRect(FDownPt, FCurrPt), Poly,
      DrawStyle.Color, DrawStyle.BorderWidth, DrawStyle.BorderColor, 255);
  Result := FLastObject as IThDrawItem;
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

{ TThRoundRectDrawObject }

function TThRoundRectDrawObject.GetDrawItem: IThDrawItem;
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

initialization
  RegistDrawObjectManager;
finalization

end.
