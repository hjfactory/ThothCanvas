unit ThShapeItem;

interface

uses
  System.Generics.Collections,
  GR32, GR32_Polygons, GR32_VectorUtils, clipper,

  DMX.DesignPattern,
  ThTypes, ThUtils, ThClasses,
  ThDrawItem;

type
  TThShapeItemClass = class of TThShapeItem;

  TThShapeItemFactory = class(TClassFactory<string, TThShapeItemClass>)
  public
    class function GetShapeItem(AId: string; AStyle: IThDrawStyle): TThShapeItem;
  end;

  TThRectItem = class(TThFillShapeItem)
  protected
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; override;
  end;

  TThRoundRectItem = class(TThFillShapeItem)
  protected
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; override;
  end;

  TThLineItem = class(TThLineShapeItem)
  protected
    function PointToPolyPoly(AFromPoint, AToPoint: TFloatPoint): TThPolyPoly; override;
  end;

  TThCurvedItem = class(TThLineItem)
  end;

implementation

uses
  System.Math,
  GR32_Geometry;

procedure RegisterDrawItems;
begin
  TThShapeItemFactory.Instance.Regist('Rect', TThRectItem);
  TThShapeItemFactory.Instance.Regist('RoundRect', TThRoundRectItem);

  TThShapeItemFactory.Instance.Regist('Line', TThLineItem);
end;

{ TThDrawItemFactory }

class function TThShapeItemFactory.GetShapeItem(AId: string; AStyle: IThDrawStyle): TThShapeItem;
var
  Cls: TThShapeItemClass;
begin
  Cls := Instance.GetClass(AId);

  if not Assigned(Cls) then
    Exit(nil);

  Result := Cls.Create;
  Result.SetStyle(AStyle);
end;

{ TThRectDrawItem }

function TThRectItem.RectToPolyPoly(ARect: TFloatRect): TThPolyPoly;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(ARect);
  Result := PolyPolygon(Poly);
end;

{ TThRoundRectDrawItem }

function TThRoundRectItem.RectToPolyPoly(ARect: TFloatRect): TThPolyPoly;
var
  Poly: TThPoly;
begin
  if ARect.Width <= ARect.Height then
    ARect.Width := ARect.Height+1;
  Poly := RoundRect(ARect, Max(Abs(ARect.Bottom - ARect.Top), 1) / 2);
  Result := PolyPolygon(Poly);
end;

{ TThLineItem }

function GetArrowPoints(APt1, APt2: TFloatPoint; ASize: Single): TThPoly;
var
  UnitVec, UnitNorm: TFloatPoint;
  TipPoint: TFloatPoint;
begin
  SetLength(Result, 3);
  UnitVec := GetUnitVector(APt1, APt2);

  Result[0] := APt1;
  TipPoint := OffsetPoint(APt1, UnitVec.X * ASize, UnitVec.Y * ASize);
  UnitNorm := GetUnitNormal(TipPoint, APt1);

  Result[1] := OffsetPoint(TipPoint, UnitNorm.X * ASize/2, UnitNorm.Y * ASize/2);
  Result[2] := OffsetPoint(TipPoint, -UnitNorm.X * ASize/2, -UnitNorm.Y * ASize/2);
end;

function TThLineItem.PointToPolyPoly(AFromPoint,
  AToPoint: TFloatPoint): TThPolyPoly;
var
  Poly, ArrowPoly, LinePoly: TThPoly;
begin
  ArrowPoly := GetArrowPoints(ToPoint, FromPoint, 20);
  LinePoly := BuildPolyline([AFromPoint, AToPoint], BorderWidth, jsRound, esRound);
  SetLength(Result, 3);
  Result[0] := LinePoly;
  Result[1] := BuildPolyline([ArrowPoly[0], ArrowPoly[1]], BorderWidth, jsRound, esRound);
  Result[2] := BuildPolyline([ArrowPoly[0], ArrowPoly[2]], BorderWidth, jsRound, esRound);

//  Result := CatPolyPolygon([LinePoly], [ArrowPoly]);
//  Result := PolyPolygon(LinePoly);
end;

initialization
  RegisterDrawItems;
finalization

end.
