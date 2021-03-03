unit ThShapeItem;

interface

uses
  System.Generics.Collections,
  GR32, GR32_Polygons, GR32_VectorUtils, clipper,

  DMX.DesignPattern,
  ThTypes, ThUtils, ThClasses,
  ThItem;

type
  TThShapeItemClass = class of TThShapeItem;

  TThShapeItemFactory = class(TClassFactory<string, TThShapeItemClass>)
  public
    class function GetShapeItem(AId: string; AStyle: IThDrawStyle): TThShapeItem;
  end;

  TThRectItem = class(TThFaceShapeItem)
  protected
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; override;
  end;

  TThRoundRectItem = class(TThFaceShapeItem)
  protected
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; override;
  end;

  TThLineItem = class(TThLineShapeItem)
  protected
    function PointToPolyPoly(AFromPoint, AToPoint: TFloatPoint): TThPolyPoly; override;
  end;

  TThElbowLineItem = class(TThLineItem)
  end;

implementation

uses
  System.Math,
  GR32_Geometry;

procedure RegisterItems;
begin
  TThShapeItemFactory.Instance.Regist('Rect', TThRectItem);
  TThShapeItemFactory.Instance.Regist('RoundRect', TThRoundRectItem);

  TThShapeItemFactory.Instance.Regist('Line', TThLineItem);
end;

{ TThShapeItemFactory }

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

{ TThRectItem }

function TThRectItem.RectToPolyPoly(ARect: TFloatRect): TThPolyPoly;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(ARect);
  Result := PolyPolygon(Poly);
end;

{ TThRoundRectItem }

function TThRoundRectItem.RectToPolyPoly(ARect: TFloatRect): TThPolyPoly;
var
  Poly: TThPoly;
begin
  if ARect.Width <= ARect.Height * 2 then
    ARect.Width := ARect.Height * 2;
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

  TipPoint := OffsetPoint(APt1, UnitVec.X * ASize, UnitVec.Y * ASize);
  UnitNorm := GetUnitNormal(TipPoint, APt1);

  Result[0] := APt1;
  Result[1] := OffsetPoint(TipPoint, UnitNorm.X * ASize/2, UnitNorm.Y * ASize/2);
  Result[2] := OffsetPoint(TipPoint, -UnitNorm.X * ASize/2, -UnitNorm.Y * ASize/2);
end;

function TThLineItem.PointToPolyPoly(AFromPoint,
  AToPoint: TFloatPoint): TThPolyPoly;
var
  Poly, ArrowPts: TThPoly;
  PolyPath: TPath;
  PolyPolyPath: TPaths;
begin
  ArrowPts := GetArrowPoints(ToPoint, FromPoint, 20);

  with TClipper.Create do
  try
    Poly := BuildPolyline([AFromPoint, AToPoint], BorderWidth, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly, 3);
    AddPath(PolyPath, ptSubject, True);

    Poly := BuildPolyline([ArrowPts[0], ArrowPts[1]], BorderWidth, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly, 3);
    AddPath(PolyPath, ptClip, True);

    Poly := BuildPolyline([ArrowPts[0], ArrowPts[2]], BorderWidth, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly, 3);
    AddPath(PolyPath, ptClip, True);

    Execute(ctUnion, PolyPolyPath, pftNonZero);
  finally
    Free;
  end;
  Result := AAPoint2AAFloatPoint(PolyPolyPath, 3);
end;

initialization
  RegisterItems;
finalization

end.
