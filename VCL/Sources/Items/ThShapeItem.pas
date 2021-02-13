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
  System.Math;

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
  Radius: Single;
  Poly: TThPoly;
begin
  if ARect.Width <= ARect.Height then
    ARect.Width := ARect.Height+1;
  Poly := RoundRect(ARect, Max(Abs(ARect.Bottom - ARect.Top), 1) / 2);
  Result := PolyPolygon(Poly);
end;

{ TThLineItem }

function TThLineItem.PointToPolyPoly(AFromPoint,
  AToPoint: TFloatPoint): TThPolyPoly;
var
  Poly: TThPoly;
begin
  Poly := BuildPolyline([AFromPoint, AToPoint], BorderWidth, jsRound, esRound);;

  Result := PolyPolygon(Poly);
end;

initialization
  RegisterDrawItems;
finalization

end.
