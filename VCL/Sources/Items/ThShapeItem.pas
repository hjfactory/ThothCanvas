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

  TThRectDrawItem = class(TThShapeItem)
  public
    function MakePoly: TThPolyPoly; override;
  end;

  TThRoundRectDrawItem = class(TThRectDrawItem)
  public
    function MakePoly: TThPolyPoly; override;
  end;

implementation

procedure RegisterDrawItems;
begin
  TThShapeItemFactory.Instance.Regist('Rect', TThRectDrawItem);
  TThShapeItemFactory.Instance.Regist('RoundRect', TThRoundRectDrawItem);
end;

{ TThDrawItemFactory }

class function TThShapeItemFactory.GetShapeItem(AId: string; AStyle: IThDrawStyle): TThShapeItem;
var
  Cls: TThShapeItemClass;
begin
  Cls := Instance.GetClass(AId);

  if not Assigned(Cls) then
    Exit(nil);

  Result := Cls.Create(AStyle);
end;

{ TThRectDrawItem }

function TThRectDrawItem.MakePoly: TThPolyPoly;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FRect);
  Result := PolyPolygon(Poly);
end;

{ TThRoundRectDrawItem }

function TThRoundRectDrawItem.MakePoly: TThPolyPoly;
var
  Poly: TThPoly;
begin
  Poly := RoundRect(FRect, Abs(FRect.Bottom - FRect.Top) / 2);
  Result := PolyPolygon(Poly);
end;

initialization
  RegisterDrawItems;
finalization

end.
