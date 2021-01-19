{
  Role
    Store drawing datas.
}

unit ThDrawItem;

interface

uses
  System.Generics.Collections,
  GR32, GR32_Polygons, GR32_VectorUtils, clipper,
  ThTypes, ThUtils, ThClasses;


type
  TThDrawItem = class;
  TThShapeDrawItem = class;
  TThDrawItemClass = class of TThDrawItem;

  TThDrawItems = class(TObjectList<TThDrawItem>)
  public
    // APoint에 포함되는 최상위 객체 반환
    function PtInItem(APoint: TFloatPoint): TThDrawItem;
    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;
  end;

  TThShapeDrawItems = class(TList<TThShapeDrawItem>)
  public
    // 모든 항목 APoint만큼 이동
    procedure Move(APoint: TFloatPoint);
  end;

  TThDrawItem = class(TThInterfacedObject, IThDrawItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
    function GetBounds: TFloatRect;
  protected
  public
    property Bounds: TFloatRect read GetBounds;
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

    FIsDeletion: Boolean;
    function GetColor: TColor32;
  public
    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly;
      AThickness: Integer; AColor: TColor32; AAlpha: Byte);
    destructor Destroy; override;

    property Path: TThPath read FPath;
    property Color: TColor32 read GetColor;
    property IsDeletion: Boolean read FIsDeletion write FIsDeletion default False;
  end;

  TThShapeDrawItem = class(TThDrawItem)
  private
    FIsSelection: Boolean;
  public
    property IsSelection: Boolean read FIsSelection write FIsSelection;
  end;

  TThRectDrawItem = class(TThShapeDrawItem)
  private
    FRect: TFloatRect;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
  public
    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single;
      AColor: TColor32; AAlpha: Byte);
    destructor Destroy; override;

    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;
  end;

implementation

{ TThDrawItems }

function TThDrawItems.PtInItem(APoint: TFloatPoint): TThDrawItem;
var
  I: Integer;
  Item: TThDrawItem;
begin
  Result := nil;

  for I := Count - 1 downto 0 do
  begin
    Item := Items[I];
    if not PtInRect(Item.Bounds, APoint) then
      Continue;

    if not PtInPolyPolygon(APoint, Item.PolyPoly) then
      Continue;

    Exit(Item);
  end;
end;

function TThDrawItems.PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  PolyPath: TPath;
  ItemPaths, DestPaths: TPaths;
begin
  PolyRect := PolygonBounds(APoly);

  for I := 0 to Count - 1 do
  begin
    // Rect로 1차 교차 비교
    IntersectRect(DestRect, PolyRect, Items[I].Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    // Polygon 교차 비교(Clipper로 교차 영역 생성 후 비었는지 확인)
    PolyPath := AAFloatPoint2AAPoint(APoly);
    ItemPaths := AAFloatPoint2AAPoint(Items[I].PolyPoly);
    with TClipper.Create do
    begin
      StrictlySimple := True;
      AddPaths(ItemPaths, ptSubject, True);
      AddPath(PolyPath, ptClip, True);

      Execute(ctIntersection, DestPaths, pftNonZero);
    end;

    if Length(DestPaths) > 0 then
      Result := Result + [Items[I]];
  end;
end;

{ TThShapeDrawItems }

procedure TThShapeDrawItems.Move(APoint: TFloatPoint);
var
  I: Integer;
  Item: TThShapeDrawItem;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].FBounds := EmptyRect;
    TranslatePolyPolygonInplace(Items[I].FPolyPoly, APoint.X, APoint.Y);
  end;
end;

{ TThDrawItem }

function TThDrawItem.GetBounds: TFloatRect;
begin
  if IsRectEmpty(FBounds) then
    FBounds := PolypolygonBounds(FPolyPoly);
  Result := FBounds;
end;

{ TThFreeDrawItem }

constructor TThPenDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly;
  AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;

  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

destructor TThPenDrawItem.Destroy;
begin

  inherited;
end;

function TThPenDrawItem.GetColor: TColor32;
var
  LAlpha: Byte;
begin
  Result := FColor;
  LAlpha := FAlpha;
  if FIsDeletion then
    LAlpha := Round(LAlpha * 0.2);
  ModifyAlpha(Result, LAlpha);
end;

{ TThRectangleItem }

constructor TThRectDrawItem.Create(ARect: TFloatRect; APoly: TThPoly;
  AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FRect := ARect;
  FPolyPoly := PolyPolygon(APoly);
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

destructor TThRectDrawItem.Destroy;
begin

  inherited;
end;

end.
