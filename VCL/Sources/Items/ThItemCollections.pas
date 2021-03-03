unit ThItemCollections;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes,
  ThItem, ThUtils;

type
  TThItemList = class(TList<IThItem>)
  private
    FTargetItem: IThSelectableItem;

    // APoint에 포함되는 최상위 객체 반환
    function GetItemAtPoint(APoint: TFloatPoint): IThItem;
  protected
    procedure Notify(const Value: IThItem; Action: TCollectionNotification); override;
  public
    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<IThItem>;

    procedure MouseDown(APoint: TFloatPoint);
    procedure MouseMove(APoint: TFloatPoint);
    procedure MouseUp(APoint: TFloatPoint);

    function GetConnectionItem(APoint: TFloatPoint): IThConnectableItem;
    property TargetItem: IThSelectableItem read FTargetItem;

//    procedure MouseOver(APoint: TFloatPoint);
  end;

  TThSelectedItems = class(TList<IThSelectableItem>)
  public
    // 모든 항목 APoint만큼 이동
    procedure MoveItems(APoint: TFloatPoint);

    procedure Clear;
  end;

implementation

uses
  System.SysUtils,
  GR32_Polygons, GR32_VectorUtils, GR32_Clipper;

{ TThItemList }

function TThItemList.GetItemAtPoint(APoint: TFloatPoint): IThItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := Count - 1 downto 0 do
  begin
    Item := Items[I];

    if Item.PtInItem(APoint) then
      Exit(Item);
  end;
end;

function TThItemList.GetConnectionItem(APoint: TFloatPoint): IThConnectableItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := Count - 1 downto 0 do
  begin
    Item := Items[I];

    if not Item.PtInItem(APoint) then
      Continue;

    if not Supports(Item, IThConnectableItem) then
      Continue;

    Exit(Item as IThConnectableItem);
  end;
end;

procedure TThItemList.MouseDown(APoint: TFloatPoint);
begin
  if Assigned(FTargetItem) then
    FTargetItem.MouseDown(APoint)
end;

procedure TThItemList.MouseMove(APoint: TFloatPoint);
var
  Item: IThSelectableItem;
begin
  Item := GetItemAtPoint(APoint) as IThSelectableItem;

  if FTargetItem <> Item then
  begin
    if Assigned(FTargetItem) then
      FTargetItem.MouseLeave(APoint);
    FTargetItem := Item;

    if Assigned(FTargetItem) then
      FTargetItem.MouseEnter(APoint)
  end;

  if Assigned(FTargetItem) then
    FTargetItem.MouseMove(APoint);
end;

procedure TThItemList.MouseUp(APoint: TFloatPoint);
begin
  if Assigned(FTargetItem) then
    FTargetItem.MouseUp(APoint)
end;

procedure TThItemList.Notify(const Value: IThItem; Action: TCollectionNotification);
begin
  inherited;
  if Action = cnRemoved then
    TObject(Value).Free;
end;

function TThItemList.PolyInItems(APoly: TThPoly): TArray<IThItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  DestPaths: TThPolyPoly;
begin
  PolyRect := PolygonBounds(APoly);
//
  for I := 0 to Count - 1 do
  begin
    // Rect로 1차 교차 비교
    IntersectRect(DestRect, PolyRect, Items[I].Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    // Polygon 교차 비교(Clipper로 교차 영역 생성 후 비었는지 확인)
    with TClipper.Create do
    try
      AddPaths(Items[I].PolyPoly, ptSubject);
      AddPath(APoly, ptClip);

      Execute(ctIntersection, frNonZero, DestPaths);
    finally
      Free;
    end;

    if Length(DestPaths) > 0 then
      Result := Result + [Items[I]];
  end;
end;

{ TThSelectedItems }

procedure TThSelectedItems.Clear;
var
  Item: IThSelectableItem;
begin
  for Item in Self do
    Item.Selected := False;
  inherited Clear;
end;

procedure TThSelectedItems.MoveItems(APoint: TFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MoveItem(APoint);
end;

end.
