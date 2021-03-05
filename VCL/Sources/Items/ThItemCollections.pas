unit ThItemCollections;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes,
  ThItem, ThUtils;

type
  TThItemList = class
  private
    FItems: TList<IThItem>;
    FTargetItem: IThSelectableItem;
    FSelectedItems: TList<IThSelectableItem>;
    FSelectedItem: IThSelectableItem;

    // APoint에 포함되는 최상위 객체 반환
    function GetItemAtPoint(APoint: TFloatPoint): IThItem;
    function GetItem(Index: Integer): IThItem;
    procedure SetItem(Index: Integer; const Value: IThItem);
    function GetCount: Integer;
  protected
//    procedure Notify(const Value: IThItem; Action: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;

    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<IThItem>;

    procedure MouseDown(APoint: TFloatPoint);
    procedure MouseMove(APoint: TFloatPoint);
    procedure MouseUp(APoint: TFloatPoint);

    function GetConnectionItem(APoint: TFloatPoint): IThConnectableItem;

    property SelectedItem: IThSelectableItem read FSelectedItem;
    property TargetItem: IThSelectableItem read FTargetItem;

    procedure Add(AItem: IThItem);
    procedure Clear;

    procedure ClearSelected;
    // 모든 항목 APoint만큼 이동
    procedure MoveSelectedItems(APoint: TFloatPoint);

    procedure RemoveSelectedItems;

    property Items[Index: Integer]: IThItem read GetItem write SetItem; default;
    procedure Delete(const Index: Integer);
    property Count: Integer read GetCount;
//    procedure MouseOver(APoint: TFloatPoint);
  end;
//
//  TThSelectedItems = class(TList<IThSelectableItem>)
//  public
//    // 모든 항목 APoint만큼 이동
//    procedure MoveItems(APoint: TFloatPoint);
//
//    procedure Clear;
//  end;

implementation

uses
  System.SysUtils,
  GR32_Polygons, GR32_VectorUtils, GR32_Clipper;

{ TThItemList }

constructor TThItemList.Create;
begin
  FSelectedItems := TList<IThSelectableItem>.Create;
  FItems := TList<IThItem>.Create;
end;

destructor TThItemList.Destroy;
begin
  FSelectedItems.Free;
  FItems.Free;

  inherited;
end;

function TThItemList.GetItem(Index: Integer): IThItem;
begin
  Result := FItems[Index];
end;

function TThItemList.GetItemAtPoint(APoint: TFloatPoint): IThItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := FItems.Count - 1 downto 0 do
  begin
    Item := Items[I];

    if Item.PtInItem(APoint) then
      Exit(Item);
  end;
end;

procedure TThItemList.Add(AItem: IThItem);
begin
  FItems.Add(AItem);
end;

procedure TThItemList.Clear;
begin
  FItems.Clear;
end;

procedure TThItemList.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TThItemList.ClearSelected;
var
  Item: IThSelectableItem;
begin
  for Item in FSelectedItems do
    Item.Selected := False;
  FSelectedItems.Clear;
end;

function TThItemList.GetConnectionItem(APoint: TFloatPoint): IThConnectableItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := FItems.Count - 1 downto 0 do
  begin
    Item := Items[I];

    if not Item.PtInItem(APoint) then
      Continue;

    if not Supports(Item, IThConnectableItem) then
      Continue;

    Exit(Item as IThConnectableItem);
  end;
end;

function TThItemList.GetCount: Integer;
begin
  Result := FItems.Count;
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

procedure TThItemList.MoveSelectedItems(APoint: TFloatPoint);
var
  I: Integer;
begin
  for I := 0 to FSelectedItems.Count - 1 do
    FSelectedItems[I].MoveItem(APoint);
end;

//procedure TThItemList.Notify(const Value: IThItem; Action: TCollectionNotification);
//begin
//  inherited;
//  if Action = cnRemoved then
//    TObject(Value).Free;
//end;

function TThItemList.PolyInItems(APoly: TThPoly): TArray<IThItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  DestPaths: TThPolyPoly;
begin
  PolyRect := PolygonBounds(APoly);
//
  for I := 0 to FItems.Count - 1 do
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

procedure TThItemList.RemoveSelectedItems;
var
  I: Integer;
  Item: IThSelectableItem;
//  Item1, Item2: TThItem;
  Item1, Item2: IInterface;
begin
  FSelectedItem := nil;
  for Item in FSelectedItems do
  begin
    // FSelectedItems.Item(IThSelectableItem)으로
    // FItemList.Item(IThItem) 삭제(Remove) 시
    // 포인터가 달라 지워지지않음
    // 객체(또는 IInterface)로 전환 후 비교 후 삭제필요(ㅠㅜㅠㅜ)
      // https://blog.excastle.com/2008/05/10/interfaces-and-reference-equality-beware/
//    FItems.Remove(Item as IThItem);
    for I := FItems.Count - 1 downto 0 do
    begin
      Item1 := FItems[I] as IInterface;
      Item2 := Item as IInterface;

      if Item1 = Item2 then
        FItems.Delete(I);
    end;
  end;
  FSelectedItems.Clear;
end;

procedure TThItemList.SetItem(Index: Integer; const Value: IThItem);
begin

end;

{ TThSelectedItems }

//procedure TThSelectedItems.Clear;
//var
//  Item: IThSelectableItem;
//begin
//  for Item in Self do
//    Item.Selected := False;
//  inherited Clear;
//end;
//
//procedure TThSelectedItems.MoveItems(APoint: TFloatPoint);
//var
//  I: Integer;
//begin
//  for I := 0 to Count - 1 do
//    Items[I].MoveItem(APoint);
//end;

end.
