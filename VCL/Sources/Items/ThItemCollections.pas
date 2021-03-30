unit ThItemCollections;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes,
  ThItem, ThUtils;

type
  TThItemList = class(TList<IThItem>)
  protected
    procedure Notify(const Value: IThItem; Action: TCollectionNotification); override;
  public
    // APoly ������ ���ԵǴ� ��ü �迭 ��ȯ
    function GetPolyInItems(APoly: TThPoly): TArray<IThItem>;

    function GetConnectionItem(APoint: TFloatPoint): IThConnectableItem;
    function GetItemAtPoint(APoint: TFloatPoint): IThSelectableItem;
  end;

  TThSelectedItems = class(TList<IThSelectableItem>)
  public
    // ��� �׸� APoint��ŭ �̵�
    procedure MoveItems(APoint: TFloatPoint);

    procedure Add(const AItem: IThSelectableItem);
    procedure Remove(const AItem: IThSelectableItem);
    procedure Clear;
  end;

implementation

uses
  System.SysUtils,
  GR32_Polygons, GR32_VectorUtils, GR32_Clipper;

{ TThItemList }

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

procedure TThItemList.Notify(const Value: IThItem; Action: TCollectionNotification);
begin
  inherited;
  if Action = cnRemoved then
    TObject(Value).Free;
end;

function TThItemList.GetItemAtPoint(APoint: TFloatPoint): IThSelectableItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := Count - 1 downto 0 do
  begin
    Item := Items[I];

    if Item.PtInItem(APoint) then
      Exit(Item as IThSelectableItem);
  end;
end;

function TThItemList.GetPolyInItems(APoly: TThPoly): TArray<IThItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  DestPaths: TThPolyPoly;
begin
  PolyRect := PolygonBounds(APoly);
//
  for I := 0 to Count - 1 do
  begin
    // Rect�� 1�� ���� ��
    IntersectRect(DestRect, PolyRect, Items[I].Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    // Polygon ���� ��(Clipper�� ���� ���� ���� �� ������� Ȯ��)
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

//procedure TThItemList.RemoveSelectedItems;
//var
//  I: Integer;
//  Item: IThSelectableItem;
////  Item1, Item2: TThItem;
//  Item1, Item2: IInterface;
//begin
//  FSelectedItem := nil;
//  for Item in FSelectedItems do
//  begin
//    // FSelectedItems.Item(IThSelectableItem)����
//    // FItemList.Item(IThItem) ����(Remove) ��
//    // �����Ͱ� �޶� ������������
//    // ��ü(�Ǵ� IInterface)�� ��ȯ �� �� �� �����ʿ�(�Ф̤Ф�)
//      // https://blog.excastle.com/2008/05/10/interfaces-and-reference-equality-beware/
////    FItems.Remove(Item as IThItem);
//    for I := FItems.Count - 1 downto 0 do
//    begin
//      Item1 := FItems[I] as IInterface;
//      Item2 := Item as IInterface;
//
//      if Item1 = Item2 then
//        FItems.Delete(I);
//    end;
//  end;
//  FSelectedItems.Clear;
//end;

{ TThSelectedItems }

procedure TThSelectedItems.Add(const AItem: IThSelectableItem);
begin
  AItem.Selected := True;
  inherited Add(AItem);
end;

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

procedure TThSelectedItems.Remove(const AItem: IThSelectableItem);
begin
  AItem.Selected := False;
  inherited Remove(AItem);
end;

end.
