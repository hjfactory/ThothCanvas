unit ThCanvasEventProcessor;

interface

uses
  System.Classes,
  GR32,
  ThTypes, ThItemCollections;

type
  TThShapeDrawMouseProcessor = class
  private
    FItemList: TThItemList;
    FTargetItem: IThSelectableItem;
    FSelectedItems: TThSelectedItems;

    function GetItemAtPoint(APoint: TFloatPoint): IThSelectableItem;
  public
    constructor Create(AItemList: TThItemList; ASelectedItems: TThSelectedItems);

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    function CalcDragState(const APoint: TFloatPoint): TThShapeDragState;
  end;

implementation

{ TThMouseHandler }

function TThShapeDrawMouseProcessor.CalcDragState(const APoint: TFloatPoint): TThShapeDragState;
begin
  // Item 판단
  // 선택 & dsItemMove

  // 핸들 판단
  // dsItemResize
end;

constructor TThShapeDrawMouseProcessor.Create(AItemList: TThItemList; ASelectedItems: TThSelectedItems);
begin
  FItemList := AItemList;
  FSelectedItems := ASelectedItems;
end;

function TThShapeDrawMouseProcessor.GetItemAtPoint(
  APoint: TFloatPoint): IThSelectableItem;
var
  I: Integer;
  Item: IThItem;
begin
  Result := nil;

  for I := FItemList.Count - 1 downto 0 do
  begin
    Item := FItemList[I];

    if Item.PtInItem(APoint) then
      Exit(Item as IThSelectableItem);
  end;end;

procedure TThShapeDrawMouseProcessor.MouseDown(const APoint: TFloatPoint;
  AShift: TShiftState);
begin
end;

procedure TThShapeDrawMouseProcessor.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  Item: IThSelectableItem;
begin
//  Item := GetItemAtPoint(APoint) as IThSelectableItem;
//
//  if FTargetItem <> Item then
//  begin
//    if Assigned(FTargetItem) then
//      FTargetItem.MouseLeave(APoint);
//    FTargetItem := Item;
//
//    if Assigned(FTargetItem) then
//      FTargetItem.MouseEnter(APoint)
//  end;
//
//  if Assigned(FTargetItem) then
//    FTargetItem.MouseMove(APoint);
end;

procedure TThShapeDrawMouseProcessor.MouseUp(const APoint: TFloatPoint;
  AShift: TShiftState);
begin

end;

end.
