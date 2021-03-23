{
  Role
    Receive mouse pointer information
     - Detect the item of the mouse position
     - Firing Mouse-event on item
}

unit ThCanvasEventProcessor;

interface

uses
  System.Classes,
  GR32,
  ThTypes, ThItemCollections;

type
  /// 도형 그리기의 마우스 처리 담당
  TThShapeDrawMouseProcessor = class
  private
    FMouseDowned: Boolean;

    FItemList: TThItemList;
    FTargetItem: IThSelectableItem;
    FSelectedItems: TThSelectedItems;

    function GetItemAtPoint(APoint: TFloatPoint): IThSelectableItem;
  public
    constructor Create(AItemList: TThItemList; ASelectedItems: TThSelectedItems);

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    property TargetItem: IThSelectableItem read FTargetItem;
  end;

implementation

{ TThMouseHandler }

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
  FMouseDowned := True;
end;

procedure TThShapeDrawMouseProcessor.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  Item: IThSelectableItem;
begin
  Item := GetItemAtPoint(APoint);

//  if FTargetItem <> Item then
//  begin
//    FTargetItem := Item;
//  end;

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

procedure TThShapeDrawMouseProcessor.MouseUp(const APoint: TFloatPoint;
  AShift: TShiftState);
begin
  FMouseDowned := False;
end;

end.
