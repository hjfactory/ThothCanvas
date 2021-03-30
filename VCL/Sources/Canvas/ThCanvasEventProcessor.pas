{
  Role
    Receive mouse pointer information
     - Detect the item of the mouse position
     - Firing Mouse-event on item

  역할을 분담하는 것이 맞는가? DrawObj에 구현하는 것이 맞나?
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
    FLastPoint: TFloatPoint;
    FMouseDowned: Boolean;

    FDragState: TThShapeDragState;

    FItemList: TThItemList;

    FTargetItem: IThSelectableItem;
    FTargetConnection: IThConnectableItem;
    FSelectedItems: TThSelectedItems;
    FShapeId: string;
    procedure SetDragState(const Value: TThShapeDragState);
    procedure SetShapeId(const Value: string);
  public
    constructor Create(AItemList: TThItemList; ASelectedItems: TThSelectedItems);

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    property DragState: TThShapeDragState read FDragState write SetDragState;
//    property TargetItem: IThSelectableItem read FTargetItem;
    property ShapeId: string read FShapeId write SetShapeId;
  end;

implementation

uses
  Vcl.Forms, System.SysUtils, System.UITypes,
  ThShapeItem, ThItemStyle;

{ TThMouseHandler }

constructor TThShapeDrawMouseProcessor.Create(AItemList: TThItemList; ASelectedItems: TThSelectedItems);
begin
  FItemList := AItemList;
  FSelectedItems := ASelectedItems;
end;

procedure TThShapeDrawMouseProcessor.MouseDown(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  PressedShift: Boolean;
begin
  FLastPoint := APoint;

  FMouseDowned := True;

  if FDragState = dsItemAdd then
  begin
    FTargetItem := TThShapeItemFactory.GetShapeItem(FShapeId, TThShapeStyle.Create);
    FTargetItem.ResizeItem(FLastPoint, FLastPoint);
    FItemList.Add(FTargetItem);

    FSelectedItems.Clear;
    FSelectedItems.Add(FTargetItem);

    DragState := dsItemResize;
    FShapeId := '';
  end
  else if FDragState = dsNone then
  begin
    {
       Click
         Canvas         : Clear Selections
         Item           : Clear selection, Select Item
         Selected Item  : None
         Item Handle    : Start drag
       Shift + Click
         Canvas         : None
         Item           : Add to selection
         Selected Item  : Remove from selection
         Item Handle    : Don't show
    }
    // Select item(Unselect, Multi-select)
    PressedShift := AShift * [ssShift, ssCtrl] <> [];

    // Canvas click
    if not Assigned(FTargetItem) then
    begin
      if not PressedShift then
        FSelectedItems.Clear;
    end
    else
    begin
      if not FTargetItem.Selected then
      // Non-selected item click
      begin
        if not PressedShift then
          FSelectedItems.Clear;
        FSelectedItems.Add(FTargetitem);
      end
      else
      // Selected item click
      begin
        if PressedShift then
        begin
          FTargetItem.Selected := False;
          FSelectedItems.Remove(FTargetItem);
        end;
      end;
    end;

    if Assigned(FTargetItem) and FTargetItem.Selected then
    begin
      if Assigned(FTargetItem.Selection.HotHandle) then
        DragState := dsItemResize
      else
        DragState := dsItemMove;
    end;
  end;
end;

procedure TThShapeDrawMouseProcessor.MouseMove(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  Item: IThSelectableItem;
  ConnectionItem: IThConnectableItem;
  MovePoint: TFloatPoint;
begin
  Item := FItemList.GetItemAtPoint(APoint);

  if not FMouseDowned then
  begin
    if FTargetItem <> Item then
    begin
      if Assigned(FTargetItem) then
      begin
        if Assigned(FTargetItem.Selection.HotHandle) then
          FTargetItem.Selection.ReleaseHotHandle;

        FTargetItem.MouseLeave(APoint);
      end;

      FTargetItem := Item;

      if Assigned(FTargetItem) then
        FTargetItem.MouseEnter(APoint)
    end;

    if Assigned(FTargetItem) then
      FTargetItem.MouseMove(APoint);
  end
  else
  begin
    case FDragState of
    dsItemMove:
      begin
        MovePoint := APoint - FLastPoint;
        FSelectedItems.MoveItems(MovePoint);
        FLastPoint := APoint;
      end;
    dsItemResize:
      begin
        FTargetItem.Selection.ResizeItem(APoint);

        if Supports(FTargetItem, IThConnectorItem) then
        begin
          ConnectionItem := FItemList.GetConnectionItem(APoint);

          if FTargetConnection <> ConnectionItem then
          begin
            if Assigned(FTargetConnection) then
            begin
              if Assigned(FTargetConnection.Connection.HotHandle) then
                FTargetConnection.Connection.ReleaseHotHandle;

              FTargetConnection.MouseLeave(APoint);
            end;

            FTargetConnection := ConnectionItem;

            if Assigned(FTargetConnection) then
              FTargetConnection.MouseEnter(APoint)
          end;

          if Assigned(FTargetConnection) then
            FTargetConnection.MouseMove(APoint);
        end;
      end;
    end;
  end;
end;

procedure TThShapeDrawMouseProcessor.MouseUp(const APoint: TFloatPoint;
  AShift: TShiftState);
begin
  FMouseDowned := False;

  DragState := dsNone;
end;

procedure TThShapeDrawMouseProcessor.SetDragState(
  const Value: TThShapeDragState);
begin
  FDragState := Value;

  case FDragState of
  dsNone:       ;
  dsItemAdd:    Screen.Cursor := crCross;
  dsItemMove:   ;
  dsItemResize: Screen.Cursor := crCross;
  end;
end;

procedure TThShapeDrawMouseProcessor.SetShapeId(const Value: string);
begin
  FShapeId := Value;

  if FShapeId = '' then
    DragState := dsNone
  else
    DragState := dsItemAdd;
end;

end.
