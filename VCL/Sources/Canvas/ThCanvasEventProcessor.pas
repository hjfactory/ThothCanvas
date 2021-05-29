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


    FTargetItem: IThSelectableItem;         // Mouse가 올라간 Item
    FTargetConnection: IThConnectableItem;  // Mouse가 올라간 연결가능한 Item(dsItemResize 시 할당 됨)

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
    property ShapeId: string read FShapeId write SetShapeId;
  end;

implementation

uses
  Vcl.Forms, System.SysUtils, System.UITypes,
  ThShapeItem, ThItemStyle, ThItemHandle;

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
  Pt: TFloatPoint;
  Item: IThSelectableItem;
  ConnectionItem: IThConnectableItem;
  MovePoint: TFloatPoint;
begin
  Pt := APoint;

  Item := FItemList.GetItemAtPoint(APoint);

  if not FMouseDowned then
  begin
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
        // 연결선을 변경하는 경우
          // 연결 대상 인지
          // 대상과 연결
        if Supports(FTargetItem, IThConnectorItem) then
        begin
          ConnectionItem := FItemList.GetConnectionItem(APoint);

          if FTargetConnection <> ConnectionItem then
          begin
            if Assigned(FTargetConnection) then
            begin
//              FTargetConnection.MouseLeave(APoint); // IThSelectableItem과 IThConnectorItem 분리?
              FTargetConnection.HideConnection;
            end;

            FTargetConnection := ConnectionItem;

            if Assigned(FTargetConnection) then
            begin
//              FTargetConnection.MouseEnter(APoint);
              FTargetConnection.ShowConnection;
            end;
          end;

          if Assigned(FTargetConnection) then
          begin
            FTargetConnection.MouseMove(APoint);

            if Assigned(FTargetConnection.Connection.HotHandle) then
              Pt := TThItemHandle(FTargetConnection.Connection.HotHandle).Point;
          end;
        end;

        FTargetItem.Selection.ResizeItem(Pt);
      end;
    end;
  end;
end;

procedure TThShapeDrawMouseProcessor.MouseUp(const APoint: TFloatPoint;
  AShift: TShiftState);
var
  ConnectorItem: IThConnectorItem;
begin
  FMouseDowned := False;

  if FDragState = dsItemResize then
  begin
    if Assigned(FTargetConnection) then
    begin
      if FTargetConnection.IsConnectable
        and Supports(FTargetItem, IThConnectorItem, ConnectorItem) then
      begin
        FTargetConnection.ConnectTo(ConnectorItem);
      end;
    end;
  end;

  if Assigned(FTargetConnection) then
  begin
    FTargetConnection.HideConnection;
    FTargetConnection := nil;
  end;

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
