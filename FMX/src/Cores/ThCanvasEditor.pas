unit ThCanvasEditor;

interface

uses
  System.UITypes, System.Classes, System.Types, System.SysUtils, FMX.Types,
  ThCanvas, ThTypes, ThItem, ThClasses;

type
{
  Features
    - ThContainers features
    - ThItem control(add, modify, delete)
}
  TThCanvasEditor = class(TThCanvas)
  private
    FDrawItem: TThItem;
    FDrawItemId: Integer;

    FSelections: TThItems;
    FSelectedItem: TThItem;

    FOnItemAdded: TItemEvent;
    FOnItemDelete: TItemListEvent;
    FOnItemMove: TItemListPointvent;
    FOnItemResize: TItemResizeEvent;

    FIsMultiSelecting: Boolean; // BeginSelect, EndSelect

    function CreateItemById(const ItemId: Integer; AItemData: IThItemData = nil): TThItem;

    procedure SetDrawItemId(const Value: Integer);
    function GetSelectionCount: Integer;
  protected
    procedure Paint; override;
    procedure ClickCanvas; override;

    procedure ItemSelect(Item: TThItem; IsMultiSelect: Boolean);
    procedure ItemUnselect(Item: TThItem);
    procedure ItemTracking(Sender: TObject; X, Y: Single);
    procedure ItemMove(Item: TThItem; DistancePos: TPointF);
    procedure ItemResize(Item: TThItem; BeforeRect: TRectF);
    procedure ItemResizing(Item: TThItem; SpotCorner: TSpotCorner; X, Y: Single;
      SwapHorz, SwapVert: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AppendFileItem(const ItemId: Integer; const AFileName: TFileName = ''): Boolean;

    function IsDrawingItem: Boolean; override;
    function IsMultiSelected: Boolean; override;

    property SelectedItem: TThItem read FSelectedItem;
    procedure ClearSelection;
    procedure BeginSelect;
    procedure EndSelect;

    procedure DeleteSelection;

    property DrawItemId: Integer read FDrawItemId write SetDrawItemId;
    property SelectionCount: Integer read GetSelectionCount;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property OnItemAdded: TItemEvent read FOnItemAdded write FOnItemAdded;
    property OnItemDelete: TItemListEvent read FOnItemDelete write FOnItemDelete;
    property OnItemMove: TItemListPointvent read FOnItemMove write FOnItemMove;
    property OnItemResize: TItemResizeEvent read FOnItemResize write FOnItemResize;

    procedure Test;
  end;

implementation

uses
  Math, SpotCornerUtils, ThItemFactory, DebugUtils;

{ TThCanvasEditor }

constructor TThCanvasEditor.Create(AOwner: TComponent);
begin
  inherited;

  CanFocus := True; // Keyboard event

  FDrawItemId := -1;
  FIsMultiSelecting := False;

  FSelections := TThItems.Create;
end;

destructor TThCanvasEditor.Destroy;
begin
  FSelections.Free;

  inherited;
end;

// ThothController���� ó�� �� ��
function TThCanvasEditor.AppendFileItem(const ItemId: Integer;
  const AFileName: TFileName): Boolean;
var
  P: TPointF;
  Item: TThItem;
begin
  Result := False;

  ClearSelection;
  Item := CreateItemById(ItemId, TThFileItemData.Create(AFileName));
  if Assigned(Item) then
  begin
    // Center position
    P := CenterPoint - PointF(Item.Width / 2, Item.Height / 2);
    Item.Position.Point := P - FContents.ScaledPoint;

    if Assigned(Item) then
    begin
      Item.Selected := True;
      if Assigned(FOnItemAdded) then
        FOnItemAdded(Item);
    end;
  end;
end;

procedure TThCanvasEditor.BeginSelect;
begin
  FIsMultiSelecting := True;
end;

procedure TThCanvasEditor.EndSelect;
begin
  FIsMultiSelecting := False;
end;

function TThCanvasEditor.CreateItemById(const ItemId: Integer; AItemData: IThItemData): TThItem;
begin
  Result := ItemFactory.Get(ItemId, AItemData);
  if Assigned(Result) then
  begin
    if (Result.Width = 0) and (Result.Height = 0) then
    begin
      Result.Free;
      Result := nil;
      Exit;
    end;

    Result.ParentCanvas := Self;
    Result.OnSelected := ItemSelect;
    Result.OnUnselected := ItemUnselect;
    Result.OnTracking := ItemTracking;
    Result.OnMove := ItemMove;
    Result.OnResize := ItemResize;
    Result.OnResizing := ItemResizing;
    // Zoom����� �ּһ����� ����
    Result.Width := Result.Width / ZoomScale;
    Result.Height := Result.Height / ZoomScale;
  end;
end;

procedure TThCanvasEditor.ClickCanvas;
begin
  ClearSelection;
end;

procedure TThCanvasEditor.ItemTracking(Sender: TObject; X, Y: Single);
var
  I: Integer;
  P: TPointF;
begin
  for I := 0 to FSelections.Count - 1 do
  begin
    if FSelections[I].IsParentSelected then
      Continue;

    P := FSelections[I].Position.Point + PointF(X, Y);
    FSelections[I].Position.Point := P;
  end;
end;

procedure TThCanvasEditor.ItemMove(Item: TThItem; DistancePos: TPointF);
var
  CurrItem: TThItem;
  ItemContainer: IThItemContainer;
begin
  for CurrItem in FSelections do
  begin
    // Set Parent
    CurrItem.Parent := FContents.FindParent(CurrItem);

    // Contain Children
    if Supports(CurrItem.Parent, IThItemContainer, ItemContainer) then
      ItemContainer.ContainChildren(CurrItem);
  end;

  if Assigned(FOnItemMove) then
    FOnItemMove(FSelections, DistancePos);
end;

procedure TThCanvasEditor.ItemResize(Item: TThItem; BeforeRect: TRectF);
begin
  // Set Parent
  Item.Parent := FContents.FindParent(Item);

  // Contain Children
  FContents.ContainChildren(Item);

  // Release Children
  Item.ReleaseChildren;

  if Assigned(FOnItemResize) then
    FOnItemResize(Item, BeforeRect);
end;

procedure TThCanvasEditor.ItemResizing(Item: TThItem; SpotCorner: TSpotCorner;
  X, Y: Single; SwapHorz, SwapVert: Boolean);
var
  I: Integer;
  P: TPointF;
begin
  P := PointF(0, 0);

  if SwapHorz or ContainSpotCorner(SpotCorner, scLeft) then
    P.X := P.X - X;
  if SwapVert or ContainSpotCorner(SpotCorner, scTop) then
    P.Y := P.Y - Y;

  if P <> PointF(0, 0) then
  begin
    for I := 0 to Item.ItemCount - 1 do
      Item.Items[I].Position.Point := Item.Items[I].Position.Point + P;
  end;
end;

procedure TThCanvasEditor.ItemSelect(Item: TThItem; IsMultiSelect: Boolean);
var
  I: Integer;
begin
  if (not IsMultiSelect) and (not FIsMultiSelecting) then
    ClearSelection;

  // Multiselect �� ó��
  for I := 0 to FSelections.Count - 1 do
    FSelections[I].ShowDisableSpots;

  FSelectedItem := Item;
  FSelections.Add(FSelectedItem);
end;

procedure TThCanvasEditor.ItemUnselect(Item: TThItem);
begin
  FSelections.Remove(Item);
  FSelectedItem := nil;
  if FSelections.Count > 0 then
    FSelectedItem := FSelections.Last;

  if FSelections.Count = 1 then
    FSelections[0].ShowSpots;
end;

procedure TThCanvasEditor.ClearSelection;
var
  I: Integer;
begin
  for I := FSelections.Count - 1 downto 0 do
    FSelections[I].Selected := False;
  FSelectedItem := nil;

  FSelections.Clear;
end;

procedure TThCanvasEditor.DeleteSelection;
var
  I: Integer;
begin
  if FSelections.Count = 0 then
    Exit;

  for I := FSelections.Count - 1 downto 0 do
    FSelections[I].Parent := nil;

  if Assigned(FOnItemDelete) then
    FOnItemDelete(FSelections);

  ClearSelection;
end;

function TThCanvasEditor.GetSelectionCount: Integer;
begin
  Result := FSelections.Count;
end;

function TThCanvasEditor.IsDrawingItem: Boolean;
begin
  Result := FDrawItemId <> -1;
end;

function TThCanvasEditor.IsMultiSelected: Boolean;
begin
  Result := FSelections.Count > 1;
end;

procedure TThCanvasEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  CurrP: TPointF;
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and IsDrawingItem then
  begin
    ClearSelection;
    FDrawItem := CreateItemById(FDrawItemId);
    if Assigned(FDrawItem) then
    begin
      CurrP := PointF(X / ZoomScale, Y / ZoomScale);
      FDrawItem.Position.Point := CurrP - FContents.ScaledPoint;
    end;
  end;
end;

procedure TThCanvasEditor.MouseMove(Shift: TShiftState; X, Y: Single);
var
  CurrP,
  FromP, ToP: TPointF;
begin
  if IsDrawingItem and Assigned(FDrawItem) then
  begin
    FromP := FMouseDownPos - FContents.ScaledPoint;
    CurrP := PointF(X / ZoomScale, Y / ZoomScale);
    ToP   := CurrP - FContents.ScaledPoint;

    FDrawItem.DrawItemAtMouse(FromP, ToP);
  end
  else
    inherited;
end;

procedure TThCanvasEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  ItemContainer: IThItemContainer;
begin
  inherited;

  if IsDrawingItem and Assigned(FDrawItem) then
  begin
    FDrawItem.Selected := True;

    // Set Parent
    FDrawItem.Parent := FContents.FindParent(FDrawItem);

    // Contain Children
    if Supports(FDrawItem.Parent, IThItemContainer, ItemContainer) then
      ItemContainer.ContainChildren(FDrawItem);

    if Assigned(FOnItemAdded) then
      FOnItemAdded(FDrawItem);
  end;

  FDrawItem := nil;
  FDrawItemId := -1;
end;

procedure TThCanvasEditor.Paint;
begin
  inherited;

end;

procedure TThCanvasEditor.SetDrawItemId(const Value: Integer);
begin
  FDrawItemId := Value;
end;

procedure TThCanvasEditor.Test;
begin
//  FContents.RotationAngle := FContents.RotationAngle + 45;
end;

end.
