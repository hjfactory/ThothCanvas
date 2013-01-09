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

    function CreateItemById(const ItemId: Integer): TThItem;

    procedure SetDrawItemId(const Value: Integer);
    function GetSelectionCount: Integer;
  protected
    procedure Paint; override;
    procedure ClickCanvas; override;

    procedure ItemSelect(Item: TThItem; IsMultiSelect: Boolean);
    procedure ItemUnselect(Item: TThItem);
    procedure ItemTracking(Sender: TObject; X, Y: Single);
    procedure ItemMove(Item: TThItem; StartPos: TPointF);
    procedure ItemResize(Item: TThItem; BeforeRect: TRectF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AppendItemById(const ItemId: Integer): Boolean;

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
  Math, ThItemFactory, FMX.Dialogs;

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

// ThothController에서 처리 할 것
function TThCanvasEditor.AppendItemById(const ItemId: Integer): Boolean;
begin
  Result := False;

  FDrawItemId := ItemId;
  ClearSelection;
  FDrawItem := CreateItemById(FDrawItemId);
  FDrawItem.Position.Point := PointF(0,0).Subtract(FContents.ScaledPoint);

  if IsDrawingItem and Assigned(FDrawItem) then
  begin
    FDrawItem.Selected := True;
    if Assigned(FOnItemAdded) then
      FOnItemAdded(FDrawItem);
  end;

  FDrawItem := nil;
  FDrawItemId := -1;
end;

procedure TThCanvasEditor.BeginSelect;
begin
  FIsMultiSelecting := True;
end;

procedure TThCanvasEditor.EndSelect;
begin
  FIsMultiSelecting := False;
end;

function TThCanvasEditor.CreateItemById(const ItemId: Integer): TThItem;
begin
  Result := ItemFactory.Get(ItemId);
  if Assigned(Result) then
  begin
    Result.ParentCanvas := Self;
    Result.OnSelected := ItemSelect;
    Result.OnUnselected := ItemUnselect;
    Result.OnTracking := ItemTracking;
    Result.OnMove := ItemMove;
    Result.OnResize := ItemResize;
    // Zoom적용된 최소사이즈 적용
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
    P := FSelections[I].Position.Point.Add(PointF(X, Y));
    FSelections[I].Position.Point := P;
  end;
end;

procedure TThCanvasEditor.ItemMove(Item: TThItem; StartPos: TPointF);
var
  P: TPointF;
begin
  if Assigned(FOnItemMove) then
  begin
    P := Item.Position.Point.Subtract(StartPos);
    FOnItemMove(FSelections, P);
  end;
end;

procedure TThCanvasEditor.ItemResize(Item: TThItem; BeforeRect: TRectF);
begin
  if Assigned(FOnItemResize) then
    FOnItemResize(Item, BeforeRect);
end;

procedure TThCanvasEditor.ItemSelect(Item: TThItem; IsMultiSelect: Boolean);
var
  I: Integer;
begin
  if (not IsMultiSelect) and (not FIsMultiSelecting) then
    ClearSelection;

  // Multiselect 시 처리
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

  if Assigned(FOnItemDelete) then
    FOnItemDelete(FSelections);

  for I := FSelections.Count - 1 downto 0 do
  begin
    FSelections[I].Tag := FSelections[I].Index; // Rollback 시 Index 복구용
    FSelections[I].Parent := nil;
    FSelections[I].Visible := False;
    FSelections[I].Selected := False;
  end;
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
      FDrawItem.Position.Point := CurrP.Subtract(FContents.ScaledPoint);
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
    FromP := FMouseDownPos.Subtract(FContents.ScaledPoint);
    CurrP := PointF(X / ZoomScale, Y / ZoomScale);
    ToP   := CurrP.Subtract(FContents.ScaledPoint);

    FDrawItem.DrawItemAtMouse(FromP, ToP);
  end
  else
    inherited;
end;

procedure TThCanvasEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if IsDrawingItem and Assigned(FDrawItem) then
  begin
    FDrawItem.Selected := True;
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
var
  C, P: TControl;
begin
  if FContents.ChildrenCount < 2 then
    Exit;

  C := FContents.Children[0] as TControl;
  P := FContents.Children[1] as TControl;

  C.Parent := P;
  C.Position.Point := C.Position.Point.Subtract( P.Position.Point);

end;

end.
