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
    FDrawItemID: Integer;
    FSelections: TThItems;
    FSelectedItem: TThItem;

    FOnItemAdded: TItemEvent;
    FOnItemDelete: TItemListEvent;
    FOnItemMove: TItemListPointvent;

    procedure SetDrawItemID(const Value: Integer);
    function GetSelectionCount: Integer;
  protected
    procedure ClickCanvas; override;

    procedure ItemSelect(Item: TThItem; IsAdded: Boolean);
    procedure ItemUnselect(Item: TThItem);
    procedure ItemTracking(Sender: TObject; X, Y: Single);
    procedure ItemMove(Item: TThItem; StartPos: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddItem(const ItemID: Integer): TThItem;

    function IsDrawingItem: Boolean; override;
    function IsMultiSelected: Boolean; override;

    property SelectedItem: TThItem read FSelectedItem;
    procedure ClearSelection;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure DeleteSelection;

    property DrawItemID: Integer read FDrawItemID write SetDrawItemID;
    property SelectionCount: Integer read GetSelectionCount;

    property OnItemAdded: TItemEvent read FOnItemAdded write FOnItemAdded;
    property OnItemDelete: TItemListEvent read FOnItemDelete write FOnItemDelete;
    property OnItemMove: TItemListPointvent read FOnItemMove write FOnItemMove;
  end;

implementation

uses
  Math, ThItemFactory, CommonUtils;

{ TThCanvasEditor }

constructor TThCanvasEditor.Create(AOwner: TComponent);
begin
  inherited;

  CanFocus := True; // Keyboard event

  FDrawItemID := -1;

  FSelections := TThItems.Create;
end;

destructor TThCanvasEditor.Destroy;
begin
  FSelections.Free;

  inherited;
end;

function TThCanvasEditor.AddItem(const ItemID: Integer): TThItem;
begin
  Result := ItemFactory.Get(ItemID);
  if Assigned(Result) then
  begin
    Result.ParentCanvas := Self;
    Result.OnSelected := ItemSelect;
    Result.OnUnselected := ItemUnselect;
    Result.OnTracking := ItemTracking;
    Result.OnMove := ItemMove;
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
//    if FSelections[I] = TThItem(Sender) then
//      Continue;

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

procedure TThCanvasEditor.ItemSelect(Item: TThItem; IsAdded: Boolean);
begin
  if not IsAdded then
    ClearSelection;

  FSelectedItem := Item;
  FSelections.Add(FSelectedItem);
end;

procedure TThCanvasEditor.ItemUnselect(Item: TThItem);
begin
  FSelections.Remove(Item);
  FSelectedItem := nil;
  if FSelections.Count > 0 then
    FSelectedItem := FSelections.Last;
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
  if Assigned(FOnItemDelete) then
    FOnItemDelete(FSelections);

  for I := FSelections.Count - 1 downto 0 do
  begin
    FSelections[I].Parent := nil;
    FSelections[I].Visible := False;
    FSelections[I].Selected := False;
  end;

//  FSelections.Clear;
//  FSelectedItem := nil;
end;

function TThCanvasEditor.GetSelectionCount: Integer;
begin
  Result := FSelections.Count;
end;

function TThCanvasEditor.IsDrawingItem: Boolean;
begin
  Result := FDrawItemID <> -1;
end;

function TThCanvasEditor.IsMultiSelected: Boolean;
begin
  Result := FSelections.Count > 1;
end;

procedure TThCanvasEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and IsDrawingItem then
  begin
    ClearSelection;
    FDrawItem := AddItem(FDrawItemID);
    if Assigned(FDrawItem) then
    begin
      FDrawItem.Position.Point := PointF(X, Y).Subtract(FContents.Position.Point);
    end;
  end;
end;

procedure TThCanvasEditor.MouseMove(Shift: TShiftState; X, Y: Single);
var
  FromP, ToP: TPointF;
begin
  if IsDrawingItem and Assigned(FDrawItem) then
  begin
    FromP := FMouseDownPos.Subtract(FContents.Position.Point);
    ToP   := PointF(X, Y).Subtract(FContents.Position.Point);

    FDrawItem.DrawItemWithMouse(FromP, ToP);
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
//    FDrawItem.Selected := True;
    if Assigned(FOnItemAdded) then
      FOnItemAdded(FDrawItem);
  end;

  FDrawItem := nil;
  FDrawItemID := -1;
end;

procedure TThCanvasEditor.SetDrawItemID(const Value: Integer);
begin
  FDrawItemID := Value;
end;

end.
