unit ThCanvasEditor;

interface

uses
  System.UITypes, System.Classes, System.Types, System.SysUtils, FMX.Types,
  ThContainer, ThTypes, ThItem, ThClasses;

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

    procedure SetDrawItemID(const Value: Integer);
    function GetSelectionCount: Integer;
  protected
    procedure ClickCanvas; override;
    procedure ItemSelect(Sender: TObject; IsMultiple: Boolean);
    procedure ItemUnselect(Sender: TObject);
    procedure ItemMove(Sender: TObject; X, Y: Single);
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
    Result.OnTrack := ItemMove;
  end;
end;

procedure TThCanvasEditor.ClickCanvas;
begin
  ClearSelection;
end;

procedure TThCanvasEditor.ItemMove(Sender: TObject; X, Y: Single);
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

procedure TThCanvasEditor.ItemSelect(Sender: TObject; IsMultiple: Boolean);
var
  I: Integer;
begin
  if not IsMultiple then
    ClearSelection;

  FSelectedItem := TThItem(Sender);
  FSelections.Add(FSelectedItem);

  for I := 0 to FSelections.Count - 1 do
    FSelections[I].Selected := True;
end;

procedure TThCanvasEditor.ItemUnselect(Sender: TObject);
var
  I: Integer;
begin
  TThItem(Sender).Selected := False;
  FSelections.Remove(TThItem(Sender));
  FSelectedItem := nil;
  if FSelections.Count > 0 then
    FSelectedItem := FSelections.Last;

  for I := 0 to FSelections.Count - 1 do
    FSelections[I].Selected := True;
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
  for I := FSelections.Count - 1 downto 0 do
  begin
    FSelections[I].Repaint;
    FSelections[I].Free;
//    FSelections[I].ParentCanvas := nil;
  end;
  FSelections.Clear;
  FSelectedItem := nil;
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

  FDrawItem := nil;
  FDrawItemID := -1;
end;
{
procedure TThCanvasEditor.Paint;
var
  I: Integer;
  R, R2: TRectF;
begin
  inherited;

  if FSelections.Count <= 1 then
    Exit;

  for I := 0 to FSelections.Count - 1 do
  begin
    R2 := FSelections[I].ClipRect;
    R2.Offset(FSelections[I].Position.X, FSelections[I].Position.Y);
    if I = 0 then
      R := R2
    else
      R := UnionRect(R, R2);
  end;

  InflateRect(R, 2, 2);
  Canvas.StrokeThickness := 3;
  Canvas.Stroke.Color := $FFFF0000;
  Canvas.DrawRect(R, 0, 0, AllCorners, 1);

  InvalidateRect(R);
end;
}
procedure TThCanvasEditor.SetDrawItemID(const Value: Integer);
begin
  FDrawItemID := Value;
end;

end.
