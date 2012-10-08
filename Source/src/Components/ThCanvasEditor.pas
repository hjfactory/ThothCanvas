unit ThCanvasEditor;

interface

uses
  System.UITypes, System.Classes, System.Types, System.SysUtils,
  ThContainer, ThItem;

type
{
  Features
    - ThContainers features
    - ThItem control(add, modify, delete)
}
  TThCanvasEditor = class(TThContainer)
  private
    FDrawItem: TThItem;
    FItemID: Integer;
    FIsDrawingItem: Boolean;

    procedure SetItemID(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ItemID: Integer read FItemID write SetItemID;
    property IsDrawingItem: Boolean read FIsDrawingItem;
  end;

implementation

uses
  CommonUtils;

{ TThCanvasEditor }

constructor TThCanvasEditor.Create(AOwner: TComponent);
begin
  inherited;

  FItemID := -1;
end;

procedure TThCanvasEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FIsDrawingItem then
  begin
    ClearSelection;
    FDrawItem := InsertItem(FItemID);
    if Assigned(FDrawItem) then
    begin
      FDrawItem.Position.Point := PointF(X, Y).Subtract(FContents.Position.Point);
    end;
  end;
end;

procedure TThCanvasEditor.MouseMove(Shift: TShiftState; X, Y: Single);
var
  R: TRectF;
begin
  if FIsDrawingItem and Assigned(FDrawItem) then
  begin
    R := RectF(FDownPos.X, FDownPos.Y, X, Y);
    R.Offset(-FContents.Position.X, -FContents.Position.Y);
    R.NormalizeRect;
    FDrawItem.BoundsRect := R;
  end
  else
    inherited;
end;

procedure TThCanvasEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  FIsDrawingItem := False;
  FDrawItem := nil;
  FItemID := -1;
end;

procedure TThCanvasEditor.SetItemID(const Value: Integer);
begin
  FItemID := Value;

  FIsDrawingItem := True;
end;

end.
