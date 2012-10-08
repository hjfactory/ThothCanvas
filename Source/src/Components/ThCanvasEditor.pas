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
    FDrawItemID: Integer;
    FIsDrawingItem: Boolean;

    procedure SetDrawItemID(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property DrawItemID: Integer read FDrawItemID write SetDrawItemID;
    property IsDrawingItem: Boolean read FIsDrawingItem;
  end;

implementation

uses
  CommonUtils;

{ TThCanvasEditor }

constructor TThCanvasEditor.Create(AOwner: TComponent);
begin
  inherited;

  FDrawItemID := -1;
end;

procedure TThCanvasEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FIsDrawingItem then
  begin
    ClearSelection;
    FDrawItem := InsertItem(FDrawItemID);
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
    R := RectF(FMouseDownPos.X, FMouseDownPos.Y, X, Y);
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
  FDrawItemID := -1;
end;

procedure TThCanvasEditor.SetDrawItemID(const Value: Integer);
begin
  FDrawItemID := Value;

  FIsDrawingItem := True;
end;

end.
