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
  Math, CommonUtils;

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
  if FIsDrawingItem and Assigned(FDrawItem) then
  begin
    FromP := FMouseDownPos.Subtract(FContents.Position.Point);
    ToP   := PointF(X, Y).Subtract(FContents.Position.Point);

    FDrawItem.DrawingWithMouse(FromP, ToP);
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
