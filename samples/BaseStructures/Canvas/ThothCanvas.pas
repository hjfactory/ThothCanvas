unit ThothCanvas;

{
그리는 도중에는 버퍼(FBufferBmp)에???
Canvas의 영역을 Bitmap으로 저장하는 방법 필요
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Layouts,
  ThothTypes, ThothObjects, ThothCommands, ObjectManager;

type
  TThDrawMode = (dmSelect, dmMove, dmDraw, dmDrawing);

///////////////////////////////////////////////////////
// Canvas
  TThCanvas = class(TFramedScrollBox, IThObserver, IThCanvas)
  private
//    FObjectManager: TThothObjectManager;
    FObjectManager: IThSubject;
    FSelectionList: TList;

    FDrawShape: TThShape;
    FDrawClass: TThShapeClass;
    FDrawMode: TThDrawMode;

    function PointInShape(const X, Y: Single): TThShape;
    procedure SetDrawClass(const Value: TThShapeClass);
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand); virtual;
    procedure SetSubject(ASubject: IThSubject);

    procedure DrawShape(AClass: TThShapeClass; AStart, AEnd: TPointF);
    property DrawClass: TThShapeClass read FDrawClass write SetDrawClass;
    property DrawMode: TThDrawMode read FDrawMode write FDrawMode;

    procedure ClearSelection;
    procedure Selection(AShape: TThShape);    // Unselect & select
    procedure AddSelection(AShape: TThShape); // Multi select
  end;


implementation

uses
  Winapi.Windows, System.Math;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FDrawClass := nil;

  FSelectionList := TList.Create;
end;

destructor TThCanvas.Destroy;
begin
  FSelectionList.Clear;
  FSelectionList.Free;

  inherited;
end;

procedure TThCanvas.DrawShape(AClass: TThShapeClass; AStart, AEnd: TPointF);

begin
  with AClass.Create(Self) do
  begin
    Position.X := AStart.X;
    Position.Y := AStart.Y;
    Width := Abs(AStart.X - AEnd.X);
    Height := Abs(AStart.Y - AEnd.Y);
    Parent := Self;
  end;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Shape: TThShape;
begin
  inherited;

  if FDrawMode = dmSelect then
  begin
    ClearSelection;
    Exit;
  end;

  FDrawShape := FDrawClass.Create(Self);
  FDrawShape.Position.X := X;
  FDrawShape.Position.Y := Y;
  FDrawShape.Parent := Self;

  FDrawMode := dmDrawing;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FDrawMode = dmDrawing then
  begin
    FDrawShape.Width := X - FDrawShape.Position.X;
    FDrawShape.Height := Y - FDrawShape.Position.Y;
  end;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FDrawMode = dmDrawing then
  begin
    FDrawShape.Width := Abs(X - FDrawShape.Position.X);
    FDrawShape.Height := Abs(Y - FDrawShape.Position.Y);

    FDrawShape.Position.X := Min(X, FDrawShape.Position.X);
    FDrawShape.Position.Y := Min(Y, FDrawShape.Position.Y);

    FObjectManager.Report(TThInsertShapeCommand.Create(FDrawShape));
    FDrawMode := dmSelect;
  end;
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  if ACommand is TThInsertShapeCommand then
    OutputDebugSTring(PChar('TThCanvas TThInsertShapeCommand'));
end;

function TThCanvas.PointInShape(const X, Y: Single): TThShape;
begin
  Result := nil;
end;

procedure TThCanvas.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to FSelectionList.Count - 1 do
    TThShape(FSelectionList[I]).Selected := False;
  FSelectionList.Clear;
end;

procedure TThCanvas.Selection(AShape: TThShape);
begin
  ClearSelection;
  AddSelection(AShape);
end;

procedure TThCanvas.AddSelection(AShape: TThShape);
begin
  AShape.Selected := True;
  FSelectionList.Add(AShape);
end;

procedure TThCanvas.SetDrawClass(const Value: TThShapeClass);
begin
  FDrawClass := Value;
  if Value = nil then
  begin
    FDrawMode := dmSelect;
  end
  else
  begin
    FDrawMode := dmDraw;
  end;
end;

procedure TThCanvas.SetSubject(ASubject: IThSubject);
begin
  if Assigned(ASubject) then
    FObjectManager := TThothObjectManager(ASubject);
  ASubject.RegistObserver(Self);
end;

end.
