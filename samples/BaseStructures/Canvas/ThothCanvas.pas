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
  TThDrawMode = (dmNone, dmSelect, dmMove, dmDraw, dmDrawing);

///////////////////////////////////////////////////////
// Canvas
  TThCanvas = class(TScrollBox, IThObserver, IThCanvas)
  private
//    FObjectManager: TThothObjectManager;
//    FTest: TThShape;
    FObjectManager: IThSubject;
    FSelectionList: TList;

    FDownPos, FCurrPos: TPointF;
    FDrawShape: TThShape;
    FDrawClass: TThShapeClass;
    FDrawMode: TThDrawMode;

    function PointInShape(const X, Y: Single): TThShape;
    procedure SetDrawClass(const Value: TThShapeClass);
  protected
    procedure Paint; override;

    procedure DoInsertShape(AShape: TThShape);
    procedure DoMoveShapes;
    procedure DoDeleteShapes;
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
    procedure SetSelection(AShape: TThShape);    // Unselect & select
    procedure AddSelection(AShape: TThShape); // Multi select
    procedure Unselection(AShape: TThShape);

    procedure DeleteSelectedShapes;
  end;


implementation

uses
  Winapi.Windows, System.Math;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FDrawClass := nil;
  FDrawMode := dmNone;

  FSelectionList := TList.Create;

  MouseTracking := True;
  DisableMouseWheel := True;
//  ShowScrollBars := False;
end;

destructor TThCanvas.Destroy;
begin
  FSelectionList.Clear;
  FSelectionList.Free;

//  if Assigned(FTest) then
//    FTest.Free;

  inherited;
end;

procedure TThCanvas.DeleteSelectedShapes;
begin
  DoDeleteShapes;
end;

procedure TThCanvas.DoDeleteShapes;
var
  I: Integer;
begin
  FObjectManager.Report(TThDeleteShapeCommand.Create(Self, FSelectionList));

//  FTest := TThShape(FSelectionList[0]);

  for I := 0 to FSelectionList.Count - 1 do
    TThShape(FSelectionList[I]).Parent := nil;

  ClearSelection;

  Repaint;
end;

procedure TThCanvas.DoInsertShape(AShape: TThShape);
begin
  FObjectManager.Report(TThInsertShapeCommand.Create(Self, AShape));
end;

procedure TThCanvas.DoMoveShapes;
begin
  FObjectManager.Report(TThMoveShapeCommand.Create(Self, FSelectionList));
end;

procedure TThCanvas.DrawShape(AClass: TThShapeClass; AStart, AEnd: TPointF);

begin
  with AClass.Create(nil) do
  begin
    Position.X := AStart.X;
    Position.Y := AStart.Y;
    Width := Abs(AStart.X - AEnd.X);
    Height := Abs(AStart.Y - AEnd.Y);
    Parent := Self;

//    Children
  end;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Shape: TThShape;
begin

  inherited;

  if  (FDrawMode in [dmSelect, dmDraw]) then
  begin
    MouseTracking := False;
  end;

  FDownPos := PointF(X, Y);
  FCurrPos := FDownPos;

//  if 공백클릭 시 then

  if FDrawMode <> dmSelect then
    ClearSelection;

  if FDrawMode = dmSelect then
    FDrawMode := dmMove;

  if FDrawMode in [dmNone, dmSelect, dmMove] then
  begin
    Exit;
  end;

  FDrawShape := FDrawClass.Create(Self);
  FDrawShape.Position.X := X;
  FDrawShape.Position.Y := Y;
  FDrawShape.Parent := Self;

  FDrawMode := dmDrawing;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
var
  I: Integer;
  Shape: TThShape;
begin
  inherited;

  if FDrawMode = dmDrawing then
  begin
    FDrawShape.Width := X - FDrawShape.Position.X;
    FDrawShape.Height := Y - FDrawShape.Position.Y;
  end
  else if (FDrawMode in [dmSelect, dmMove]) and (ssLeft in Shift) then
  begin
//    Exit;
    for I := 0 to FSelectionList.Count - 1 do
    begin
      Shape := TThShape(FSelectionList[I]);
      Shape.Position.X := Shape.Position.X + X - FCurrPos.X{ - FDownPos.X};
      Shape.Position.Y := Shape.Position.Y + Y - FCurrPos.Y{ - FDownPos.Y};
    end;

    FCurrPos := PointF(X, Y);
    Repaint;
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

    DoInsertShape(FDrawShape);
  end;

  if (FDrawMode = dmMove) and (FDownPos <> FCurrPos) then
    DoMoveShapes;

  FDrawMode := dmNone;
    MouseTracking := True;
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  OutputDebugSTring(PChar('TThCanvas - ' + TThShapeCommand(ACommand).ClassName));

  if ACommand is TThInsertShapeCommand then
    // Canvas에 추가
  else if ACommand is TThDeleteShapeCommand then
    // Canvas에서 제거
  else if ACommand is TThRestoreShapeCommand then
    // Canvas에 InsertObject
  else if ACommand is TThRemoveShapeCommand then
    // Canvas에서 제거(있으면)
  ;
end;

procedure TThCanvas.Paint;
begin
  inherited;
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

procedure TThCanvas.SetSelection(AShape: TThShape);
begin
  ClearSelection;
  AddSelection(AShape);
end;

procedure TThCanvas.AddSelection(AShape: TThShape);
begin
  TThShape(AShape).Selected := True;
  FSelectionList.Add(AShape);

  FDrawMode := dmSelect;
end;

procedure TThCanvas.Unselection(AShape: TThShape);
begin
  TThShape(AShape).Selected := False;
  FSelectionList.Remove(AShape);

  if FSelectionList.Count = 0 then
    FDrawMode := dmNone
  else
    FDrawMode := dmSelect;
end;

procedure TThCanvas.SetDrawClass(const Value: TThShapeClass);
begin
  FDrawClass := Value;
  if Value = nil then
  begin
    FDrawMode := dmNone;
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
