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
//    procedure Paint; override;

    procedure DoInsertShape(AShape: TThShape);
    procedure DoMoveShapes(AShapes: TList; ABefore, AAfter: TPointF);
    procedure DoDeleteShapes(AShapes: TList);
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InsertObject(Index: Integer; AObject: TFmxObject); override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure DrawShape(AClass: TThShapeClass; AStart, AEnd: TPointF);
    property DrawClass: TThShapeClass read FDrawClass write SetDrawClass;
    property DrawMode: TThDrawMode read FDrawMode write FDrawMode;

    procedure ClearSelection;
    procedure SetSelection(AShape: TThShape);    // Unselect & select
    procedure AddSelection(AShape: TThShape); // Multi select
    procedure Unselection(AShape: TThShape);

    procedure DeleteSelectedShapes;

    procedure InsertShape(AShape: TThShape);
    procedure InsertShapes(AShapes: TList);
    procedure MoveShapes(AShapes: TList; ABefore, AAfter: TPointF);
    procedure DeleteShapes(AShapes: TList);

    procedure SelectionScale(Value: Single);
    procedure SelectionRoate(Value: Single);
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
  DeleteShapes(FSelectionList);
  DoDeleteShapes(FSelectionList);
  ClearSelection;
end;

procedure TThCanvas.DeleteShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    TThShape(AShapes[I]).Parent := nil;

  Repaint;
end;

procedure TThCanvas.DoDeleteShapes(AShapes: TList);
begin
  FObjectManager.Subject(Self, TThDeleteShapeCommand.Create(AShapes));
end;

procedure TThCanvas.DoInsertShape(AShape: TThShape);
begin
  FObjectManager.Subject(Self, TThInsertShapeCommand.Create(AShape));
end;

procedure TThCanvas.DoMoveShapes(AShapes: TList; ABefore, AAfter: TPointF);
begin
  FObjectManager.Subject(Self, TThMoveShapeCommand.Create(AShapes, ABefore, AAfter));
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

procedure TThCanvas.InsertObject(Index: Integer; AObject: TFmxObject);
begin
  InvalidateRect(BoundsRect);
  if (FContent <> nil) and (AObject <> FContent) and (AObject <> FResourceLink) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.InsertObject(Index, AObject);
//    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TThCanvas.InsertShape(AShape: TThShape);
begin
  if AShape.Depth > -1 then
  begin
    InsertObject(AShape.Depth, AShape);   // InsertObject이용 시 오류 발생
//    AShape.Parent := Self;
  end
  else
    AShape.Parent := Self;

//  Repaint;
end;

procedure TThCanvas.InsertShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    InsertShape(AShapes[I]);

//  Repaint;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
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
var
  P: TPointF;
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
  begin
    P := PointF(
        FDrawShape.Position.Point.X - (FCurrPos.X - FDownPos.X)
      , FDrawShape.Position.Point.Y - (FCurrPos.Y - FDownPos.Y)
    );
    DoMoveShapes(FSelectionList, P, FDrawShape.Position.Point);
  end;

  FDrawMode := dmNone;
    MouseTracking := True;
end;

procedure TThCanvas.MoveShapes(AShapes: TList; ABefore, AAfter: TPointF);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    TThShape(AShapes[I]).Position.Point := AAfter;

  Repaint;
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  OutputDebugSTring(PChar('TThCanvas - ' + TThShapeCommand(ACommand).ClassName));

  if ACommand is TThInsertShapeCommand then
    // Canvas에 추가
    InsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThDeleteShapeCommand then
    // Canvas에서 제거
    DeleteShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRestoreShapeCommand then
    // Canvas에 InsertObject
    InsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRemoveShapeCommand then
    // Canvas에서 제거(있으면)
  else if ACommand is TThMoveShapeCommand then
    MoveShapes(TThShapeCommand(ACommand).List, TThMoveShapeCommand(ACommand).BeforePos, TThMoveShapeCommand(ACommand).AfterPos)
  ;
end;
//
//procedure TThCanvas.Paint;
//begin
//  inherited;
//end;

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

procedure TThCanvas.SelectionRoate(Value: Single);
var
  I: Integer;
begin
  for I := 0 to FSelectionList.Count - 1 do
  begin
    TThShape(FSelectionList[I]).RotationAngle := Value;
  end;

  Repaint;
end;

procedure TThCanvas.SelectionScale(Value: Single);
var
  I: Integer;
begin
  for I := 0 to FSelectionList.Count - 1 do
  begin
    TThShape(FSelectionList[I]).Scale.Point := PointF(Value, Value);
  end;

  Repaint;
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
