unit TestThCommandHistory;

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #23 Undo/Redo기능을 이용하여 명령을 되돌린다.
  TestTThCommandHistory = class(TBaseCommandTestUnit)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // #123 사각형을 추가하고 Undo명령하면 도형이 선택되지 않아야 한다.
    procedure TestCommandHistoryAddUndo;

    // #138 사각형 추가 후 Undo > Redo 시 사각형이 추가되어 있어야 한다.
    procedure TestCommandHistoryAddRedo;

    // #154 아래위치한 아이템을 삭제 > 복구 시 그대로 아래에 위치해야한다.
    procedure TestCommandHistoryDelete;


    procedure TestCommandHistoryMove;
    procedure TestCommandHistoryResize;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor,
  ThItem, ThShape, ThItemFactory;

{ TestTThCommandHistory }

procedure TestTThCommandHistory.SetUp;
begin
  inherited;

end;

procedure TestTThCommandHistory.TearDown;
begin
  inherited;

end;

procedure TestTThCommandHistory.TestCommandHistoryAddUndo;
begin
  DrawRectangle(10, 10, 100, 100);

  FThothController.Undo;

  TestLib.RunMouseClick(50, 50);
  CheckNull(FCanvas.SelectedItem, 'Undo');
end;

procedure TestTThCommandHistory.TestCommandHistoryAddRedo;
begin
  DrawRectangle(10, 10, 100, 100);

  FThothController.Undo;

  TestLib.RunMouseClick(50, 50);
  CheckNull(FCanvas.SelectedItem, 'Undo');

  FThothController.Redo;
  TestLib.RunMouseClick(50, 50);
  CheckNotNull(FCanvas.SelectedItem, 'Redo');
  Check(FCanvas.SelectedItem.Position.Point = PointF(10, 10),
      Format('Position (%f, %f)', [FCanvas.SelectedItem.Position.X, FCanvas.SelectedItem.Position.Y]));
end;

procedure TestTThCommandHistory.TestCommandHistoryDelete;
begin
  DrawRectangle(10, 10, 100, 100);

  TestLib.RunMouseClick(50, 50);

  FCanvas.DeleteSelection;

  TestLib.RunMouseClick(50, 50);
  CheckNull(FCanvas.SelectedItem, 'Delete');

  FThothController.Undo;

  TestLib.RunMouseClick(50, 50);
  CheckNotNull(FCanvas.SelectedItem, 'Undo');

  FThothController.Redo;
  // Redo 시 셀렉션에서 제거되지 않음
    // Selected 변경 시 Selection 처리 필요

  TestLib.RunMouseClick(50, 50);
  CheckNull(FCanvas.SelectedItem, 'Redo');
end;

procedure TestTThCommandHistory.TestCommandHistoryMove;
var
  P: TPointF;
begin
//  ShowForm;

  DrawRectangle(10, 10, 100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(80, 100)
  .Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  CheckNotNull(FCanvas.SelectedItem);
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(60, 60), Format('Org(60,60) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Undo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(10, 10), Format('Undo(10,10) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Redo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(60, 60), Format('Redo(60,60) X: %f, Y: %f', [P.X, P.Y]));
end;

procedure TestTThCommandHistory.TestCommandHistoryResize;
begin

end;

initialization
  RegisterTest(TestTThCommandHistory.Suite);

end.

