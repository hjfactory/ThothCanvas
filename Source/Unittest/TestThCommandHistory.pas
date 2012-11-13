unit TestThCommandHistory;

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #23 Undo/Redo기능을 이용하여 명령을 되돌린다.
  TestTThCommandHistory = class(TBaseCommandTestUnit)
  published
    // #123 사각형을 추가하고 Undo명령하면 도형이 선택되지 않아야 한다.
    procedure TestCommandHistoryAddUndo;

    // #138 사각형 추가 후 Undo > Redo 시 사각형이 추가되어 있어야 한다.
    procedure TestCommandHistoryAddRedo;

    // #154 아래위치한 아이템을 삭제 > 복구 시 그대로 아래에 위치해야한다.
    procedure TestCommandHistoryDelete;

    // #136 사각형 이동 후 Undo 명령 시 기존 영역에 위치해야 한다.
    procedure TestCommandHistoryMove;

    // #137 사각형의 TopLeft로 크기 변경 후 Undo 명령 시 이전 크기 및 위치에 돌아와야 한다.
    procedure TestCommandHistoryResize;

    // #134 Undo 명령된 커맨드 들은 새로운 커맨드가 발생될때 제거되어야 한다.
    procedure TestClearUndoCommand;

    // #135 Undo 명령으로 취소된 아이템추가는 새로운 명령이 있을경우 아이템이 제거되야 한다.
    procedure TestClearUndoCommandDestroyItem;

    // #154 아래위치한 아이템을 삭제> 복구 시 그대로 아래에 위치해야 한다.
    procedure TestBackItemUndoAfterBack;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor,
  ThItem, ThShape, ThItemFactory;

{ TestTThCommandHistory }

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
  Check(FCanvas.SelectedItem.Position.Point = PointF(100, 100),
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
  DrawRectangle(10, 10, 100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(80, 100)
  .Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  CheckNotNull(FCanvas.SelectedItem);
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(600, 600), Format('Org(60,60) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Undo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(100, 100), Format('Undo(10,10) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Redo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(600, 600), Format('Redo(60,60) X: %f, Y: %f', [P.X, P.Y]));
end;

procedure TestTThCommandHistory.TestCommandHistoryResize;
begin
  DrawRectangle(100, 100, 200, 200);

  TestLib.RunMouseClick(150, 150);
  MousePath.New
  .Add(100, 100)
  .Add(80, 100)
  .Add(50, 50);
  TestLib.RunMousePath(MousePath.Path);

  CheckNotNull(FCanvas.SelectedItem, 'Not null');
  Check(FCanvas.SelectedItem.Width = 1500, Format('Width: %f', [FCanvas.SelectedItem.Width]));

  FThothController.Undo;
  CheckNotNull(FCanvas.SelectedItem, 'Undo. Not null');
  Check(FCanvas.SelectedItem.Width = 1000, Format('Undo. Width: %f', [FCanvas.SelectedItem.Width]));

  FThothController.Redo;
  CheckNotNull(FCanvas.SelectedItem, 'Redo. Not null');
  Check(FCanvas.SelectedItem.Width = 1500, Format('Redo. Width: %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThCommandHistory.TestClearUndoCommand;
begin
  DrawRectangle(100, 100, 200, 200);
  FThothController.Undo;
  Check(FThothController.RedoCount = 1, 'RedoCount = 1');

  DrawRectangle(100, 100, 200, 200);
  Check(FThothController.RedoCount = 0, 'RedoCount = 0');
end;

procedure TestTThCommandHistory.TestClearUndoCommandDestroyItem;
begin
  DrawRectangle(10, 10, 80, 80);
  DrawRectangle(100, 100, 200, 200);
  Check(FThothController.ItemCount = 2, 'ItemCount = 2');

  FThothController.Undo;
  Check(FThothController.ItemCount = 2, 'Undo. ItemCount = 2');

  MousePath.New
  .Add(50, 50)
  .Add(80, 100);
  TestLib.RunMousePath(MousePath.Path);

  Check(FThothController.ItemCount = 1, 'New cmd. ItemCount = 1');
end;

procedure TestTThCommandHistory.TestBackItemUndoAfterBack;
begin
  DrawRectangle(250, 250, 300, 300);
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(80, 80, 200, 200);

  TestLib.RunMouseClick(90, 90);
  CheckNotNull(FCanvas.SelectedItem);
  Check(FCanvas.SelectedItem.Position.X = 800, '2nd item select');

  TestLib.RunMouseClick(50, 50);
  FCanvas.DeleteSelection;
  FThothController.Undo;

  TestLib.RunMouseClick(90, 90);
  CheckNotNull(FCanvas.SelectedItem);
  Check(FCanvas.SelectedItem.Position.X = 800, 'Undo. 2nd item select');
end;

initialization
  RegisterTest(TestTThCommandHistory.Suite);

end.

