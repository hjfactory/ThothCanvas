unit TestThCommandHistory;

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #23 Undo/Redo����� �̿��Ͽ� ����� �ǵ�����.
  TestTThCommandHistory = class(TBaseCommandHistoryTestUnit)
  published
    // #123 �簢���� �߰��ϰ� Undo����ϸ� ������ ���õ��� �ʾƾ� �Ѵ�.
    procedure TestCommandHistoryAddUndo;

    // #138 �簢�� �߰� �� Undo > Redo �� �簢���� �߰��Ǿ� �־�� �Ѵ�.
    procedure TestCommandHistoryAddRedo;

    // #154 �Ʒ���ġ�� �������� ���� > ���� �� �״�� �Ʒ��� ��ġ�ؾ��Ѵ�.
    procedure TestCommandHistoryDelete;

    // #136 �簢�� �̵� �� Undo ��� �� ���� ������ ��ġ�ؾ� �Ѵ�.
    procedure TestCommandHistoryMove;

    // #137 �簢���� TopLeft�� ũ�� ���� �� Undo ��� �� ���� ũ�� �� ��ġ�� ���ƿ;� �Ѵ�.
    procedure TestCommandHistoryResize;

    // #134 Undo ��ɵ� Ŀ�ǵ� ���� ���ο� Ŀ�ǵ尡 �߻��ɶ� ���ŵǾ�� �Ѵ�.
    procedure TestClearUndoCommand;

    // #135 Undo ������� ��ҵ� �������߰��� ���ο� ����� ������� �������� ���ŵǾ� �Ѵ�.
    procedure TestClearUndoCommandDestroyItem;

    // #154 �Ʒ���ġ�� �������� ����> ���� �� �״�� �Ʒ��� ��ġ�ؾ� �Ѵ�.
    procedure TestBackItemUndoAfterBack;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor,
  ThItem, ThShapeItem, ThItemFactory;

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
  Check(FCanvas.SelectedItem.Position.Point = PointF(-140, -140),
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
  // Redo �� �����ǿ��� ���ŵ��� ����
    // Selected ���� �� Selection ó�� �ʿ�

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
  Check(P = PointF(-90, -90), Format('Org(60,60) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Undo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(-140, -140), Format('Undo(10,10) X: %f, Y: %f', [P.X, P.Y]));

  FThothController.Redo;
  P := FCanvas.SelectedItem.Position.Point;
  Check(P = PointF(-90, -90), Format('Redo(60,60) X: %f, Y: %f', [P.X, P.Y]));
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
  Check(FCanvas.SelectedItem.Width = 150, Format('Width: %f', [FCanvas.SelectedItem.Width]));

  FThothController.Undo;
  CheckNotNull(FCanvas.SelectedItem, 'Undo. Not null');
  Check(FCanvas.SelectedItem.Width = 100, Format('Undo. Width: %f', [FCanvas.SelectedItem.Width]));

  FThothController.Redo;
  CheckNotNull(FCanvas.SelectedItem, 'Redo. Not null');
  Check(FCanvas.SelectedItem.Width = 150, Format('Redo. Width: %f', [FCanvas.SelectedItem.Width]));
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
  Check(FCanvas.SelectedItem.Position.X = -70, '2nd item select');

  TestLib.RunMouseClick(50, 50);
  FCanvas.DeleteSelection;
  FThothController.Undo;

  TestLib.RunMouseClick(90, 90);
  CheckNotNull(FCanvas.SelectedItem);
  Check(FCanvas.SelectedItem.Position.X = -70, 'Undo. 2nd item select');
end;

initialization
  RegisterTest(TestTThCommandHistory.Suite);

end.

