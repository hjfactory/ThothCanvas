unit TestThItemGroupingHistory;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #275 Grouping�� Undo / Redo �ÿ��� ����Ǿ�� �Ѵ�.
  TestTThItemGrouppingHistory = class(TBaseCommandHistoryTestUnit)
  published
    // #277 P1�� C1�� �ø��� Undo / Redo �� C1�� �θ�� ��ġ Ȯ��
    procedure TestMoveChild;

    // #276 C1�� P1���� ���� Undo / Redo �� C1�� �θ�� ��ġ Ȯ��
    procedure TestMoveParent;

    // #279 P1�� C1�� �߰��ϰ� Undo / Redo �� C1�� �θ� Ȯ��
    procedure TestAdd;

    // #280 P1���� C1�� �����ϰ� Undo / Redo �� C1�� �θ� Ȯ��
    procedure TestDeleteChild;

    // #281 C1�� ũ�⸦ P1�� ����� Undo/Redo �� C1�� �θ� Ȯ��
    procedure TestResizeChild;

    // #282 P1�� ũ�⸦ ���̰� Undo/Redo �� C1�� �θ� Ȯ��
    procedure TestResizeParent;

    // #249 Undo / Redo �� ItemIndex�� ������� ���ƿ;� �Ѵ�.
    procedure TestRecoveryIndexMove;

    // #285: P1�� C1�� �׸��� C1�� ������ �� �� Undo / Redo
    procedure TestMoveReleaseUndo;

    // #288 Undo / Redo �ݺ� �� Contain ����
      // ���� �θ�� �̵� �� Undo �� �������� �����Ǿ� �Ѵ�.
    procedure TestParentCmdHistory;
    procedure BugUndoRedoIncorrectParent;

    // #291 P1, C1���� P1���� C1�� ���� Undo*2 ���� Redo*2 �� P1�� C1�� �����ؾ� �Ѵ�.
    procedure TestMoveContainChildUndo2Redo2;

    // #290: P1>P2>P3>P4���� C1�� �׸��� ũ�⸦ �����ؼ� P3>P2>P1���� �θ� ���� �� Undo�� ��...
    procedure TestResizeParentAndParent;

    // #293 ũ�⺯��>�θ��̵� �� Undo 2ȸ �� ũ�⺯��� �̵��� ��
    procedure TestResizeAndMoveUndo2;

    // #287 P1�� C1�� �ø��� P2�� C1�� ����, Delete �� Undo*2 �� C1�� �����
    procedure TestDeleteParentUndoHideChild;

    // #286 P1���� P2�� �̵��� C1�� Undo
    procedure TestMoveBetweenParentUndo;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThItemGrouppingMulti }

procedure TestTThItemGrouppingHistory.TestMoveChild;
var
  P1, C1: TThItem;
  C1P_O, C1p_N: TPointF;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(160, 160, 210, 210, 'C1');
  C1P_O := C1.Position.Point;

  TestLib.RunMousePath(MousePath.New
  .Add(180, 180)
  .Add(100, 30)
  .Add(30, 30).Path);

  Check(C1.Parent = P1, 'Contain');
  C1P_N := C1.Position.Point;

  FThothController.Undo;

  Check(C1.Parent <> P1, Format('Undo: %s', [C1.Parent.Name]));
  CheckEquals(C1.Position.Point.X, C1P_O.X, 'C1P_O');

  FThothController.Redo;

  Check(C1.Parent = P1, Format('Redo: %s', [C1.Parent.Name]));
  Check(C1.Position.Point = C1P_N, 'C1P_N');
end;

procedure TestTThItemGrouppingHistory.TestMoveParent;
var
  P1, C1, C2: TThItem;
begin
  // C2�� �ö� P1���� C1�� ������ �̵��Ѵ�.
  // Undo �� P1�� C2�� �����Ͽ� ���ڸ��� ����
  // C1�� �״�� ��ġ�ϴ��� Ȯ���Ѵ�.
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C2 := DrawRectangle(20, 50, 50, 50, 'C2');
  TThRectangle(C2).BgColor := claBlue;

  C1 := DrawRectangle(130, 130, 180, 180, 'C1');
  TThRectangle(C1).BgColor := claRed;

  // P1���� C1 ����
  TestLib.RunMouseClick(20, 20);
  TestLib.RunMousePath(MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(140, 140).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  // C1�� ��ġ, �θ� Ȯ��
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(150, 150);
  CheckEquals(C1.Index, 1, 'C1.Index');
  Check(FCanvas.SelectedItem = C1, 'Not selected C1');
  Check(C1.Parent <> P1, Format('C1.Parent is %s(Not Parent <> P1)', [C1.Parent.Name]));
  Check(C2.Parent = P1, 'C2.Parent is P1');

  FThothController.Redo;

  Check(C1.Parent = P1, Format('Redo: %s', [C1.Parent.Name]));
end;

procedure TestTThItemGrouppingHistory.TestAdd;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(30, 30, 130, 130, 'C1');

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  Check(not Assigned(C1.Parent), Format('Undo: %s', ['C1.Parent is not null']));

  FThothController.Redo;

  Check(C1.Parent = P1, Format('Redo: %s', [C1.Parent.Name]));
end;

procedure TestTThItemGrouppingHistory.TestDeleteChild;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(30, 30, 130, 130, 'C1');

  Check(C1.Parent = P1, 'Contain');

  TestLib.RunMouseClick(100, 100);
  FCanvas.DeleteSelection;

  Check(not Assigned(C1.Parent), Format('Delete: %s', ['C1.Parent is not null']));

  FThothController.Undo;

  Check(Assigned(C1.Parent), Format('Undo: %s', ['C1.Parent is null']));
  Check(C1.Parent = P1, Format('Undo: %s', [C1.Parent.Name]));

  FThothController.Redo;

  CheckEquals(P1.ItemCount, 0, Format('ItemCount: %d', [P1.ItemCount]));
end;

procedure TestTThItemGrouppingHistory.TestResizeChild;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 250, 250, 'P1');
  C1 := DrawRectangle(130, 130, 290, 290, 'C1');

  Check(C1.Parent <> P1, 'Not contain');
  CheckEquals(C1.Position.X, -20, Format('C1.Position.X : %f', [C1.Position.X]));

  TestLib.RunMouseClick(200, 200);
  TestLib.RunMousePath(MousePath.New
  .Add(290, 290)
  .Add(200, 200)
  .Add(230, 230).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;
  Application.ProcessMessages;

  Check(C1.Parent <> P1, 'Undo> Not contain');
  CheckEquals(C1.Position.X, -20, Format('Undo> C1.Position.X : %f', [C1.Position.X]));

  FThothController.Redo;
  Application.ProcessMessages;

  Check(C1.Parent = P1, 'Contain');
end;

procedure TestTThItemGrouppingHistory.TestResizeParent;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 250, 250, 'P1');
  C1 := DrawRectangle(130, 130, 240, 240, 'C1');

  Check(C1.Parent = P1, 'Contain');
  CheckEquals(C1.Position.X, 20, Format('C1.Position.X : %f', [C1.Position.X]));

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New
  .Add(250, 250)
  .Add(200, 200)
  .Add(230, 230).Path);

  Check(C1.Parent <> P1, 'Not contain');

  FThothController.Undo;

  Check(C1.Parent = P1, 'Undo> Contain');
  CheckEquals(C1.Position.X, 20, Format('Undo> C1.Position.X : %f', [C1.Position.X]));

  FThothController.Redo;

  Check(C1.Parent <> P1, 'Redo> Not contain');
end;

procedure TestTThItemGrouppingHistory.TestRecoveryIndexMove;
var
  P1, P2, C1: TThItem;
  C1P: TPointF;
begin
  // C1���� P2�� ��ġ�� �׸���
  // C1�� P1�� �ø� �� Undo �� C1�� �״�� P2�Ʒ��� �־�� �Ѵ�.

  P1 := DrawRectangle(10, 10, 130, 130, 'P1');
  C1 := DrawRectangle(150, 150, 200, 200, 'C1');
  P2 := DrawRectangle(170, 170, 250, 250, 'P2');

  C1P := C1.Position.Point;

  TestLib.RunMouseClick(180, 180);
  Check(FCanvas.SelectedItem = P2);

  TestLib.RunMouseClick(155, 155);
  TestLib.RunMousePath(MousePath.New
  .Add(160, 160)
  .Add(100, 100)
  .Add(30, 30).Path);
  Check(C1.Parent = P1, Format('C1.Parent = %s', [C1.Name]));

  FThothController.Undo;

  TestLib.RunMouseClick(180, 180);
  Check(FCanvas.SelectedItem = P2, Format('Selection Item = %s(Not P2)', [FCanvas.SelectedItem.Name]));
  Check(C1.Position.Point = C1P, Format('C1.Position.Point: %f.%f', [C1.Position.Point.X, C1.Position.Point.Y]));
end;

procedure TestTThItemGrouppingHistory.TestMoveReleaseUndo;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 140, 140, 'P1');
  C1 := DrawRectangle(50, 50, 120, 120, 'C1');
  Check(C1.Parent = P1, Format('[0] C1.Parent = %s', [C1.Name]));
  CheckEquals(C1.Position.X, 40);

//  TestLib.RunMouseClick(65, 65);
  TestLib.RunMousePath(MousePath.New
  .Add(65, 65)
  .Add(100, 100)
  .Add(165, 165).Path);
  Check(C1.Parent <> P1, Format('[1] C1.Parent <> %s', [C1.Name]));
  CheckEquals(C1.AbsolutePoint.X, 0);

  FThothController.Undo;

  Check(C1.Parent = P1, Format('[2] C1.Parent = %s', [C1.Name]));
  CheckEquals(C1.Position.X, 40);
end;

procedure TestTThItemGrouppingHistory.TestParentCmdHistory;
var
  P1, P2, P3, C1: TThItem;
begin
  // P1�� C1 �׸�
  P1 := DrawRectangle(10, 210, 60, 260, 'P1');
  P2 := DrawRectangle(70, 210, 120, 260, 'P2');
  P3 := DrawRectangle(130, 210, 180, 260, 'P3');
  C1 := DrawRectangle(10, 10, 40, 40, 'C1');

  // Contain P1
  TestLib.RunMousePath(MousePath.New
  .Add(20, 20).Add(100, 100).Add(20, 220).Path);
  Check(C1.Parent = P1, '[0] P1');

  // Contain P2
  TestLib.RunMousePath(MousePath.New
  .Add(20, 220).Add(100, 100).Add(80, 220).Path);
  Check(C1.Parent = P2, '[0] P2');

  // Contain P3
  TestLib.RunMousePath(MousePath.New
  .Add(80, 220).Add(100, 100).Add(140, 220).Path);
  Check(C1.Parent = P3, '[0] P3');

  // Contain P3
  TestLib.RunMousePath(MousePath.New
  .Add(140, 220).Add(100, 100).Add(140, 20).Path);
  Check(C1.Parent = FCanvas.Controls[0], '[0] Cotents');

  Debug('Undo start');

// �θ� �ǵ�����
  FThothController.Undo;
  Check(C1.Parent = P3, '[1] P3');

  FThothController.Undo;
  Check(C1.Parent = P2, '[1] P2');

  FThothController.Undo;
  Check(C1.Parent = P1, '[1] P1');

  FThothController.Undo;
  Check(C1.Parent = FCanvas.Controls[0], '[1] Cotents');
end;

procedure TestTThItemGrouppingHistory.BugUndoRedoIncorrectParent;
var
  P1, C1: TThItem;
begin
  // P1�� C1 �׸�
  P1 := DrawRectangle(10, 10, 140, 140, 'P1');
  C1 := DrawRectangle(50, 50, 120, 120, 'C1');
  Check(C1.Parent = P1);

  // P1�̵�
//  TestLib.RunMousePath(MousePath.New
//  .Add(20, 20)
//  .Add(100, 100)
//  .Add(40, 40).Path);

  // C1�̵�(�����)
  TestLib.RunMousePath(MousePath.New
  .Add(100, 100)
  .Add(100, 100)
  .Add(180, 180).Path);

  Check(C1.Parent <> P1);

  FThothController.Undo;  // C1 Move
  Application.ProcessMessages;
//  FThothController.Undo;  // P1 Move
  FThothController.Undo;  // Draw C1
  Application.ProcessMessages;

  FThothController.Redo;  // Draw C1
  Application.ProcessMessages;
//  FThothController.Redo;  // P1 Move
  FThothController.Redo;  // C1 Move
  Application.ProcessMessages;

  Check(C1.Parent <> P1, C1.Parent.Name);
end;

procedure TestTThItemGrouppingHistory.TestMoveContainChildUndo2Redo2;
var
  P1, C1: TThItem;
begin
  // P1�� C1 �׸�
  P1 := DrawRectangle(10, 10, 100, 100, 'P1');
  C1 := DrawRectangle(150, 150, 180, 180, 'C1');

  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(100, 100)
  .Add(170, 170).Path);

  Check(C1.Parent = P1, '[0] C1.Parent = P1');
  CheckEquals(C1.Position.X, 50, 4, Format('[0] %f', [C1.Position.X]));

  FThothController.Undo;  // C1 Move
  Application.ProcessMessages;
//  FThothController.Undo;  // P1 Move
  FThothController.Undo;  // Draw C1
  Application.ProcessMessages;

  FThothController.Redo;  // Draw C1
  Application.ProcessMessages;
//  FThothController.Redo;  // P1 Move
  FThothController.Redo;  // C1 Move
  Application.ProcessMessages;

  Check(C1.Parent = P1, '[1] C1.Parent = P1');
  CheckEquals(C1.Position.X, 50, 4, Format('[1] %f', [C1.Position.X]));
end;

procedure TestTThItemGrouppingHistory.TestResizeParentAndParent;
var
  P1, P2, P3, P4, C1: TThItem;
begin
  // P1�� C1 �׸�
  P1 := DrawRectangle(10, 10, 250, 250, 'P1');
  P2 := DrawRectangle(20, 20, 220, 220, 'P2');
  P3 := DrawRectangle(30, 30, 190, 190, 'P3');
  P4 := DrawRectangle(40, 40, 160, 160, 'P4');

  C1 := DrawRectangle(100, 100, 130, 130, 'C1');
  Check(C1.Parent = P4, '[0] P4');
  CheckEquals(C1.Position.X, 60);

  TestLib.RunMousePath(MousePath.New
  .Add(130, 130).Add(100, 100)
  .Add(170, 170).Path);
  Check(C1.Parent = P3, '[0] P3');
  CheckEquals(C1.Position.X, 70);

  TestLib.RunMousePath(MousePath.New
  .Add(170, 170).Add(100, 100)
  .Add(200, 200).Path);
  Check(C1.Parent = P2, '[0] P2');
  CheckEquals(C1.Position.X, 80);

  TestLib.RunMousePath(MousePath.New
  .Add(200, 200).Add(100, 100)
  .Add(230, 230).Path);
  Check(C1.Parent = P1, '[0] P1');
  CheckEquals(C1.Position.X, 90);

  TestLib.RunMousePath(MousePath.New
  .Add(230, 230).Add(100, 100)
  .Add(260, 260).Path);
  Check(C1.Parent <> P1, '[0] P1');
  CheckEquals(C1.Position.X, -50);

// Undo Action

  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P1, '[1] P1');
  CheckEquals(C1.Position.X, 90);

  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P2, '[1] P2');
  CheckEquals(C1.Position.X, 80);

  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P3, '[1] P3');
  CheckEquals(C1.Position.X, 70);

  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P4, '[1] P4');
  CheckEquals(C1.Position.X, 60);

// RedoAction
  FThothController.Redo;
  Application.ProcessMessages;
  Check(C1.Parent = P3, '[2] P3');
  CheckEquals(C1.Position.X, 70);

  FThothController.Redo;
  Application.ProcessMessages;
  Check(C1.Parent = P2, '[2] P2');
  CheckEquals(C1.Position.X, 80);

  FThothController.Redo;
  Application.ProcessMessages;
  Check(C1.Parent = P1, '[2] P1');
  CheckEquals(C1.Position.X, 90);

  FThothController.Redo;
  Application.ProcessMessages;
  Check(C1.Parent <> P1, '[2] P1');
  CheckEquals(C1.Position.X, -50);
end;

procedure TestTThItemGrouppingHistory.TestResizeAndMoveUndo2;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 140, 290, 'P1');
  P2 := DrawRectangle(160, 10, 290, 290, 'P2');

  C1 := DrawRectangle(170, 20, 280, 150, 'C1');
  Check(C1.Parent = P2, '[0] C1.Parent = P2');

  TestLib.RunMousePath(MousePath.New
  .Add(280, 150).Add(100, 100)
  .Add(220, 80).Path);
  Check(C1.Parent = P2, '[1] C1.Parent = P2');

  TestLib.RunMousePath(MousePath.New
  .Add(180, 30).Add(100, 100)
  .Add(30, 30).Path);
  Check(C1.Parent = P1, '[1] C1.Parent = P1');

  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P2, '[2] C1.Parent = P2');
  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P2, '[3] C1.Parent = P2');
  CheckEquals(C1.Position.X, 10);
end;

procedure TestTThItemGrouppingHistory.TestDeleteParentUndoHideChild;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(80, 80, 120, 120, 'C1');

  P2 := DrawRectangle(30, 30, 140, 140, 'P2');
  Check(C1.Parent = P2, '[0] C1.Parent = P2');

  FCanvas.DeleteSelection;
  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P2, '[1] C1.Parent = P2');
  FThothController.Undo;
  Application.ProcessMessages;
  Check(C1.Parent = P1, '[2] C1.Parent = P1');
end;

procedure TestTThItemGrouppingHistory.TestMoveBetweenParentUndo;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 140, 290, 'P1');
  P2 := DrawRectangle(160, 10, 290, 290, 'P2');

  C1 := DrawRectangle(170, 20, 200, 50, 'C1');
  Check(C1.Parent = P2, '[0] C1.Parent = P2');

  TestLib.RunMousePath(MousePath.New
  .Add(185, 35).Add(100, 100)
  .Add(35, 35).Path);
  Check(C1.Parent = P1, '[1] C1.Parent = P1');

  FThothController.Undo;
  Check(C1.Parent = P2, '[2] C1.Parent = P2');

  FThothController.Redo;
  Check(C1.Parent = P1, '[3] C1.Parent = P1');
end;

initialization
  RegisterTest(TestTThItemGrouppingHistory.Suite);

end.
