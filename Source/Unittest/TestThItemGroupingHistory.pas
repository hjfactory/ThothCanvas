unit TestThItemGroupingHistory;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #260 중복 그루핑이 처리되야 한다.
  // #256 그루핑 예외 및 기타
  TestTThItemGrouppingHistory = class(TBaseCommandHistoryTestUnit)
  published
    // #277 P1에 C1을 올리고 Undo / Redo 후 C1의 부모와 위치 확인
    procedure TestMoveChild;

    // #276 C1을 P1으로 덮고 Undo / Redo 후 C1의 부모와 위치 확인
    procedure TestMoveParent;

    // #279 P1에 C1을 추가하고 Undo / Redo 후 C1의 부모를 확인
    procedure TestAdd;

    // #280 P1위의 C1을 삭제하고 Undo / Redo 후 C1의 부모를 확인
    procedure TestDeleteChild;

    // #281 C1의 크기를 P1을 벗어나고 Undo/Redo 후 C1의 부모를 확인
    procedure TestResizeChild;

    // #282 P1의 크기를 줄이고 Undo/Redo 후 C1의 부모를 확인
    procedure TestResizeParent;

    // #249 Undo / Redo 시 ItemIndex가 원래대로 돌아와야 한다.
    procedure TestRecoveryIndexMove;

    // #285: P1에 C1을 그리고 C1을 밖으로 뺀 후 Undo / Redo
    procedure TestMoveReleaseUndo;

    // #288 Undo / Redo 반복 시 Contain 오류
      // 여러 부모로 이동 후 Undo 시 이전으로 원복되야 한다.
    procedure TestParentCmdHistory;
    procedure BugUndoRedoIncorrectParent;
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
  // C2가 올라간 P1으로 C1을 덮도록 이동한다.
  // Undo 시 P1이 C2를 포함하여 제자리에 가고
  // C1이 그대로 위치하는지 확인한다.
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C2 := DrawRectangle(20, 50, 50, 50, 'C2');
  TThRectangle(C2).BgColor := claBlue;

  C1 := DrawRectangle(130, 130, 180, 180, 'C1');
  TThRectangle(C1).BgColor := claRed;

  // P1으로 C1 덮기
  TestLib.RunMouseClick(20, 20);
  TestLib.RunMousePath(MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(140, 140).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  // C1의 위치, 부모 확인
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
  CheckEquals(C1.Position.X, -200, Format('C1.Position.X : %f', [C1.Position.X]));

  TestLib.RunMouseClick(200, 200);
  TestLib.RunMousePath(MousePath.New
  .Add(290, 290)
  .Add(200, 200)
  .Add(230, 230).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;
  Application.ProcessMessages;

  Check(C1.Parent <> P1, 'Undo> Not contain');
  CheckEquals(C1.Position.X, -200, Format('Undo> C1.Position.X : %f', [C1.Position.X]));

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
  CheckEquals(C1.Position.X, 200, Format('C1.Position.X : %f', [C1.Position.X]));

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New
  .Add(250, 250)
  .Add(200, 200)
  .Add(230, 230).Path);

  Check(C1.Parent <> P1, 'Not contain');

  FThothController.Undo;

  Check(C1.Parent = P1, 'Undo> Contain');
  CheckEquals(C1.Position.X, 200, Format('Undo> C1.Position.X : %f', [C1.Position.X]));

  FThothController.Redo;

  Check(C1.Parent <> P1, 'Redo> Not contain');
end;

procedure TestTThItemGrouppingHistory.TestRecoveryIndexMove;
var
  P1, P2, C1: TThItem;
  C1P: TPointF;
begin
  // C1위에 P2를 겹치게 그리고
  // C1을 P1에 올린 후 Undo 시 C1이 그대로 P2아래에 있어야 한다.

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
  CheckEquals(C1.Position.X, 400);

//  TestLib.RunMouseClick(65, 65);
  TestLib.RunMousePath(MousePath.New
  .Add(65, 65)
  .Add(100, 100)
  .Add(165, 165).Path);
  Check(C1.Parent <> P1, Format('[1] C1.Parent <> %s', [C1.Name]));
  CheckEquals(C1.AbsolutePoint.X, 0);

  FThothController.Undo;

  Check(C1.Parent = P1, Format('[2] C1.Parent = %s', [C1.Name]));
  CheckEquals(C1.Position.X, 400);
end;

procedure TestTThItemGrouppingHistory.TestParentCmdHistory;
var
  P1, P2, P3, C1: TThItem;
begin
  // P1에 C1 그림
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

// 부모 되돌리기
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
  // P1에 C1 그림
  P1 := DrawRectangle(10, 10, 140, 140, 'P1');
  C1 := DrawRectangle(50, 50, 120, 120, 'C1');
  Check(C1.Parent = P1);

  // P1이동
//  TestLib.RunMousePath(MousePath.New
//  .Add(20, 20)
//  .Add(100, 100)
//  .Add(40, 40).Path);

  // C1이동(벗어나기)
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

initialization
  RegisterTest(TestTThItemGrouppingHistory.Suite);

end.
