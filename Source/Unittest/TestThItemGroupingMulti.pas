unit TestThItemGroupingMulti;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #260 중복 그루핑이 처리되야 한다.
  // #256 그루핑 예외 및 기타
  TestTThItemGrouppingMulti = class(TBaseCommandHistoryTestUnit)
  published
    // #259 P1위의 P2에 C1을 올리면 P1>P2>C1으로 그루핑 된다.
    procedure TestOverlapItem;

    // #251 P1에 C1이 올라간 상태서 P2를 C1보다 크게 P1에 올리면 P1>P2>C1으로 그루핑된다.
    procedure TestMoveGroupingAndContain;

    // #265 Undo/Redo 시 부모가 원복 되어야 한다.
    procedure TestCmdHistoryRecoveryParent_Move;
    procedure TestCmdHistoryRecoveryParent_Add;
    procedure TestCmdHistoryRecoveryParent_DeleteChild;
    procedure TestCmdHistoryRecoveryParent_DeleteParent;
    procedure TestCmdHistoryRecoveryParent_Resize;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThItemGrouppingMulti }

procedure TestTThItemGrouppingMulti.TestOverlapItem;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 200, 200, 'P1');
  P2 := DrawCircle(30, 30, 180, 180, 'P2');
  C1 := DrawRectangle(70, 70, 120, 120, 'C1');

  Check(P2.Parent = P1, Format('P2 parent is %s(not P1)', [P2.Parent.Name]));
  Check(C1.Parent = P2, Format('C1 parent is %s(not P2)', [C1.Parent.Name]));
  CheckEquals(C1.Position.X, 400, Format('C1.Position.X = %f', [C1.Position.X]));
end;

procedure TestTThItemGrouppingMulti.TestMoveGroupingAndContain;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  P2 := DrawRectangle(160, 20, 260, 140, 'P2');
  C1 := DrawRectangle(40, 40, 110, 110, 'C1');

  TestLib.RunMouseClick(170, 30);
  TestLib.RunMousePath(MousePath.New
  .Add(180, 30)
  .Add(100, 30)
  .Add(40, 30).Path);

  Check(P1.ItemCount = 1);
  Check(C1.Parent = P2, C1.Parent.Name);
  CheckEquals(P2.Position.X, 100, 3, Format('P2.X : %f', [P2.Position.X]));
  CheckEquals(C1.Position.X, 200, 3, Format('C1.X : %f', [C1.Position.X]));
end;

procedure TestTThItemGrouppingMulti.TestCmdHistoryRecoveryParent_Move;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(160, 160, 210, 210, 'C1');

  TestLib.RunMousePath(MousePath.New
  .Add(180, 180)
  .Add(100, 30)
  .Add(30, 30).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  Check(C1.Parent <> P1, Format('Undo: %s', [C1.Parent.Name]));

  FThothController.Redo;

  Check(C1.Parent = P1, Format('Redo: %s', [C1.Parent.Name]));
end;

procedure TestTThItemGrouppingMulti.TestCmdHistoryRecoveryParent_Add;
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

procedure TestTThItemGrouppingMulti.TestCmdHistoryRecoveryParent_DeleteChild;
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

procedure TestTThItemGrouppingMulti.TestCmdHistoryRecoveryParent_DeleteParent;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');
  C1 := DrawRectangle(30, 30, 130, 130, 'C1');

  Check(C1.Parent = P1, 'Contain');

  TestLib.RunMouseClick(20, 20);
  FCanvas.DeleteSelection;

  // Parent 지우면 자식도 지워짐
  Check(not Assigned(P1.Parent), Format('P1.Parent is not null', []));

  FThothController.Undo;

  Check(Assigned(P1.Parent), Format('Undo: %s', [P1.Parent.Name]));
  Check(C1.Parent = P1, Format('Undo>C1.Parent: %s', [P1.Parent.Name]));

  FThothController.Redo;

  Check(not Assigned(P1.Parent), Format('Redo> P1.Parent is not null', []));
end;

procedure TestTThItemGrouppingMulti.TestCmdHistoryRecoveryParent_Resize;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 250, 250, 'P1');
  C1 := DrawRectangle(130, 130, 290, 290, 'C1');

  Check(C1.Parent <> P1, 'Not contain');

  TestLib.RunMouseClick(200, 200);
  TestLib.RunMousePath(MousePath.New
  .Add(290, 290)
  .Add(200, 200)
  .Add(230, 230).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  Check(C1.Parent <> P1, 'Undo> Not contain');
  CheckEquals(C1.Position.X, 200, Format('C1.Position.X : %f', [C1.Position.X]));
Check(False);
  FThothController.Redo;

  Check(C1.Parent = P1, 'Redo> Contain');
end;

initialization
  RegisterTest(TestTThItemGrouppingMulti.Suite);

end.
