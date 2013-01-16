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
    procedure TestRecoveryItemIndex_Move;
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
  Check(C1.Position.Point = C1P_O);

  FThothController.Redo;

  Check(C1.Parent = P1, Format('Redo: %s', [C1.Parent.Name]));
  Check(C1.Position.Point = C1P_N);
end;

procedure TestTThItemGrouppingHistory.TestMoveParent;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 100, 100, 'P1');
  C1 := DrawRectangle(110, 110, 160, 160, 'C1');

  // P1으로 C1 덮기
  TestLib.RunMouseClick(20, 20);
  TestLib.RunMousePath(MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(130, 130).Path);

  Check(C1.Parent = P1, 'Contain');

  FThothController.Undo;

  // C1의 위치, 부모 확인
  TestLib.RunMouseClick(150, 150);
  Check(FCanvas.SelectedItem = C1);
  Check(C1.Parent <> P1, Format('C1.Parent is %s(Not Parent <> P1)', [C1.Parent.Name]));

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

  Check(C1.Parent <> P1, 'Undo> Not contain');
  CheckEquals(C1.Position.X, -200, Format('Undo> C1.Position.X : %f', [C1.Position.X]));

  FThothController.Redo;

  Check(C1.Parent = P1, 'Redo> Contain');
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

procedure TestTThItemGrouppingHistory.TestRecoveryItemIndex_Move;
var
  P1, P2, C1: TThItem;
begin
  // C1위에 P2를 겹치게 그리고
  // C1을 P1에 올린 후 Undo 시 C1이 그대로 P2아래에 있어야 한다.

  P1 := DrawRectangle(10, 10, 130, 130, 'P1');
  C1 := DrawRectangle(150, 150, 200, 200, 'C1');
  P2 := DrawRectangle(170, 170, 250, 250, 'P2');

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
end;

initialization
  RegisterTest(TestTThItemGrouppingHistory.Suite);

end.
