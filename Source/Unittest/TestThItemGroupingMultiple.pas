unit TestThItemGroupingMultiple;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #260 중복 그루핑이 처리되어야 한다.
  TestTThItemGrouppingMultiple = class(TThCanvasBaseTestUnit)
  published
    // #251 P1에 C1이 올라간 상태서 P2를 C1보다 크게 P1에 올리면 P1>P2>C1으로 그루핑된다.
    procedure TestMoveGroupingAndContain;

    // #274 그루핑은 같은 부모만 대상으로 한다.
    procedure TestGroupingSameParent;

    // #259 P1위의 P2에 C1을 올리면 P1>P2>C1으로 그루핑 된다.
    procedure TestOverlapItem;

    // #258 P1위의 P2, C1에서 C1을 P2에 그루핑 할 수 있어야 한다.
    // #266 P1위의 P2에 올라간 C1을 P1으로 이동 시 P1>P2,C1으로 그루핑 되야한다.
    procedure TestSameParentMoving;

    // #267 P1>P2>C1에서 C1을 밖으로 빼면 해당 위치로 이동되어야 한다.
    procedure TestMoveGrandParentToCanvas;

    // #270: C1, C2, C3 중복 선택 후 P1에 C1,C2만 올라갈 경우 C3 제외하고 그루핑 된다.
    // #271: P1위의 C1,C2을 중복선택 후 밖으로 빼면 모두 빠져 나와야 한다.
    // #272: P1위의 C1과 캔버스의 C2를 중복 선택 후 이동 시 C1이 P1에서 빠져나와야 한다.
    procedure TestMoveMultiselOnlyContain;

    // #273 P1위의 C1과 캔버스의 C2를 중복 선택 후 P2영역안으로 이동 시 P2>C1,C2가 되어야 한다.
    procedure TestMoveChildAndCanvasItem;

    // #269: P1>P2>C1 구조에서 3개를 선택하고 이동 시 P1에 따라 이동되어야 한다.
    procedure TestMoveMultiselected;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

procedure TestTThItemGrouppingMultiple.TestMoveGroupingAndContain;
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

procedure TestTThItemGrouppingMultiple.TestGroupingSameParent;
var
  P1, P2, C1: TThItem;
begin
  // P1위에 C1 위치
  // 세로는 P1보다 크고 가로는 P1보다 작은 P2 생성
  // P2가 C1을 포함한 영역으로 덮어도 C1이 포함되지 않아야 함
  // P2가 P1에 포함하는 경우 C1을 포함함


  P1 := DrawRectangle(100, 100, 180, 180, 'P1');
  C1 := DrawRectangle(130, 130, 160, 160, 'C1');
  Check(C1.Parent = P1, 'C1.Parent(0)');

  P2 := DrawRectangle(120, 90, 170, 190, 'P2');
  Check(C1.Parent <> P2, 'Not inrange.');

  FCanvas.DeleteSelection;
  P2 := DrawRectangle(120, 120, 170, 170, 'P2');
  Check(C1.Parent = P2, 'Inrange');
end;

procedure TestTThItemGrouppingMultiple.TestOverlapItem;
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

procedure TestTThItemGrouppingMultiple.TestSameParentMoving;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(50, 50, 250, 250);
  P2 := DrawRectangle(150, 150, 230, 230);
  C1 := DrawCircle(60, 90, 90, 120);

  Check(P2.Parent = P1, '[0] P2.Parent');
  Check(C1.Parent = P1, '[0] C1.Parent');

  TestLib.RunMousePath(MousePath.New
  .Add(85, 115)
  .Add(130, 130)
  .Add(200, 200).Path);

  Check(C1.Parent = P2, '[1] C1.Parent');

  TestLib.RunMousePath(MousePath.New
  .Add(200, 200)
  .Add(130, 130)
  .Add(85, 115).Path);

  Check(C1.Parent = P1, '[2] C1.Parent');
end;

procedure TestTThItemGrouppingMultiple.TestMoveGrandParentToCanvas;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 210, 210, 'P1');
  P2 := DrawRectangle(60, 60, 160, 160, 'P2');
  C1 := DrawCircle(90, 90, 130, 130, 'C1');
  Check(C1.Parent = P2, 'C1.Parent = P2');

  CheckEquals(P2.AbsolutePoint.X, -900);
  CheckEquals(C1.Position.X, 300);

  TestLib.RunMouseClick(110, 110);
  TestLib.RunMousePath(MousePath.New
  .Add(110, 110)
  .Add(130, 130)
  .Add(250, 250).Path);

  Check(C1.Parent <> P1, 'C1.Parent <> P1');
  Check(C1.Parent <> P2, 'C1.Parent <> P2');
  CheckEquals(C1.Position.X, 800, 4, Format('C1.Position.X = %f', [C1.Position.X]));
  // 800 = -900 + (300 + 1400)
  // X = P2.AbsX + (C1.X + Move.X)
end;

procedure TestTThItemGrouppingMultiple.TestMoveMultiselOnlyContain;
var
  P1, C1, C2, C3: TThItem;
  P_1, P2, P3: TPointF;
begin
  P1 := DrawRectangle(10, 10, 150, 290, 'P1');

  // #270: C1, C2, C3 중복 선택 후 P1에 C1,C2만 올라갈 경우 C3 제외하고 그루핑 된다.
  C1 := DrawCircle(180, 50, 210, 80, 'C1');
  C2 := DrawCircle(180, 100, 210, 130, 'C3');
  C3 := DrawCircle(230, 50, 270, 80, 'C3');

  P_1 := C1.Position.Point;
  P2 := C2.Position.Point;
  P3 := C3.Position.Point;

  TestLib.RunMouseClick(195, 65);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(195, 115);
  TestLib.RunMouseClick(250, 65);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 3, 'Select');

  TestLib.RunMousePath(MousePath.New
  .Add(195, 65)
  .Add(130, 130)
  .Add(120, 65).Path);

  Check(C1.Parent = P1, Format('[1]C1.Parent = P1 / %s', [C1.Parent.Name]));
  Check(C2.Parent = P1, Format('[1]C2.Parent = P1 / %s', [C2.Parent.Name]));
  Check(C3.Parent <> P1, Format('[1]C3.Parent <> P1 / %s', [C3.Parent.Name]));

  // #271: P1위의 C1,C2을 중복선택 후 밖으로 빼면 모두 빠져 나와야 한다.
  FCanvas.ClearSelection;

  TestLib.RunMouseClick(120, 65);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(120, 115);
  TestLib.RunMouseClick(175, 65);
  TestLib.RunKeyUpShift;

  TestLib.RunMousePath(MousePath.New
  .Add(120, 65)
  .Add(130, 130)
  .Add(195, 65).Path);

  Check(C1.Parent <> P1, Format('[2]C1.Parent = P1 / %s', [C1.Parent.Name]));
  Check(C2.Parent <> P1, Format('[2]C2.Parent = P1 / %s', [C2.Parent.Name]));
  Check(C3.Parent <> P1, Format('[2]C3.Parent <> P1 / %s', [C3.Parent.Name]));

  // #272: P1위의 C1과 캔버스의 C2를 중복 선택 후 이동 시 C1이 P1에서 빠져나와야 한다.
  Check(C1.Position.Point = P_1);
  Check(C2.Position.Point = P2);
  Check(C3.Position.Point = P3);
end;

procedure TestTThItemGrouppingMultiple.TestMoveChildAndCanvasItem;
var
  P1, C1, C2: TThItem;
begin
  P1 := DrawRectangle(10, 10, 200, 150, 'P1');
  C1 := DrawRectangle(50, 110, 80, 140, 'C1');
  C2 := DrawRectangle(100, 160, 130, 190, 'C2');

  Check(C1.Parent = P1, '[0] C1.Parent = P1');
  Check(C2.Parent <> P1, '[0] C2.Parent <> P1');

  TestLib.RunMouseClick(115, 175);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(65, 125);
  TestLib.RunKeyUpShift;

  TestLib.RunMousePath(MousePath.New
  .Add(115, 175)
  .Add(130, 130)
  .Add(100, 100).Path);

  Check(C1.Parent = P1, '[1] C1.Parent = P1');
  Check(C2.Parent = P1, '[1] C2.Parent = P1');
end;

procedure TestTThItemGrouppingMultiple.TestMoveMultiselected;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 140, 140, 'P1');
  P2 := DrawRectangle(30, 30, 110, 110, 'P2');
  C1 := DrawRectangle(50, 50, 80, 80, 'C1');

  TestLib.RunMouseClick(65, 65);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(40, 65);
  TestLib.RunMouseClick(20, 65);
  TestLib.RunKeyUpShift;

  TestLib.RunMousePath(MousePath.New
  .Add(65, 65)
  .Add(130, 130)
  .Add(165, 165).Path);

  CheckEquals(P1.Position.X, -400, 4, 'P1');
  CheckEquals(P2.AbsolutePoint.X, -200, 4, 'P2');
  CheckEquals(C1.AbsolutePoint.X, 0, 4, 'C1');

  CheckEquals(P2.Position.X, 200, 4, 'P2');
  CheckEquals(C1.Position.X, 200, 4, 'C1');
end;

initialization
  RegisterTest(TestTThItemGrouppingMultiple.Suite);

end.
