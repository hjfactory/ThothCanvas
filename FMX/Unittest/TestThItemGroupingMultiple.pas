unit TestThItemGroupingMultiple;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #260 �ߺ� �׷����� ó���Ǿ�� �Ѵ�.
  TestTThItemGrouppingMultiple = class(TThCanvasBaseTestUnit)
  published
    // #251 P1�� C1�� �ö� ���¼� P2�� C1���� ũ�� P1�� �ø��� P1>P2>C1���� �׷��εȴ�.
    procedure TestMoveGroupingAndContain;

    // #274 �׷����� ���� �θ� ������� �Ѵ�.
    procedure TestGroupingSameParent;

    // #259 P1���� P2�� C1�� �ø��� P1>P2>C1���� �׷��� �ȴ�.
    procedure TestOverlapItem;

    // #258 P1���� P2, C1���� C1�� P2�� �׷��� �� �� �־�� �Ѵ�.
    // #266 P1���� P2�� �ö� C1�� P1���� �̵� �� P1>P2,C1���� �׷��� �Ǿ��Ѵ�.
    procedure TestSameParentMoving;

    // #267 P1>P2>C1���� C1�� ������ ���� �ش� ��ġ�� �̵��Ǿ�� �Ѵ�.
    procedure TestMoveGrandParentToCanvas;

    // #270: C1, C2, C3 �ߺ� ���� �� P1�� C1,C2�� �ö� ��� C3 �����ϰ� �׷��� �ȴ�.
    // #271: P1���� C1,C2�� �ߺ����� �� ������ ���� ��� ���� ���;� �Ѵ�.
    // #272: P1���� C1�� ĵ������ C2�� �ߺ� ���� �� �̵� �� C1�� P1���� �������;� �Ѵ�.
    procedure TestMoveMultiselOnlyContain;

    // #273 P1���� C1�� ĵ������ C2�� �ߺ� ���� �� P2���������� �̵� �� P2>C1,C2�� �Ǿ�� �Ѵ�.
    procedure TestMoveChildAndCanvasItem;

    // #269: P1>P2>C1 �������� 3���� �����ϰ� �̵� �� P1�� ���� �̵��Ǿ�� �Ѵ�.
    procedure TestMoveMultiselected;

    // #284 P1>C1���� P1�� �̵��Ͽ� P2�� C1�� �����ϴ� ��� P2�� C1�� ���ԵǾ� �Ѵ�.
    procedure TestMoveContainChildOfParent;
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
  CheckEquals(P2.Position.X, 10, 3, Format('P2.X : %f', [P2.Position.X]));
  CheckEquals(C1.Position.X, 20, 3, Format('C1.X : %f', [C1.Position.X]));
end;

procedure TestTThItemGrouppingMultiple.TestGroupingSameParent;
var
  P1, P2, C1: TThItem;
begin
  // P1���� C1 ��ġ
  // ���δ� P1���� ũ�� ���δ� P1���� ���� P2 ����
  // P2�� C1�� ������ �������� ��� C1�� ���Ե��� �ʾƾ� ��
  // P2�� P1�� �����ϴ� ��� C1�� ������


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
  CheckEquals(C1.Position.X, 40, Format('C1.Position.X = %f', [C1.Position.X]));
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

  CheckEquals(P2.AbsolutePoint.X, -90);
  CheckEquals(C1.Position.X, 30);

  TestLib.RunMouseClick(110, 110);
  TestLib.RunMousePath(MousePath.New
  .Add(110, 110)
  .Add(130, 130)
  .Add(250, 250).Path);

  Check(C1.Parent <> P1, 'C1.Parent <> P1');
  Check(C1.Parent <> P2, 'C1.Parent <> P2');
  CheckEquals(C1.Position.X, 80, 4, Format('C1.Position.X = %f', [C1.Position.X]));
  // 800 = -900 + (300 + 1400)
  // X = P2.AbsX + (C1.X + Move.X)
end;

procedure TestTThItemGrouppingMultiple.TestMoveMultiselOnlyContain;
var
  P1, C1, C2, C3: TThItem;
  P_1, P2, P3: TPointF;
begin
  P1 := DrawRectangle(10, 10, 150, 290, 'P1');

  // #270: C1, C2, C3 �ߺ� ���� �� P1�� C1,C2�� �ö� ��� C3 �����ϰ� �׷��� �ȴ�.
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

  // #271: P1���� C1,C2�� �ߺ����� �� ������ ���� ��� ���� ���;� �Ѵ�.
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

  // #272: P1���� C1�� ĵ������ C2�� �ߺ� ���� �� �̵� �� C1�� P1���� �������;� �Ѵ�.
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

  CheckEquals(P1.Position.X, -40, 4, 'P1');
  CheckEquals(P2.AbsolutePoint.X, -20, 4, 'P2');
  CheckEquals(C1.AbsolutePoint.X, 0, 4, 'C1');

  CheckEquals(P2.Position.X, 20, 4, 'P2');
  CheckEquals(C1.Position.X, 20, 4, 'C1');
end;

procedure TestTThItemGrouppingMultiple.TestMoveContainChildOfParent;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 140, 140, 'P1');
  C1 := DrawRectangle(50, 50, 80, 80, 'C1');

  P2 := DrawRectangle(160, 30, 220, 100, 'P2');

  Check(C1.Parent = P1, '[0] C1.Parent = P1');

  TestLib.RunMouseClick(20, 15);
  TestLib.RunMousePath(MousePath.New[20, 20][130,130][150,20]);

  Check(P2.Parent = P1, '[1] P2.Parent');
  Check(C1.Parent = P2, '[1] C1.Parent');
end;

initialization
  RegisterTest(TestTThItemGrouppingMultiple.Suite);

end.
