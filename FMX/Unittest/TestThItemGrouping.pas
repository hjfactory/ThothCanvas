unit TestThItemGrouping;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #252 ��ģ �������� �׷��εǾ�� �Ѵ�.
  // #253 ��ģ �κ� �����Ǵ� ��� �׷����� Ǯ���� �Ѵ�.
  TestTThItemGroupping = class(TThCanvasBaseTestUnit)
  published
    // ��� �׽�Ʈ
    procedure TestGetAbsolutePoint;

    // #250 P1�� C1�� ������ϸ� �׷��� �Ǿ�� �Ѵ�.
    procedure TestDrawToParent;

    // #283 C1�� �����Ͽ� �׸��� ��� C1�� ���ԵǾ�� �Ѵ�.
    procedure TestDrawOverChildRange;

    // #244 P1�� C1�� �����ϴ� �������� �̵��ϴ� ��� C1�� �׷��� �Ǿ�� �Ѵ�.
    procedure TestP1MoveOuterC1;

    // #235 C1�� P1�� �������� �̵� �� �׷��� �ȴ�.
    procedure TestC1MoveInnerP1;

    // #242 �׷��� ���Ŀ� ������ ���� ��ο� ��ġ�ؾ��Ѵ�.
    procedure TestGroupingAbsPos;
    procedure TestGroupingAbsPos2;

    // #246 �θ𿵿��� ������ ���Ե� ��츸 �׷��εǾ�� �Ѵ�.
    procedure TestCotainRangeTopLeft; // (1/4)
    procedure TestCotainRangeTopLeft2; // (99% contain)

    // #248 ���ȿ��� �������� ���Ե��� �ʾƾ� �Ѵ�.
    procedure TestDecontainLineItem;

    // #236 C1�� P1�� ���������� �̵� �� �׷����� �����ȴ�.
    procedure TestMoveOutDegrouping;

    // #243 �׷��� ���� ���� ������ ���� ��ġ�� �����ؾ� �Ѵ�.
    procedure TestDegroupingSameAbsPoint;

    // #245 P1�� ���Ե� C1�� P1������ �̵� �� ��ġ�� �߰� �̵��Ǵ� ����
    procedure BugMoveAtAddPointFromSameParent;

    // #238 P1 ũ�⸦ ������Ͽ� C1�� ������ ��� �׷��� �ȴ�.
    procedure TestGroupingFromResize;

    // #240 C2�� ������ C1�� P1���� �̵� �� P1�� �׷��εǾ�� �Ѵ�.
    procedure TestMoveGroupingGroupItem;

    // #241 C2�� ������ C1�� P1�������� ������ �̵��ϴ� ��� P1�� �׷����� �����Ǿ��Ѵ�.
    procedure TestMoveGroupingGroupItemRelease;

    // #239 C1ũ�⺯���Ͽ� P1�� ������ �Ѿ�� ��� �׷��� �����ȴ�.
    procedure TestResizeReleaseChild;
    // #257 P1�� ũ�⸦ C1���� �۰� �����ϴ� ��� �׷����� �����Ǿ� �Ѵ�.
    procedure TestResizeReleaseParent;

    // #237 C1�� P1���� P2�� �������� �̵� �� �׏����� �缳�� �ȴ�.
    procedure TestChangeGrouping;

    // #268 �̹��� ���� �������� �׷��� �Ǿ�� �Ѵ�.
    procedure TestImageGrouping;

    // #261 ������ũ�⸦ ��������(TopLeft)���� �����ϴ� ��� �ڽľ����� ��ġ�� �״�ο��� �Ѵ�.
    procedure TestResizeTopLeftChildAbsolutePoint;

    // #295 �׷�� ������ ũ�� ���� �� BottomRight�� TopLeft�� �̵� �� �ڽ��� ��ġ�� �����ϰ� ǥ�� ��
    procedure TestResizeRightToLeftChildMove;
    procedure TestResizeRightToLeftAfterNearLeft;
    procedure TestResizeRightToLeftNearLeft;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThItemGroupping }

procedure TestTThItemGroupping.TestGetAbsolutePoint;
var
  D1, D2, D3: TThItem;
  P: TPointF;
begin
  D1 := DrawRectangle(100, 120, 250, 250, 'D1');
  D2 := DrawRectangle(120, 140, 200, 240, 'D2');
  D3 := DrawRectangle(160, 190, 190, 220, 'D3');

  P := D1.AbsolutePoint;
  CheckEquals(P.X, -50,  'D1.X');
  CheckEquals(P.Y, -30,  'D1.Y');

  P := D2.AbsolutePoint;
//  Check(D2.Parent = D1, Format('D2.Parent = %s(%s)', [D2.Parent.Name, D2.Parent.ClassName]));
  CheckEquals(P.X, -30,    'D2.X');
  CheckEquals(P.Y, -10,  'D2.Y');

  P := D3.AbsolutePoint;
//  Check(D3.Parent = D2, Format('D3.Parent = %s(%s)', [D3.Parent.Name, D3.Parent.ClassName]));
  CheckEquals(P.X, 10,  'D3.X');
  CheckEquals(P.Y, 40,  'D3.Y');
end;

procedure TestTThItemGroupping.TestDrawToParent;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(50, 50, 250, 250); // P1
  P1.Name := 'P1';
  DrawRectangle(100, 100, 200, 200);
  TestLib.RunMouseClick(150, 150);
  C1 := FCanvas.SelectedItem;
  C1.Name := 'C1';

  CheckEquals(FCanvas.ItemCount, 1);
  Check(C1.Parent = P1, C1.Parent.ClassName);
end;

procedure TestTThItemGroupping.TestDrawOverChildRange;
var
  P1, C1: TThItem;
begin
  C1 := DrawRectangle(40, 40, 110, 110, 'C1');
  P1 := DrawRectangle(10, 10, 150, 150, 'P1');

  Check(C1.Parent = P1, 'parent');
  CheckEquals(C1.Position.X, 30);
end;

procedure TestTThItemGroupping.TestP1MoveOuterC1;
begin
  DrawRectangle(0, 0, 100, 100); // P1
  DrawRectangle(130, 130, 170, 170);   // C1

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(130, 130);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(150, 150);
  CheckNotNull(FCanvas.SelectedItem);
end;

procedure TestTThItemGroupping.TestC1MoveInnerP1;
begin
  DrawRectangle(10, 10, 50, 50);   // C1
  DrawRectangle(100, 100, 200, 200); // P1

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
end;

procedure TestTThItemGroupping.TestGroupingAbsPos;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(0, 0, 100, 100); // P1
  C1 := DrawRectangle(130, 130, 170, 170);   // C1

  CheckEquals(C1.Position.X, -20);

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(130, 130);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
  Check(C1.Parent = P1, Format('C1.Parent : %s', [C1.Parent.ClassName]));

  CheckEquals(C1.Position.X, -20 - P1.Position.X, 0.00001);
end;

procedure TestTThItemGroupping.TestGroupingAbsPos2;
var
  P1, C1: TThItem;
begin
  C1 := DrawRectangle(10, 10, 50, 50);   // C1
  P1 := DrawRectangle(100, 100, 200, 200); // P1

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
  Check(C1.Parent = P1, Format('C1.Parent : %s', [C1.Parent.ClassName]));

  CheckEquals(Round(C1.Position.X), Round(-20 - P1.Position.X), 0.00001);
end;

procedure TestTThItemGroupping.TestCotainRangeTopLeft;
var
  P1, C1: TThItem;
begin
  C1 := DrawRectangle(10, 10, 50, 50);   // C1
  P1 := DrawRectangle(100, 100, 200, 200); // P1

  C1.Name := 'C1'; P1.Name := 'P1';

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  Check(C1.Parent <> P1, Format('C1.Parent : %s', [C1.Parent.ClassName]));
end;

procedure TestTThItemGroupping.TestCotainRangeTopLeft2;
var
  P1, C1: TThItem;
begin
  C1 := DrawRectangle(10, 10, 50, 50);   // C1
  P1 := DrawRectangle(100, 100, 200, 200); // P1

  C1.Name := 'C1'; P1.Name := 'P1';

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(119, 119);
  TestLib.RunMousePath(MousePath.Path);

  Check(C1.Parent <> P1, Format('C1.Parent : %s', [C1.Parent.ClassName]));
end;

procedure TestTThItemGroupping.TestDecontainLineItem;
begin
  DrawLine(50, 50, 250, 250);
  DrawRectangle(100, 100, 200, 200);

  CheckEquals(FCanvas.ItemCount, 2);
end;

procedure TestTThItemGroupping.TestMoveOutDegrouping;
begin
  DrawRectangle(50, 50, 130, 130, 'P1'); // P1
  DrawRectangle(70, 70, 120, 120, 'C1');

  CheckEquals(FCanvas.ItemCount, 1);

  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(180, 180)
  .Add(120, 120).Path);

  CheckEquals(FCanvas.ItemCount, 2);
end;

procedure TestTThItemGroupping.TestDegroupingSameAbsPoint;
var
  C1: TThItem;
begin
  DrawRectangle(50, 50, 130, 130, 'P1'); // P1
  C1 := DrawRectangle(70, 70, 120, 120, 'C1');

  CheckEquals(FCanvas.ItemCount, 1);

  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(180, 180)
  .Add(160, 160).Path);

  CheckEquals(FCanvas.ItemCount, 2, 'ItemCount');
  TestLib.RunMouseClick(160, 160);
  Check(FCanvas.SelectedItem = C1, 'Not selected C1');
  CheckEquals(FCanvas.SelectedItem.Position.X, 0, 4, 'Zero point');
end;

procedure TestTThItemGroupping.BugMoveAtAddPointFromSameParent;
var
  C1: TThItem;
begin
  DrawRectangle(50, 50, 250, 250); // P1
  DrawRectangle(100, 100, 200, 200);

  TestLib.RunMouseClick(150, 150);
  C1 := FCanvas.SelectedItem;

  CheckNotNull(C1);
  CheckEquals(C1.Position.X, 50);

  TestLib.RunMousePath(MousePath.New
  .Add(110, 110)
  .Add(100, 100)
  .Add(120, 120).Path);

  CheckEquals(C1.Position.X, 60);
end;

procedure TestTThItemGroupping.TestGroupingFromResize;
begin
  DrawRectangle(70, 70, 120, 120);
  DrawRectangle(50, 50, 110, 110); // P1

  CheckEquals(FCanvas.ItemCount, 2);

  TestLib.RunMousePath(MousePath.New
  .Add(110, 110)
  .Add(100, 100)
  .Add(130, 130).Path);

  CheckEquals(FCanvas.ItemCount, 1);
end;

procedure TestTThItemGroupping.TestMoveGroupingGroupItem;
var
  P1, C1, C2: TThItem;
begin
  // C2�� �׸���
  C2 := DrawRectangle(190, 190, 270, 270);
  // C2���� C1�� �׸���.
  C1 := DrawRectangle(200, 200, 250, 250);
  Check(C1.Parent = C2, 'C1.Parent');

  // P1�� �׸���.
  P1 := DrawRectangle(10, 10, 150, 150);

  // C2�� P1�� �ø���.
  TestLib.RunMousePath(MousePath.New
  .Add(195, 195)
  .Add(100, 100)
  .Add(20, 20).Path);

  // C2�� �θ� P1���� Ȯ���Ѵ�.
  Check(C2.Parent = P1, 'C2.Parent');

  // C2�� �ڽ��� C1���� Ȯ���Ѵ�.
  Check(C1.Parent = C2, 'C1.Parent2');
end;

procedure TestTThItemGroupping.TestMoveGroupingGroupItemRelease;
var
  P1, C2: TThItem;
begin
  // C2�� �׸���
  C2 := DrawRectangle(190, 190, 270, 270);
  // C2���� C1�� �׸���.
  DrawRectangle(220, 220, 260, 260);

  // P1�� �׸���.
  P1 := DrawRectangle(10, 10, 150, 150);

  // C2�� P1�� �ø���.
  TestLib.RunMousePath(MousePath.New
  .Add(195, 195)
  .Add(100, 100)
  .Add(20, 20).Path);

  // C2�� �θ� P1���� Ȯ���Ѵ�.
  CheckNotNull(C2);
  Check(C2.Parent = P1, 'C2.Parent');

  // C2�� ������� �̵��Ѵ�.
  TestLib.RunMousePath(MousePath.New
  .Add(30, 30)
  .Add(100, 100)
  .Add(190, 190).Path);

  // C2�� �θ� P1�� �ƴ� ���� Ȯ��
  Check(C2.Parent <> P1, 'C2');
end;

procedure TestTThItemGroupping.TestResizeReleaseChild;
var
  P1, C1: TThItem;
begin
  // P1 �׸���
  P1 := DrawRectangle(10, 10, 110, 110, 'P1');

  // C1 �׸���
  C1 := DrawRectangle(40, 40, 80, 80, 'C1');

  // C1�� P1�� �ѵ��� ũ�� ����
  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(100, 100)
  .Add(190, 100).Path);

  // C1�� �θ� Ȯ��
  Check(C1.Parent <> P1, 'Parent check');
end;

procedure TestTThItemGroupping.TestResizeReleaseParent;
var
  P1, C1: TThItem;
begin
  // P1 �׸���
  P1 := DrawRectangle(10, 10, 200, 200, 'P1');

  // C1 �׸���
  C1 := DrawRectangle(40, 40, 150, 150, 'C1');

  // C1�� P1�� �ѵ��� ũ�� ����
  TestLib.RunMouseClick(195, 195);
  TestLib.RunMousePath(MousePath.New
  .Add(200, 200)
  .Add(100, 100)
  .Add(100, 100).Path);

  // C1�� �θ� Ȯ��
  Check(C1.Parent <> P1, 'Parent check');
end;

procedure TestTThItemGroupping.TestChangeGrouping;
var
  P1, P2, C1: TThItem;
begin
  P1 := DrawRectangle(10, 10, 120, 120);
  P2 := DrawRectangle(150, 10, 290, 120);

  C1 := DrawRectangle(20, 20, 80, 80);

  Check(C1.Parent = P1, 'Parent is P1');
  CheckEquals(C1.Position.X, 10);

  TestLib.RunMousePath(MousePath.New
  .Add(30, 30)
  .Add(100, 30)
  .Add(170, 30).Path);

  Check(C1.Parent = P2, 'Parent is P2');
  CheckEquals(C1.Position.X, 10);
end;

procedure TestTThItemGroupping.TestImageGrouping;
var
  TestImagePath: string;
  P1, C1: TThItem;
  R: TRectF;
begin
  TestImagePath := GetImagePath;
  FCanvas.AppendFileItem(ItemFactoryIDImageFile, TestImagePath);

  TestLib.RunMouseClick(150, 150);
  P1 := FCanvas.SelectedItem;
  CheckNotNull(P1);

  R := P1.AbsoluteRect;
  R.Offset(-FCanvas.Position.X, -FCanvas.Position.Y);
  R.Offset(10, 10);
  R.Width := R.Width - 20;
  R.Height := R.Height - 20;

  C1 := DrawRectangle(R);

  Check(C1.Parent = P1);
end;

procedure TestTThItemGroupping.TestResizeTopLeftChildAbsolutePoint;
var
  P1, C1: TThItem;
  P: TPointF;
begin
  P1 := DrawRectangle(110, 110, 240, 240, 'P1');
  C1 := DrawRectangle(150, 150, 220, 220, 'C1');

  P := C1.AbsolutePoint;
  CheckEquals(P.X, 0, Format('[0] P.X = %f', [P.X]));

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New[110, 110][100,100][50,50]);

  CheckEquals(P.X, C1.AbsolutePoint.X, Format('[0] C1.AbsolutePoint.X = %f', [C1.AbsolutePoint.X]));
end;

procedure TestTThItemGroupping.TestResizeRightToLeftChildMove;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 240, 240, 'P1');
  C1 := DrawRectangle(150, 150, 220, 220, 'C1');

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[0]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[0]Y');

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New[240, 240][100,100][50,50]);

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[1]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[1]Y');

  TestLib.RunMousePath(MousePath.New[50, 50][120, 120][240,240]);
  TestLib.RunMousePath(MousePath.New[240, 240][100,100][120, 120][50,50]);

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[2]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[2]Y');
end;

procedure TestTThItemGroupping.TestResizeRightToLeftAfterNearLeft;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 240, 240, 'P1');
  C1 := DrawRectangle(150, 150, 220, 220, 'C1');

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[0]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[0]Y');

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New[240, 240]
    [100,100][50,50]
    [100, 100]
  );

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[1]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[1]Y');
end;

procedure TestTThItemGroupping.TestResizeRightToLeftNearLeft;
var
  P1, C1: TThItem;
begin
  P1 := DrawRectangle(110, 110, 240, 240, 'P1');
  C1 := DrawRectangle(150, 150, 220, 220, 'C1');

  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[0]X');
  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[0]Y');

  TestLib.RunMouseClick(120, 120);
  TestLib.RunMousePath(MousePath.New[240, 240]
//    [150, 240][111, 240]
    [90, 240][100, 240]
  );

  PrintLog;

  Check(C1.Parent <> P1);
  CheckEquals(C1.AbsolutePoint.X, 0, 4, '[1]X');
//  CheckEquals(C1.AbsolutePoint.Y, 0, 4, '[1]Y');
end;

initialization
  RegisterTest(TestTThItemGroupping.Suite);

end.
