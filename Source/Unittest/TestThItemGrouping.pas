unit TestThItemGrouping;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #252 겹친 아이템은 그루핑되어야 한다.
  // #253 겹친 부분 해제되는 경우 그루핑이 풀려야 한다.
  TestTThItemGroupping = class(TThCanvasBaseTestUnit)
  published
    // 기능 테스트
    procedure TestGetAbsolutePoint;

    // #250 P1에 C1을 드루윙하면 그루핑 되어야 한다.
    procedure TestDrawToParent;

    // #283 C1을 포함하여 그리는 경우 C1이 포함되어야 한다.
    procedure TestDrawOverChildRange;

    // #244 P1이 C1을 포함하는 영역으로 이동하는 경우 C1이 그룹핑 되어야 한다.
    procedure TestP1MoveOuterC1;

    // #235 C1이 P1의 영역으로 이동 시 그룹핑 된다.
    procedure TestC1MoveInnerP1;

    // #242 그루핑 이후에 동일한 절대 경로에 위치해야한다.
    procedure TestGroupingAbsPos;
    procedure TestGroupingAbsPos2;

    // #246 부모영역에 온전히 포함된 경우만 그루핑되어야 한다.
    procedure TestCotainRangeTopLeft; // (1/4)
    procedure TestCotainRangeTopLeft2; // (99% contain)

    // #248 선안에는 아이템이 포함되지 않아야 한다.
    procedure TestDecontainLineItem;

    // #236 C1이 P1의 영역밖으로 이동 시 그루핑이 해제된다.
    procedure TestMoveOutDegrouping;

    // #243 그루핑 해제 이후 동일한 절대 위치에 존재해야 한다.
    procedure TestDegroupingSameAbsPoint;

    // #245 P1에 포함된 C1이 P1내에서 이동 시 위치가 추가 이동되는 버그
    procedure BugMoveAtAddPointFromSameParent;

    // #238 P1 크기를 변경경하여 C1을 포함한 경우 그루핑 된다.
    procedure TestGroupingFromResize;

    // #240 C2를 포함한 C1을 P1으로 이동 시 P1과 그루핑되어야 한다.
    procedure TestMoveGroupingGroupItem;

    // #241 C2를 포함한 C1을 P1영역에서 밖으로 이동하는 경우 P1과 그루핑이 해제되야한다.
    procedure TestMoveGroupingGroupItemRelease;

    // #239 C1크기변경하여 P1의 영역을 넘어서는 경우 그루핑 해제된다.
    procedure TestResizeReleaseChild;
    // #257 P1의 크기를 C1보다 작게 조정하는 경우 그루핑이 해제되야 한다.
    procedure TestResizeReleaseParent;

    // #237 C1이 P1에서 P2의 영역으로 이동 시 그뤂핑이 재설정 된다.
    procedure TestChangeGrouping;

    // #268 이미지 위에 아이템이 그루핑 되어야 한다.
    procedure TestImageGrouping;

    // #261 아이템크기를 음수영역(TopLeft)으로 변경하는 경우 자식아이템 위치는 그대로여야 한다.
    procedure TestResizeTopLeftChildAbsolutePoint;

    // #295 그룹된 아이템 크기 변경 시 BottomRight를 TopLeft로 이동 시 자식의 위치가 엉뚱하게 표시 됨
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
  CheckEquals(P.X, -500,  'D1.X');
  CheckEquals(P.Y, -300,  'D1.Y');

  P := D2.AbsolutePoint;
//  Check(D2.Parent = D1, Format('D2.Parent = %s(%s)', [D2.Parent.Name, D2.Parent.ClassName]));
  CheckEquals(P.X, -300,    'D2.X');
  CheckEquals(P.Y, -100,  'D2.Y');

  P := D3.AbsolutePoint;
//  Check(D3.Parent = D2, Format('D3.Parent = %s(%s)', [D3.Parent.Name, D3.Parent.ClassName]));
  CheckEquals(P.X, 100,  'D3.X');
  CheckEquals(P.Y, 400,  'D3.Y');
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
  CheckEquals(C1.Position.X, 300);
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

  CheckEquals(C1.Position.X, -200);

  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(130, 130);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
  Check(C1.Parent = P1, Format('C1.Parent : %s', [C1.Parent.ClassName]));

  CheckEquals(C1.Position.X, -200 - P1.Position.X, 0.00001);
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

  CheckEquals(Round(C1.Position.X), Round(-200 - P1.Position.X), 0.00001);
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
  CheckEquals(C1.Position.X, 500);

  TestLib.RunMousePath(MousePath.New
  .Add(110, 110)
  .Add(100, 100)
  .Add(120, 120).Path);

  CheckEquals(C1.Position.X, 600);
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
  // C2를 그린다
  C2 := DrawRectangle(190, 190, 270, 270);
  // C2위에 C1을 그린다.
  C1 := DrawRectangle(200, 200, 250, 250);
  Check(C1.Parent = C2, 'C1.Parent');

  // P1을 그린다.
  P1 := DrawRectangle(10, 10, 150, 150);

  // C2를 P1에 올린다.
  TestLib.RunMousePath(MousePath.New
  .Add(195, 195)
  .Add(100, 100)
  .Add(20, 20).Path);

  // C2의 부모가 P1임을 확인한다.
  Check(C2.Parent = P1, 'C2.Parent');

  // C2의 자식이 C1임을 확인한다.
  Check(C1.Parent = C2, 'C1.Parent2');
end;

procedure TestTThItemGroupping.TestMoveGroupingGroupItemRelease;
var
  P1, C2: TThItem;
begin
  // C2를 그린다
  C2 := DrawRectangle(190, 190, 270, 270);
  // C2위에 C1을 그린다.
  DrawRectangle(220, 220, 260, 260);

  // P1을 그린다.
  P1 := DrawRectangle(10, 10, 150, 150);

  // C2를 P1에 올린다.
  TestLib.RunMousePath(MousePath.New
  .Add(195, 195)
  .Add(100, 100)
  .Add(20, 20).Path);

  // C2의 부모가 P1임을 확인한다.
  Check(C2.Parent = P1, 'C2.Parent');

  // C2를 빈곳으로 이동한다.
  TestLib.RunMousePath(MousePath.New
  .Add(30, 30)
  .Add(100, 100)
  .Add(190, 190).Path);

  // C2의 부모가 P1이 아닌 것을 확인
  Check(C2.Parent <> P1, 'C2');
end;

procedure TestTThItemGroupping.TestResizeReleaseChild;
var
  P1, C1: TThItem;
begin
  // P1 그리기
  P1 := DrawRectangle(10, 10, 110, 110, 'P1');

  // C1 그리기
  C1 := DrawRectangle(40, 40, 80, 80, 'C1');

  // C1을 P1이 넘도록 크기 변경
  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(100, 100)
  .Add(190, 100).Path);

  // C1의 부모 확인
  Check(C1.Parent <> P1, 'Parent check');
end;

procedure TestTThItemGroupping.TestResizeReleaseParent;
var
  P1, C1: TThItem;
begin
  // P1 그리기
  P1 := DrawRectangle(10, 10, 200, 200, 'P1');

  // C1 그리기
  C1 := DrawRectangle(40, 40, 150, 150, 'C1');

  // C1을 P1이 넘도록 크기 변경
  TestLib.RunMouseClick(195, 195);
  TestLib.RunMousePath(MousePath.New
  .Add(200, 200)
  .Add(100, 100)
  .Add(100, 100).Path);

  // C1의 부모 확인
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
  CheckEquals(C1.Position.X, 100);

  TestLib.RunMousePath(MousePath.New
  .Add(30, 30)
  .Add(100, 30)
  .Add(170, 30).Path);

  Check(C1.Parent = P2, 'Parent is P2');
  CheckEquals(C1.Position.X, 100);
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
