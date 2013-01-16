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

    // #250 P1에 C1을 드루윙하면 그루핑 되어야 한다.
    procedure TestParentDrawing;

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

    // #259 P1위의 P2에 C1을 올리면 P1>P2>C1으로 그루핑 된다.
    procedure TestOverlapItem;

    // #251 P1에 C1이 올라간 상태서 P2를 C1보다 크게 P1에 올리면 P1>P2>C1으로 그루핑된다.
    procedure TestMoveGroupingAndContain;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThItemGroupping }

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

procedure TestTThItemGroupping.TestParentDrawing;
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
begin
  DrawRectangle(50, 50, 130, 130); // P1
  DrawRectangle(70, 70, 120, 120);

  CheckEquals(FCanvas.ItemCount, 1);

  TestLib.RunMousePath(MousePath.New
  .Add(80, 80)
  .Add(180, 180)
  .Add(160, 160).Path);

  CheckEquals(FCanvas.ItemCount, 2, 'ItemCount');
  CheckNotNull(FCanvas.SelectedItem);
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

//  TestLib.RunMouseClick(FCanvas.CenterPoint);
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

procedure TestTThItemGroupping.TestOverlapItem;
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

procedure TestTThItemGroupping.TestMoveGroupingAndContain;
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

initialization
  RegisterTest(TestTThItemGroupping.Suite);

end.
