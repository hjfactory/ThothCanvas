unit TestThItemGrouping;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #19 캔버스에 선을 추가한다.
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
  DrawRectangle(50, 50, 130, 130); // P1
  DrawRectangle(70, 70, 120, 120);

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

initialization
  RegisterTest(TestTThItemGroupping.Suite);

end.
