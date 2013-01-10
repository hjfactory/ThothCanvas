unit TestThCanvasEditor;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #22 캔버스를 자유롭게 이동한다.
  TestTThCanvasEditor = class(TBaseTestUnit)
  published
    // #50 드래그하여 캔버스를 이동할 수 있어야 한다.
    procedure TestTracking;

    // #44 음수영역의 좌표로도 이동가능해야 한다.
    procedure TestTrackingMinusArea;

    // #51 영역밖으로 드래그 시 캔버스 이동이 계속 되어야 한다.
    procedure TestTrackingOutOfArea;

    // #59 Tracking으로 캔버스 영역을 벗어나면 도형등이 표시되지 않음
    procedure TestTrackingOutOfCanvasAreaNotDisplayed;

    // #49 캔버스를 클릭하면 (도형)선택이 취소되어야 한다.
    procedure TestUnselectingItem;

// #180 캔버스 이동 완료 시 애니메이션 효과를 주어 약간 더 흘러가게 한다.
    // #187 캔버스를 클릭하여 이동 후 마우스를 놓으면 마지막 이동거리 * 지정횟수 만큼 움직여야 한다.
    procedure TestCanvasTrackingAnimation;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor,
  ThItem, ThShapeItem, ThItemFactory, FMX.Forms, ThConsts;

procedure TestTThCanvasEditor.TestTracking;
begin
//  ShowForm;

//  DrawRectangle(50, 200, 100, 250);

  FCanvas.TrackAnimated := False;
  MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(100, 150)
  .Add(150, 150)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  Check(
    // Canvas의 중심이 0.0이므로 초기 ViewPortPosition.X는 150
    // 가로로 150 이동하여 최종 300
    (FCanvas.ViewPortPosition.X = 300) and (FCanvas.ViewPortPosition.X = 300)
    , Format('FCanvas.Postion : %f, %f', [FCanvas.ViewPortPosition.X, FCanvas.ViewPortPosition.X])
  );
end;

procedure TestTThCanvasEditor.TestTrackingMinusArea;
begin
  MousePath.New
  .Add(150, 150)
  .Add(100, 100)
  .Add(100, 150)
  .Add(150, 100)
  .Add(0, 0);

  TestLib.RunMousePath(MousePath.Path);

  Check(
    (FCanvas.ViewPortPosition.X <= 0) and (FCanvas.ViewPortPosition.X <= 0)
    , Format('FCanvas.Postion : %f, %f', [FCanvas.ViewPortPosition.X, FCanvas.ViewPortPosition.X])
  );
end;

procedure TestTThCanvasEditor.TestTrackingOutOfArea;
begin
  FCanvas.TrackAnimated := False;
  MousePath.New
  .Add(50, 50)
  .Add(0, 50)
  .Add(-200, 50);

  TestLib.RunMousePath(MousePath.Path);

  Check(
    (FCanvas.ViewPortPosition.X = -100)
    , Format('FCanvas.Postion : %f, %f', [FCanvas.ViewPortPosition.X, FCanvas.ViewPortPosition.X])
  );
end;

// S 아래쪽에 걸치게 도형을 그리고
//    캔버스 높이 이상을 Tracking하여 이동
//    도형의 색상을 추출하여 색상비교
procedure TestTThCanvasEditor.TestTrackingOutOfCanvasAreaNotDisplayed;
begin
  FCanvas.TrackAnimated := False;

  // Draw Rectangle
  FCanvas.DrawItemID := 1100;
  MousePath.New
  .Add(100, 150)
  .Add(150, 300)
  .Add(210, 400);
  TestLib.RunMousePath(MousePath.Path);

  // Canvas Tracking
  MousePath.New
  .Add(10, 290)
  .Add(10, 200)
  .Add(10, -50);
  TestLib.RunMousePath(MousePath.Path);

  // Select
  TestLib.RunMouseClick(150, 10);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');  // 안보여도 선택은 됨
  TThShapeItem(FCanvas.SelectedItem).BgColor := claRed;
  FCanvas.SelectedItem.Opacity := 1;

  // 색상비교
  Check(TestLib.GetControlPixelColor(FCanvas, 150, 10) = claRed);
end;

procedure TestTThCanvasEditor.TestUnselectingItem;
var
  Item: TThItem;
begin
  // Draw Rectangle
  FCanvas.DrawItemID := 1100;
  MousePath.New
  .Add(10, 10)
  .Add(50, 50)
  .Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  // Select
  TestLib.RunMouseClick(50, 50);

  Item := FCanvas.SelectedItem;
  Check(Assigned(Item), 'Corrent select');

  // Select
  TestLib.RunMouseClick(150, 150);

  Check(not Assigned(FCanvas.SelectedItem), 'Incorrent select');
  Check(not Item.Selected, 'Item unselected');
end;

procedure TestTThCanvasEditor.TestCanvasTrackingAnimation;
begin
  DrawRectangle(100, 100, 150, 150);

  MousePath.New
  .Add(10, 10)
  .Add(50, 10)
  .Add(55, 10);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.Delay(1000);

  Check(FCanvas.ViewPortPosition.X = (45 + 5 * CanvasTrackAniCount + CenterPos.X), Format('X: %f', [FCanvas.ViewPortPosition.X]));
end;

initialization
  RegisterTest(TestTThCanvasEditor.Suite);

end.

