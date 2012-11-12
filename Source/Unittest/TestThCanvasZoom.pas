unit TestThCanvasZoom;
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
  // Test methods for class TThInterfacedObject

  TestTThCanvasZoom = class(TBaseTestUnit)
  published
    // #191 지정된 비율로 캔버스가 확대/축소 되어야 한다.
    procedure TestZoomBasic;

    // #192 Zoom 되도 ResizeSpot 및 Highlight의 크기는 변경되지 않아야 한다.
    procedure TestZoomAndSpotSizeMaintain;
    procedure TestZoomAndHighlightSizeMaintain;

    // #196 명령을 통한 확대 축소는 중심점을 기준으로 Zoom 되어야 한다.
    procedure TestZoomCenterPosition;

    // #190 위치 중심으로 확대/축소 되어야 한다.
    procedure TestZoomAtPoint;

    // #189 캔버스 선택(클릭) 후 마우스 휠을 올리면 확대, 내리면 축소가 되어야 한다.
    procedure TestZoomWithWheel;

    // #201 한곳에서 Zoom처리 하면 포인트에 맞게 처리되지만 마우스를 다른데로 옮겨가면 시도 시 처리되지 않음
    procedure BugTestZoomPointAnotherPoint;

    // #199 이동 후에도 마우스 위치에 따른 Zoom이 가능해야 한다.
    procedure TestZoomPositionCheckAfterMove;

    // #198 Zoom이후 마우스로 아이템 추가 시 Zoom이 적용되지 않고 그려짐
    procedure BugTestZoomAfterDrawCenter;
    procedure BugTestZoomAfterDraw25;
    procedure BugTestZoomAfterDraw75;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShape, ThItemFactory, ThConsts, UITypes, FMX.Forms,
  DebugUtils;

{ TestTThCanvasZoom }

procedure TestTThCanvasZoom.TestZoomBasic;
begin
  DrawRectangle(50, 50, 150, 150);

  FCanvas.ZoomIn;
  Check(FCanvas.ZoomScale > 1, Format('ZoomIn: %f', [FCanvas.ZoomScale]));

  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  Check(FCanvas.ZoomScale < 1, Format('ZoomOt: %f', [FCanvas.ZoomScale]));
end;

procedure TestTThCanvasZoom.TestZoomAndSpotSizeMaintain;
var
  l: single;
  AC: TAlphaColor;
begin
  DrawRectangle(50, 50, 150, 150);

  AC := TestLib.GetControlPixelColor(FCanvas, 50-ItemResizeSpotRadius+1, 50);
  Check(AC = ItemResizeSpotOutColor, Format('Not matching color Orginal(%d, %d)', [AC, ItemResizeSpotOutColor]));

  FCanvas.ZoomOutAtPoint(0, 0);
  FCanvas.ZoomOutAtPoint(0, 0);

  l := 50 * FCanvas.ZoomScale;
  AC := TestLib.GetControlPixelColor(FCanvas, l-ItemResizeSpotRadius+1, l);
//  AC := TestLib.GetControlPixelColor(FCanvas, 50-ItemResizeSpotRadius+2, 50);
  Check(AC = ItemResizeSpotOutColor, Format('[Left: %f] Not matching color ZoomOut(%d, %d)', [l, AC, ItemResizeSpotOutColor]));
end;

procedure TestTThCanvasZoom.TestZoomAndHighlightSizeMaintain;
var
  l, c: single;
  AC: TAlphaColor;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseMove(MousePath.New.Add(100, 100).Path);

  AC := TestLib.GetControlPixelColor(FCanvas, 150+ItemHighlightSize-1, 100);
  Check(AC = ItemHighlightColor, Format('Not matching color Orginal(%d, %d)', [AC, ItemHighlightColor]));

  FCanvas.ZoomOutAtPoint(0, 0);
  FCanvas.ZoomOutAtPoint(0, 0);

  // 구현은 되었으나 테스트에서 오류
  l := 150 * FCanvas.ZoomScale;
  c := 100 * FCanvas.ZoomScale;
  AC := TestLib.GetControlPixelColor(FCanvas, Trunc(l)+ItemHighlightSize-1, Trunc(c));
  Check(AC = ItemHighlightColor, Format('[Left: %f] Not matching color ZoomOut(%d, %d)', [l, AC, ItemHighlightColor]));
Exit;
end;

procedure TestTThCanvasZoom.TestZoomCenterPosition;
begin
  DrawRectangle(150, 150, 250, 250);

  FCanvas.ZoomAnimated := False;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ClearSelection;

  TestLib.RunMouseClick(149, 149);

  CheckNull(FCanvas.SelectedItem);
end;

procedure TestTThCanvasZoom.TestZoomAtPoint;
begin
  DrawRectangle(100, 100, 250, 250);

  FCanvas.ZoomAnimated := False;
  FCanvas.ZoomOutAtPoint(100, 100);
  FCanvas.ZoomOutAtPoint(100, 100);
  FCanvas.ZoomOutAtPoint(100, 100);
  FCanvas.ClearSelection;

  TestLib.RunMouseClick(99, 99);

  CheckNull(FCanvas.SelectedItem);
end;

procedure TestTThCanvasZoom.TestZoomWithWheel;
var
  ZS: Single;
begin
  DrawRectangle(150, 150, 250, 250);

  ZS := FCanvas.ZoomScale;
  TestLib.RunMouseWheelUp(150, 150);
  TestLib.Delay(100);
  Check(FCanvas.ZoomScale < ZS, 'Up');

  ZS := FCanvas.ZoomScale;
  TestLib.RunMouseWheelDown(150, 150);
  TestLib.Delay(100);
  Check(FCanvas.ZoomScale > ZS, 'Down');
end;

procedure TestTThCanvasZoom.BugTestZoomPointAnotherPoint;
var
  X: Single;
  ZoomScale, ZoomScale2: Single;
begin
  ShowForm;
//
//  FForm.Left := 10;
//  FForm.Top := 10;
//  FForm.Width := 1020;
//  FForm.Height := 1020;
//
//  FCanvas.BoundsRect := RectF(100,100,1100,1100);
//  TestLib.SetInitialMousePoint(GetInitialPoint);
//  Application.ProcessMessages;

  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
//  FCanvas.ZoomOutAtPoint(75, 75);
//  FCanvas.ZoomOutAtPoint(225, 225);
//  FCanvas.ZoomOutAtPoint(0,0);

  DrawRectangle(75, 75, 150, 150);
  FCanvas.ClearSelection;

//  FCanvas.ZoomOutAtPoint(25, 25);
  FCanvas.ZoomOutAtPoint(75, 75);
//  FCanvas.test(FCanvas.ViewPortPosition.X + 0.75); //0
  FCanvas.test(FCanvas.ViewPortPosition.X - 0.75 - (0.75*0.9)-(0.75*0.9*0.9)-(0.75*0.9*0.9*0.9)-(0.75*0.9*0.9*0.9*0.9)); //150(Center)
//  FCanvas.test(FCanvas.ViewPortPosition.X - 1.5);  // 225
  DrawRectangle(75, 200, 150, 150);
  DrawRectangle(200, 75, 150, 150);


  FCanvas.ClearSelection;
  TestLib.RunMouseClick(76, 76);
  CheckNotNull(FCanvas.SelectedItem);
{
  ZoomScale := 1 * 0.9;
  X := (FCanvas.ViewPortSize.Width - 1000) * ZoomScale * (250 / FCanvas.Width);
  Check(FCanvas.ViewPortPosition.X = X, Format('1, Width: %f, X: (%f / %f)', [FCanvas.ViewPortSize.Width, FCanvas.ViewPortPosition.X, X]));

  FCanvas.ZoomOutAtPoint(750, 750);

  ZoomScale2 := ZoomScale * 0.9;
  X := X + (1000 / ZoomScale2 - 1000 / ZoomScale) * ZoomScale2 * (750 / 1000);
  Check(FCanvas.ViewPortPosition.X = X, Format('2, Width: %f, X: (%f / %f)', [FCanvas.Width, FCanvas.ViewPortPosition.X, X]));
}
end;

procedure TestTThCanvasZoom.TestZoomPositionCheckAfterMove;
begin
  MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(100, 150)
  .Add(150, 150)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);
end;

procedure TestTThCanvasZoom.BugTestZoomAfterDrawCenter;
begin
  FForm.Left := 10;
  FForm.Top := 10;
  FForm.Width := 1020;
  FForm.Height := 920;

  FCanvas.BoundsRect := RectF(10,10,1010,910);
  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;

  FCanvas.ZoomOut;
  FCanvas.ZoomOut;
  FCanvas.ZoomOut;

  DrawRectangle(100, 100, 200, 200);
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(101, 101);
  CheckNotNull(FCanvas.SelectedItem, 'TopLeft');
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(199, 199);
  CheckNotNull(FCanvas.SelectedItem, 'BottomRight');

  FCanvas.ClearSelection;
  TestLib.RunMouseClick(99, 99);
  CheckNull(FCanvas.SelectedItem);
//  CheckNull(FCanvas.SelectedItem, Format('TopLeft Null %f, %f', [FCanvas.SelectedItem.Position.X, FCanvas.SelectedItem.Position.Y]));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(201, 201);
  CheckNull(FCanvas.SelectedItem);
//  CheckNull(FCanvas.SelectedItem, Format('BottomRight Null %f, %f', [FCanvas.SelectedItem.Position.X, FCanvas.SelectedItem.Position.Y]));
end;

procedure TestTThCanvasZoom.BugTestZoomAfterDraw25;
begin
  FForm.Left := 10;
  FForm.Top := 10;
  FForm.Width := 1020;
  FForm.Height := 920;

  FCanvas.BoundsRect := RectF(10,10,1010,910);
  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;

  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4, FCanvas.Height / 4);
  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4, FCanvas.Height / 4);
  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4, FCanvas.Height / 4);

  DrawRectangle(100, 100, 200, 200);
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(101, 101);
  CheckNotNull(FCanvas.SelectedItem, 'TopLeft');
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(199, 199);
  CheckNotNull(FCanvas.SelectedItem, 'BottomRight');

  FCanvas.ClearSelection;
  TestLib.RunMouseClick(99, 99);
  CheckNull(FCanvas.SelectedItem);
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(201, 201);
  CheckNull(FCanvas.SelectedItem);
end;

procedure TestTThCanvasZoom.BugTestZoomAfterDraw75;
begin
  FForm.Left := 10;
  FForm.Top := 10;
  FForm.Width := 1020;
  FForm.Height := 920;

  FCanvas.BoundsRect := RectF(10,10,1010,910);
  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;

  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4 * 3, FCanvas.Height / 4 * 3);
  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4 * 3, FCanvas.Height / 4 * 3);
  FCanvas.ZoomOutAtPoint(FCanvas.Width / 4 * 3, FCanvas.Height / 4 * 3);

  DrawRectangle(100, 100, 200, 200);
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(101, 101);
  CheckNotNull(FCanvas.SelectedItem, 'TopLeft');
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(199, 199);
  CheckNotNull(FCanvas.SelectedItem, 'BottomRight');

  FCanvas.ClearSelection;
  TestLib.RunMouseClick(99, 99);
  CheckNull(FCanvas.SelectedItem);
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(201, 201);
  CheckNull(FCanvas.SelectedItem);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTThCanvasZoom.Suite);
end.

