unit TestThItemRectangle;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #18 캔버스에 사각형을 추가한다.
  TestTThRectangle = class(TThCanvasBaseTestUnit)
  private
    FTestClick: Boolean;
    procedure _Test(Sender: TObject);
  published
    procedure TestItemFactory;

    // #36 마우스 드래그로 시작점과 끝점을 이용해 도형을 그린다.
    procedure TestDrawRectangle;

    // #56 사각형 영역을 클릭하면 사각형이 선택되어야 한다.
    procedure TestRectangleSelect;

    // #54 캔버스 트래킹 이후 마우스 드래그 시 드래그한 위치에 그려져야 한다.
    procedure TestCanvasTrackingAndDrawRectangle;

    // #53 캔버스 Tracking 시 Rectangle이 Canvas 영역 밖으로 나오지 않는다.
    procedure TestRectangleOutOfCanvas;

{$IFDEF ON_HIGHLIGHT}
    // #68 도형 선택 시 하이라이트 효과가 나타나야 한다.
    procedure TestRectangleSelectionHighlight;

    // #38 도형에 마우스 오버시 하이라이트 효과가 나타난다.
    procedure TestRectangleMouseOverHighlight;
{$ENDIF}

    // #37 끝점이 시작점 앞에 있어도 그려져야 한다.
    procedure TestDrawRectangleEx;

    // #42 최소 크기를 갖으며 그리거나 크기 조정시 반영된다.
    procedure TestDrawRectangleMinSize;

    // #72 캔버스 영역 밖으로 이동이 가능해야 한다.
    procedure TestMoveRectangleOutOfCanvas;

{$IFDEF ON_HIGHLIGHT}
    // #69 도형 선택 후 다른 도형 선택 시 이전 도형의 하이라이트 효과는 없어져야 한다.
    procedure TestHideHighlightFromBeforeSelections;
{$ENDIF}

    // #94 아이템 추가 후 바로 이동 시 TopLeft Spot의 잔상이 남음
    procedure BugTestCreateAndMoveAfterPaint;

    // #133 도형 그릴때에는 도형위에 중복하여 그린다.
    procedure TestDrawOverItem;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts;

{ TestTThShape }

procedure TestTThRectangle._Test(Sender: TObject);
begin
  FTestClick := True;
end;

procedure TestTThRectangle.TestItemFactory;
var
  Item: TThItem;
begin
  // Not assigned number 0
  Item := ItemFactory.Get(0);
  try
    Check(not Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;

  // 1100 is Rectangle
  Item := ItemFactory.Get(1100);
  try
    Check(Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;
end;

procedure TestTThRectangle.TestDrawRectangle;
var
  Item: TThItem;
begin
  // Draw
  DrawRectangle(50, 50, 200, 200);

  // Select
  TestLib.RunMouseClick(100, 100);

  Item := FCanvas.SelectedItem;

  Check(Assigned(Item), 'Check SelectedItem');
  Check(Item.ClassType = TThRectangle, 'Check Class type');

  Check(Item.Position.X = -1000, Format('X = %f', [Item.Position.X]));
  Check(Item.Width = 1500,     Format('Width = %f', [Item.Width]));
end;

// S1.100,50 으로 Canvas 이동 후 0,0 > 100, 100 Rectangle 그리면
//    Rectangle의 좌표는 -100, -50 이어야 한다.
procedure TestTThRectangle.TestCanvasTrackingAndDrawRectangle;
var
  Item: TThItem;
begin
  FCanvas.TrackAnimated := False;

  // Tracking
  MousePath.New
  .Add(0, 0)
  .Add(10, 0)
  .Add(100, 50);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ViewPortPosition.X = 250, Format('ViewPortPosition.X: %f', [FCanvas.ViewPortPosition.X]));

  // Draw Rectangle
  DrawRectangle(10, 10, 100, 100);

  Check(FCanvas.ItemCount = 1);

  // Select
  TestLib.RunMouseClick(50, 50);

  Item := FCanvas.SelectedItem;

  Check(Assigned(Item), 'not assigned');
  Check(Item.Position.X = -2400, Format('Postion.X : %f', [Item.Position.X]));
  Check(Item.Position.Y = -1900, Format('Postion.Y : %f', [Item.Position.Y]));
end;

procedure TestTThRectangle.TestRectangleSelect;
begin
  // Draw Rectangle
  DrawRectangle(50, 50, 100, 100);

  // Select
  TestLib.RunMouseClick(60, 60);

  Check(Assigned(FCanvas.SelectedItem));
end;

// S1.0,0에 Rectangle을 그리고 -100, -100 이동한 후
//    -30, -30을 클릭하면 그영역에 있는 버튼이 클릭되야 한다.
procedure TestTThRectangle.TestRectangleOutOfCanvas;
var
  Button: TButton;
begin
  Button := TButton.Create(FForm);
  Button.Parent := FForm;
  Button.Position.Point := PointF(0,0);
  Button.OnClick := _Test;
  Button.Width := 50;
  Button.Height := 100;
  Button.SendToBack;

  // Draw Rectangle
  DrawRectangle(10, 10, 150, 150);

  // Canvas Tracking
  MousePath.New
  .Add(160, 160)
  .Add(80, 80)
  .Add(60, 60);
  TestLib.RunMousePath(MousePath.Path);

  // Button Click
  TestLib.RunMouseClick(-30, -30);

//  Check(FCanvas.SelectedItem.LocalToAbsolute(PointF(0, 0)).X = -30, Format('Postion.X : %f', [FCanvas.SelectedItem.LocalToAbsolute(PointF(0, 0)).X]));
  Check(FTestClick, '버튼이 클릭되지 않음');
end;

{$IFDEF ON_HIGHLIGHT}
procedure TestTThRectangle.TestRectangleSelectionHighlight;
var
  AC: TAlphaColor;
begin
  // 그리기
  DrawRectangle(10, 10, 100, 100);

  // 클릭 선택
  TestLib.RunMouseClick(50, 50);

  // 105.50 색상확인
  AC := TestLib.GetControlPixelColor(FCanvas, 100 + (ItemHighlightSize - 1), 50);
  Check(AC = ItemHighlightColor, 'Not matching Color');
end;

procedure TestTThRectangle.TestRectangleMouseOverHighlight;
var
  AC: TAlphaColor;
begin
  // 추가
  DrawRectangle(10, 10, 100, 100);

  // 선택
  TestLib.RunMouseClick(50, 50);

  FCanvas.BgColor := claPink;

  // 선택해제
  TestLib.RunMouseClick(150, 150);
  AC := TestLib.GetControlPixelColor(FCanvas, 100 + (ItemHighlightSize - 1), 100 + (ItemHighlightSize - 1));
  Check(AC <> ItemHighlightColor, 'Canvas color is not highlight color');

  MousePath.New
  .Add(150, 150)
  .Add(50, 50);
  TestLib.RunMouseMove(MousePath.Path);

  // 그림자 확인
  AC := TestLib.GetControlPixelColor(FCanvas, 100 + (ItemHighlightSize - 1), 100 + (ItemHighlightSize - 1));
  Check(AC = ItemHighlightColor, 'Not matching Color');
//  Check(AC = claGray, 'Not matching Color');
end;
{$ENDIF}

procedure TestTThRectangle.TestDrawRectangleEx;
begin
  // BRToTL
  DrawRectangle(200, 100, 110, 10);
  TestLib.RunMouseClick(150, 50);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned BRToTL');
  Check(FCanvas.SelectedItem.Position.X = -400, Format('BottomRight > TopLeft - X : %f', [FCanvas.SelectedItem.Position.X]));

  // TRToBL
  DrawRectangle(100, 110, 10, 200);
  TestLib.RunMouseClick(50, 150);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned TRToBL');
  Check(FCanvas.SelectedItem.Position.X = -1400, Format('TopRight > BottomLeft - X : %f', [FCanvas.SelectedItem.Position.X]));

  // BLToTR
  DrawRectangle(110, 200, 200, 110);
  TestLib.RunMouseClick(150, 150);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned BLToTR');
  Check(FCanvas.SelectedItem.Position.X = -400, Format('BottomLeft > TopRight - X : %f', [FCanvas.SelectedItem.Position.X]));
end;

procedure TestTThRectangle.TestDrawRectangleMinSize;
begin
  DrawRectangle(10, 10, 10, 10);
  TestLib.RunMouseClick(35, 35);  // MinSize 30, 30 : 10,10 > 40, 40
  Check(Assigned(FCanvas.SelectedItem), 'Empty Rect');

  // TLToBR
  DrawRectangle(10, 10, 30, 30);
  TestLib.RunMouseClick(35, 35);  // MinSize 30, 30 : 10,10 > 40, 40
  Check(Assigned(FCanvas.SelectedItem), 'TLToBR');

  // TRToBL
  DrawRectangle(60, 30, 80, 10);
 TestLib.RunMouseClick(85, 5);  // MinSize 30, 30 : 10,0 > 40, 30
  Check(Assigned(FCanvas.SelectedItem), 'TRToBL');

  // BLToTR
  DrawRectangle(30, 60, 10, 80);
  TestLib.RunMouseClick(5, 85);  // MinSize 30, 30 : 0,10 > 30, 40
  Check(Assigned(FCanvas.SelectedItem), 'BLToTR');

  // BRToTL
  DrawRectangle(80, 80, 60, 60);
  TestLib.RunMouseClick(55, 55);  // MinSize 30, 30 : 0,0 > 30, 30
  Check(Assigned(FCanvas.SelectedItem), 'BRToTL');
end;

procedure TestTThRectangle.TestMoveRectangleOutOfCanvas;
var
  Item: TThItem;
begin
  DrawRectangle(10, 10, 100, 100);

  // 밖에서 안으로 드래그
  MousePath.New
  .Add(50, 50)
  .Add(-100, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  // 선택
  TestLib.RunMouseClick(200, 200);
  Item := FCanvas.SelectedItem;
  Check(Assigned(Item), 'Not assigned');
end;

{$IFDEF ON_HIGHLIGHT}
procedure TestTThRectangle.TestHideHighlightFromBeforeSelections;
var
  AC: TAlphaColor;
begin
  // 2개 그리기
  DrawRectangle(10, 10, 40, 40);  // A
  DrawRectangle(60, 60, 90, 90);  // B

  // 선택 B
  TestLib.RunMouseClick(75, 75);

  // B 선택 그림자 확인
  AC := TestLib.GetControlPixelColor(FCanvas, 90 + (ItemHighlightSize - 1), 75);
  Check(AC = ItemHighlightColor, 'Check Show Highlight(B)');

  // 선택 A
  TestLib.RunMouseClick(25, 25);

  // B 그림자 없어짐
  AC := TestLib.GetControlPixelColor(FCanvas, 90 + (ItemHighlightSize - 1), 75);
  Check(AC <> ItemHighlightColor, 'Check Hide Highlight(B)');

  // A 선택 그림자 확인
  AC := TestLib.GetControlPixelColor(FCanvas, 40 + (ItemHighlightSize - 1), 25);
  Check(AC = ItemHighlightColor, 'Check Show Highlight(A)');
end;
{$ENDIF}

procedure TestTThRectangle.BugTestCreateAndMoveAfterPaint;
var
  ExceptL: Integer;
  AC: TAlphaColor;
  X, Y: Single;
  P: TPointF;
begin
  FCanvas.BgColor := claRed;

{ Screen shot을 위해 폼 활성화 체크 }
  ExceptL := 0;
  P := IControl(FCanvas).LocalToScreen(PointF(100, 100));
  while TestLib.GetBitmapPixelColor(P.X, P.Y) <> claRed do
  begin
//    Application.ProcessMessages;
//    Sleep(0);

    Inc(ExceptL);

    if ExceptL > 2 then
    begin
      Check(False, 'Checking Testcase - Activate Form');
    end;
  end;
{ Screen shot을 위해 폼 활성화 체크 }

  DrawRectangle(10, 10, 100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(100, 100)
  .Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  X := 10-ItemHighlightSize+2;
  Y := 10-ItemHighlightSize+2;
  P := IControl(FCanvas).LocalToScreen(PointF(X, Y));

  AC := TestLib.GetBitmapPixelColor(P.X, P.Y);
  Check(AC = claRed, 'Showing Afterpaint');
end;

procedure TestTThRectangle.TestDrawOverItem;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(50, 50, 200, 200);

  TestLib.RunMouseClick(150, 150);
  Check(Assigned(FCanvas.SelectedItem), 'Drawing item over of item');
end;

initialization
  RegisterTest(TestTThRectangle.Suite);

end.

