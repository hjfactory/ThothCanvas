unit TestThShape;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, BaseTestUnit, ThContainer, ThCanvasEditor,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, FMX.Forms;

type
  TestTThShape = class(TBaseTestUnit)
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

    // #55 사각형 그리고 처음 사각형 그려진 범위 위로 캔버스 트래킹 하면 사각형이 감춰짐
    procedure TestDisappearsUpward;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShape, ThItemFactory;

{ TestTThShape }

procedure TestTThShape.TestItemFactory;
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

procedure TestTThShape.TestDrawRectangle;
var
  Item: TThItem;
begin
  // Draw
  FCanvas.ItemID := 1100;   // 1100 is Rectangles ID
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(50, 50);
  MousePath.Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  // Select
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(100, 100);
  MousePath.Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  Item := FCanvas.SelectedItem;

  Check(Assigned(Item), 'Check SelectedItem');
  Check(Item.ClassType = TThRectangle, 'Check Class type');

  Check(Item.Position.X = 50, Format('X = %f', [Item.Position.X]));
  Check(Item.Width = 150, Format('Width = %f', [Item.Width]));
end;

// S1.100,50 으로 Canvas 이동 후 0,0 > 100, 100 Rectangle 그리면
//    Rectangle의 좌표는 -100, -50 이어야 한다.
procedure TestTThShape.TestCanvasTrackingAndDrawRectangle;
var
  Item: TThItem;
begin
  // Tracking
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(0, 0);
  MousePath.Add(10, 0);
  MousePath.Add(100, 50);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ContentPos.X = 100);

  Application.ProcessMessages;

  // Draw Rectangle
  FCanvas.ItemID := 1100;
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(10, 10);
  MousePath.Add(10, 20);
  MousePath.Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1);

  Application.ProcessMessages;
  // Select
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(50, 50);
  MousePath.Add(50, 50);
  TestLib.RunMousePath(MousePath.Path);

  Item := FCanvas.SelectedItem;

  Check(Assigned(Item), 'not assigned');
  Check(Item.Position.X = -90, Format('Postion.X : %f', [Item.Position.X]));
  Check(Item.Position.Y = -40, Format('Postion.Y : %f', [Item.Position.Y]));
end;

procedure TestTThShape.TestRectangleSelect;
begin
  // Draw Rectangle
  FCanvas.ItemID := 1100;
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(50, 50);
  MousePath.Add(100, 100);
  TestLib.RunMousePath(MousePath.Path);

  // Select
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(60, 60);
  MousePath.Add(60, 60);
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem));
end;

// S1.0,0에 Rectangle을 그리고 -100, -100 이동한 후
//    -30, -30을 클릭하면 그영역에 있는 버튼이 클릭되야 한다.
procedure TestTThShape.TestRectangleOutOfCanvas;
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
  FCanvas.ItemID := 1100;
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(10, 10);
  MousePath.Add(100, 100);
  MousePath.Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  Application.ProcessMessages;

  // Canvas Tracking
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(160, 160);
  MousePath.Add(80, 80);
  MousePath.Add(60, 60);
  TestLib.RunMousePath(MousePath.Path);

  Application.ProcessMessages;

  // Button Click
  FTestClick := False;
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(-30, -30);
  MousePath.Add(-30, -30);
  TestLib.RunMousePath(MousePath.Path);

//  Check(FCanvas.SelectedItem.LocalToAbsolute(PointF(0, 0)).X = -30, Format('Postion.X : %f', [FCanvas.SelectedItem.LocalToAbsolute(PointF(0, 0)).X]));
  Check(FTestClick, '버튼이 클릭되지 않음');
end;

procedure TestTThShape.TestDisappearsUpward;
begin
  // Draw Rectangle
  FCanvas.ItemID := 1100;
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(10, 10);
  MousePath.Add(100, 100);
  MousePath.Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  Application.ProcessMessages;

  // Canvas Tracking
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(160, 160);
  MousePath.Add(160, 80);
  MousePath.Add(160, 60);
  TestLib.RunMousePath(MousePath.Path);

  // Select
  MousePath.Clear;
  MousePath.SetInitialPoint(GetInitialPoint);
  MousePath.Add(10, 10);
  MousePath.Add(10, 10);
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem));
end;

procedure TestTThShape._Test(Sender: TObject);
begin
  FTestClick := True;
end;

initialization
  RegisterTest(TestTThShape.Suite);

end.

