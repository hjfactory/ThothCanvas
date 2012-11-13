unit TestThItemRectangleResizer;
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
  // #39 사각형 모서리를 드레그 하여 크기를 변경한다.
  TestTThItemRectangleResizer = class(TBaseTestUnit)
  published
    // #81 사각형 선택 시 4개의 ResizeSpot이 표시되야 한다.
    procedure TestShowResizeSpot;

    // #41 ResizeSpot을 드래그하여 크기를 변경 할 수 있다.
    procedure TestResizeRectangle;

    // #93 ResizeSpot을 이동 시 다른 Spot 위치 이전/이후로 이동이 가능해야 한다.
    procedure TestResizeRectangleTLtoBROver;
    procedure TestResizeRectangleTLtoRightOver;
    procedure TestResizeRectangleTLtoBottomOver;
    procedure TestResizeRectangleTRtoBLOver;
    procedure TestResizeRectangleBLtoTROver;
    procedure TestResizeRectangleBRtoTLOver;

    // #83 ResizeSpot의 이동 전후 마우스커서 위치가 ResizeSpot한해 동일 해야 한다.
    procedure TestResizeSpotSamePosition;

    // #84 크기 변경 시 최소 크기 미만으로 축소 할 수 없어야 한다.
    procedure TestResizeMinimum;
    procedure TestResizeMinimum2;

    // #96 크기 조정 시 Minsize가 적용되지 않고 겹치는 경우 발생
    procedure BugTestResizeSpotOverlap;

    // #95 크기 조정 시 대상이 아닌 Spot의 위치가 변함
    procedure BugTestResizeAnotherSpotMove;
    procedure BugTestResizeAnotherSpotMoveCenter;
    procedure BugTestResizeAnotherSpotMoveTopToBottom;
    procedure BugTestResizeAnotherSpotMoveBottomToTop;
  end;
implementation

uses
  FMX.TestLib, ThItem, ThShape, ThItemFactory, ThConsts;

{ TestTThItemResizer }

procedure TestTThItemRectangleResizer.TestShowResizeSpot;
begin
  DrawRectangle(10, 10, 160, 160);
  TestLib.RunMouseClick(100, 100);

  // ResizeSpot 표시 확인
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 10) = ItemResizeSpotOutColor, Format('Not matching color TopLeft(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotOutColor]));
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 160) = ItemResizeSpotOutColor, 'Not matching color BottomLeft');
  Check(TestLib.GetControlPixelColor(FCanvas, 160, 10) = ItemResizeSpotOutColor, 'Not matching color TopRight');
  Check(TestLib.GetControlPixelColor(FCanvas, 160, 160) = ItemResizeSpotOutColor, 'Not matching color BottomRight');
end;

procedure TestTThItemRectangleResizer.TestResizeRectangle;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(FCanvas.SelectedItem.Width = 1500, Format('Width : %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoBROver;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(220, 220)
  .Add(250, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoRightOver;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(180, 55)
  .Add(250, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 900),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoBottomOver;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(180, 180)
  .Add(60, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(100, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 900) and (Round(FCanvas.SelectedItem.Height) = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTRtoBLOver;
begin
  DrawRectangle(150, 50, 250, 150);
  TestLib.RunMouseClick(200, 100);

  MousePath.New
  .Add(250, 50)
  .Add(180, 180)
  .Add(60, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(100, 200);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 900) and (Round(FCanvas.SelectedItem.Height) = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleBLtoTROver;
begin
  DrawRectangle(50, 150, 150, 250);
  TestLib.RunMouseClick(100, 200);

  MousePath.New
  .Add(50, 250)
  .Add(50, 100)
  .Add(40, 240)
  .Add(250, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(200, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 900),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleBRtoTLOver;
begin
  DrawRectangle(150, 150, 250, 250);
  TestLib.RunMouseClick(200, 200);

  MousePath.New
  .Add(250, 250)
  .Add(200, 80)
  .Add(70, 80)
  .Add(50, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(120, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 900),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

// S1 - Spot의 최좌측을 이용해 크기조정
//      1> 이동된 포인터의 X-1 좌표의 색상 확인
//      2> Width 크기변화 확인 100에서 10이동
procedure TestTThItemRectangleResizer.TestResizeSpotSamePosition;
var
  SP, EP: TPointF;
  C: TAlphaColor;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  TestLib.RunMouseMove([PointF(150, 150)]);
  SP := PointF(150 - ItemResizeSpotRadius+1, 150);
  EP := SP;
  EP.Offset(10, 10);

  Check(TestLib.GetControlPixelColor(FCanvas, SP.X, SP.Y) = ItemResizeSpotOverColor, 'Spot color');

  MousePath.New
  .Add(SP)
  .Add(180, 180)
  .Add(EP);
  TestLib.RunMousePath(MousePath.Path);

  // 1> 색상확인                                         a
  C := TestLib.GetControlPixelColor(FCanvas, EP.X-1, EP.Y);
  Check(C <> ItemResizeSpotOverColor);

  // 2> 크기확인
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(Round(FCanvas.SelectedItem.Width) = 1100, Format('Width : %f', [FCanvas.SelectedItem.Width]));
end;

// S - 100 크기를 20으로 줄였을때 30이 되어야 한다.
procedure TestTThItemRectangleResizer.TestResizeMinimum;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(70, 70);
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(FCanvas.SelectedItem.Width = 300, Format('W: %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThItemRectangleResizer.TestResizeMinimum2;
begin
  DrawRectangle(100, 100, 200, 200);
  TestLib.RunMouseClick(150, 150);

  MousePath.New
  .Add(200, 200)
  .Add(80, 120);
  TestLib.RunMousePath(MousePath.Path);

//  Debug(Format('W: %f, H: %F',[FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height]));
  Check(Assigned(FCanvas.SelectedItem), 'Not Assigned');
  Check(FCanvas.SelectedItem.Width = 300, Format('Width: %f', [FCanvas.SelectedItem.Width]));
  Check(FCanvas.SelectedItem.Height = 300, Format('Height: %f', [FCanvas.SelectedItem.Height]));
end;

procedure TestTThItemRectangleResizer.BugTestResizeSpotOverlap;
begin
  DrawRectangle(100, 100, 200, 200);
  TestLib.RunMouseClick(150, 150);

  MousePath.New
  .Add(200, 200)
  .Add(99, 120);
  TestLib.RunMousePath(MousePath.Path);

//  Debug(Format('W: %f, H: %F',[FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height]));
  Check(Assigned(FCanvas.SelectedItem), 'Not Assigned');
  Check(FCanvas.SelectedItem.Width = 300, Format('Width: %f', [FCanvas.SelectedItem.Width]));
  Check(FCanvas.SelectedItem.Height = 300, Format('Height: %f', [FCanvas.SelectedItem.Height]));
end;

procedure TestTThItemRectangleResizer.BugTestResizeAnotherSpotMove;
begin
  FForm.Left := 10;
  FForm.Top := 10;
  FForm.Width := 1000;
  FForm.Height := 800;

  FCanvas.BoundsRect := RectF(10,10,799,603);
  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;

  DrawRectangle(395, 294, 595, 444);
  TestLib.RunMouseClick(500, 350);

  MousePath.New
  .Add(595, 444)
  .Add(250, 105)
  .Add(366,324)
  .Add(453,401);
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');
  Check(FCanvas.SelectedItem.Position.X = 3950, Format('X: %f', [FCanvas.SelectedItem.Position.X]));
end;

procedure TestTThItemRectangleResizer.BugTestResizeAnotherSpotMoveCenter;
begin
  DrawRectangle(10, 10, 150, 150);
  TestLib.RunMouseClick(50, 50);

  MousePath.New
  .Add(10, 10)
  .Add(50, 50)
  .Add(150, 150)
  .Add(250, 250)
  ;
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');
  Check(TestLib.GetControlPixelColor(FCanvas, 250, 150) = ItemResizeSpotOutColor);
end;

procedure TestTThItemRectangleResizer.BugTestResizeAnotherSpotMoveTopToBottom;
begin
  DrawRectangle(150, 50, 250, 150);
  TestLib.RunMouseClick(200, 100);

  MousePath.New
  .Add(250, 50)
  .Add(250, 150)
  .Add(250, 200)
  ;
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');
  Check(Round(FCanvas.SelectedItem.Position.Y) = 1500);
end;

procedure TestTThItemRectangleResizer.BugTestResizeAnotherSpotMoveBottomToTop;
begin
  DrawRectangle(50, 150, 150, 250);
  TestLib.RunMouseClick(100, 200);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');

  MousePath.New
  .Add(150, 250)
  .Add(150, 150)
  .Add(150, 50)
  ;
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');
  Check(Round(FCanvas.SelectedItem.Height) = 1000, Format('H : %f', [FCanvas.SelectedItem.Height]));
  Check(Round(FCanvas.SelectedItem.Position.Y) = 500, Format('Y : %f', [FCanvas.SelectedItem.Position.Y]));
end;

initialization
  RegisterTest(TestTThItemRectangleResizer.Suite);

end.
