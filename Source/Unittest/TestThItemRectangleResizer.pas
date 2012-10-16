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
  // #39 �簢�� �𼭸��� �巹�� �Ͽ� ũ�⸦ �����Ѵ�.
  TestTThItemRectangleResizer = class(TBaseTestUnit)
  protected
    procedure SetTestControl(var FormRect, CanvasRect: TRectF); override;
  published
    // #81 �簢�� ���� �� 4���� ResizeSpot�� ǥ�õǾ� �Ѵ�.
    procedure TestShowResizeSpot;

    // #41 ResizeSpot�� �巡���Ͽ� ũ�⸦ ���� �� �� �ִ�.
    procedure TestResizeRectangle;

    // #93 ResizeSpot�� �̵� �� �ٸ� Spot ��ġ ����/���ķ� �̵��� �����ؾ� �Ѵ�.
    procedure TestResizeRectangleTLtoBROver;
    procedure TestResizeRectangleTLtoRightOver;
    procedure TestResizeRectangleTLtoBottomOver;
    procedure TestResizeRectangleTRtoBLOver;
    procedure TestResizeRectangleBLtoTROver;
    procedure TestResizeRectangleBRtoTLOver;

    // #83 ResizeSpot�� �̵� ���� ���콺Ŀ�� ��ġ�� ResizeSpot���� ���� �ؾ� �Ѵ�.
    procedure TestResizeSpotSamePosition;

    // #84 ũ�� ���� �� �ּ� ũ�� �̸����� ��� �� �� ����� �Ѵ�.
    procedure TestResizeMinimum;
    procedure TestResizeMinimum2;

    // #96 ũ�� ���� �� Minsize�� ������� �ʰ� ��ġ�� ��� �߻�
    procedure BugTestResizeSpotOverlap;

    // #95 ũ�� ���� �� ����� �ƴ� Spot�� ��ġ�� ����
    procedure BugTestResizeAnotherSpotMove;
  end;
implementation

uses
  FMX.TestLib, ThItem, ThShape, ThItemFactory, ThConsts, CommonUtils;

{ TestTThItemResizer }

procedure TestTThItemRectangleResizer.TestShowResizeSpot;
begin
  DrawRectangle(10, 10, 160, 160);
  TestLib.RunMouseClick(100, 100);

  // ResizeSpot ǥ�� Ȯ��
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 10) = ItemResizeSpotOutColor, Format('Not matching color TopLeft(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotOutColor]));
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 160) = ItemResizeSpotOutColor, 'Not matching color BottomLeft');
  Check(TestLib.GetControlPixelColor(FCanvas, 160, 10) = ItemResizeSpotOutColor, 'Not matching color TopRight');
  Check(TestLib.GetControlPixelColor(FCanvas, 160, 160) = ItemResizeSpotOutColor, 'Not matching color BottomRight');
end;

procedure TestTThItemRectangleResizer.TestResizeRectangle;
begin
  // �׸���
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����
  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(FCanvas.SelectedItem.Width = 150, Format('Width : %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoBROver;
begin
  // �׸���
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����
  MousePath.New
  .Add(50, 50)
  .Add(220, 220)
  .Add(250, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 100) and (FCanvas.SelectedItem.Height = 100),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoRightOver;
begin
  // �׸���
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����(150, 60, 250, 150)
  MousePath.New
  .Add(50, 50)
  .Add(180, 55)
  .Add(250, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 100) and (FCanvas.SelectedItem.Height = 90),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTLtoBottomOver;
begin
  // �׸���
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����(60, 150, 150, 250)
  MousePath.New
  .Add(50, 50)
  .Add(180, 180)
  .Add(60, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(100, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 90) and (FCanvas.SelectedItem.Height = 100),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleTRtoBLOver;
begin
  // �׸���
  DrawRectangle(150, 50, 250, 150);
  TestLib.RunMouseClick(200, 100);

  //ũ�� ����(60, 150, 150, 250)
  MousePath.New
  .Add(250, 50)
  .Add(180, 180)
  .Add(60, 250);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(100, 200);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 90) and (FCanvas.SelectedItem.Height = 100),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleBLtoTROver;
begin
  // �׸���
  DrawRectangle(50, 150, 150, 250);
  TestLib.RunMouseClick(100, 200);

  //ũ�� ����(150, 60, 250, 250)
  MousePath.New
  .Add(50, 250)
  .Add(50, 100)
  .Add(40, 240)
  .Add(250, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(200, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 100) and (FCanvas.SelectedItem.Height = 90),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThItemRectangleResizer.TestResizeRectangleBRtoTLOver;
begin
  // �׸���
  DrawRectangle(150, 150, 250, 250);
  TestLib.RunMouseClick(200, 200);

  //ũ�� ����(150, 60, 250, 250)
  MousePath.New
  .Add(250, 250)
  .Add(200, 80)
  .Add(70, 80)
  .Add(50, 60);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(120, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 100) and (FCanvas.SelectedItem.Height = 90),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

// S1 - Spot�� �������� �̿��� ũ������
//      1> �̵��� �������� X-1 ��ǥ�� ���� Ȯ��
//      2> Width ũ�⺯ȭ Ȯ�� 100���� 10�̵�
procedure TestTThItemRectangleResizer.TestResizeSpotSamePosition;
var
  SP, EP: TPointF;
  C: TAlphaColor;
begin
  // �׸���
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  TestLib.RunMouseMove([PointF(150, 150)]);
  SP := PointF(150 - ItemResizeSpotRadius+1, 150);
  EP := SP;
  EP.Offset(10, 10);

  Check(TestLib.GetControlPixelColor(FCanvas, SP.X, SP.Y) = ItemResizeSpotOverColor, 'Spot color');
  //ũ�� ����
  MousePath.New
  .Add(SP)
  .Add(180, 180)
  .Add(EP);
  TestLib.RunMousePath(MousePath.Path);

  // 1> ����Ȯ��                                         a
  C := TestLib.GetControlPixelColor(FCanvas, EP.X-1, EP.Y);
  Check(C <> ItemResizeSpotOverColor);

  // 2> ũ��Ȯ��
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(FCanvas.SelectedItem.Width = 110, Format('Width : %f', [FCanvas.SelectedItem.Width]));
end;

// S - 100 ũ�⸦ 20���� �ٿ����� 30�� �Ǿ�� �Ѵ�.
procedure TestTThItemRectangleResizer.TestResizeMinimum;
begin
  DrawRectangle(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����
  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(70, 70);
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(FCanvas.SelectedItem.Width = 30, Format('W: %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThItemRectangleResizer.TestResizeMinimum2;
begin
  DrawRectangle(100, 100, 200, 200);
  TestLib.RunMouseClick(150, 150);

  MousePath.New
  .Add(200, 200)
  .Add(80, 120);
  TestLib.RunMousePath(MousePath.Path);

  Debug(Format('W: %f, H: %F',[FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height]));
  Check(Assigned(FCanvas.SelectedItem), 'Not Assigned');
  Check(FCanvas.SelectedItem.Width = 30, Format('Width: %f', [FCanvas.SelectedItem.Width]));
  Check(FCanvas.SelectedItem.Height = 30, Format('Height: %f', [FCanvas.SelectedItem.Height]));
end;

procedure TestTThItemRectangleResizer.BugTestResizeSpotOverlap;
begin
  DrawRectangle(100, 100, 200, 200);
  TestLib.RunMouseClick(150, 150);

  MousePath.New
  .Add(200, 200)
  .Add(99, 120);
  TestLib.RunMousePath(MousePath.Path);

  Debug(Format('W: %f, H: %F',[FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height]));
  Check(Assigned(FCanvas.SelectedItem), 'Not Assigned');
  Check(FCanvas.SelectedItem.Width = 30, Format('Width: %f', [FCanvas.SelectedItem.Width]));
  Check(FCanvas.SelectedItem.Height = 30, Format('Height: %f', [FCanvas.SelectedItem.Height]));
end;

procedure TestTThItemRectangleResizer.SetTestControl(var FormRect,
  CanvasRect: TRectF);
begin
  FormRect.Top := 10;
  FormRect.Left := 10;
  FormRect.Width := 1000;
  FormRect.Height := 800;

  CanvasRect := RectF(10,10,799,603);
end;

procedure TestTThItemRectangleResizer.BugTestResizeAnotherSpotMove;
begin
  DrawRectangle(395, 294, 595, 444);
  TestLib.RunMouseClick(500, 350);

  MousePath.New
  .Add(595, 444)
  .Add(250, 105)
  .Add(366,324)
  .Add(453,401)
  ;
  TestLib.RunMousePath(MousePath.Path);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned.');
  Check(FCanvas.SelectedItem.Position.X = 395, Format('X: %F', [FCanvas.SelectedItem.Position.X]));


end;

initialization
  RegisterTest(TestTThItemRectangleResizer.Suite);

end.
