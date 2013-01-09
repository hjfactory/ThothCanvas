unit TestThItemLine;
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
  // #19 ĵ������ ���� �߰��Ѵ�.
  TestTThLine = class(TBaseTestUnit)
  published
    procedure TestItemFactory;

    // #74 ���콺 �巡�׷� �������� ������ �̿��� ������ �׸���.
    procedure TestDrawLineTLtoBR;
    // #75 ������ �������� �տ� �־ �׷����� �Ѵ�.
    procedure TestDrawLineTRtoBL;
    procedure TestDrawLineBLtoTR;
    procedure TestDrawLineBRtoTL;

    // #76 ������ ���콺 ������ ���̶���Ʈ ȿ���� ��Ÿ����.
    procedure TestLineMouseOverHighlight;

    // #99 ���� �Ѿ�� ������ ������ Ŭ�� �� ���� ���õȴ�.
    procedure BugTestLineOutOfRange;

    // #98 ���� �ƴ� �ٸ� ������ ���� ������ ���� �ʾƾ� �Ѵ�.
    procedure TestLineSelectionRange;

    // #97 ������, ������ �׸� �� �־�� �Ѵ�.
    procedure TestLineHorizon;
    procedure TestLineVertical;

    // #77 �ּ� ũ�⸦ ������ �׸��ų� ũ������ �� �ݿ��ȴ�.
    procedure TestLineMinimumSize;

    // #43 ���� �������� �������� ���콺�� ���õǾ�� �Ѵ�.
    procedure TestRangeSelectHorizonOverY;  // ����
    procedure TestRangeSelectVerticalOverX; // ������
    procedure TestRangeSelectLineTLtoBR;

    // #80 SizeSpot�� �巡�� �Ͽ� ũ�⸦ ���� �� �� �ִ�.
    procedure TestResizeLine;

    procedure TestResizeLineBRtoBottom;
    procedure TestResizeLineBottomtoBLOver;
    procedure TestResizeLineTLtoBROver;
    procedure TestResizeLineTLtoRightOver;

    // #100 ũ������ �� �ּ� ũ�Ⱑ ����Ǿ�� �Ѵ�.
    procedure TestResizeMinimum;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThShape }

procedure TestTThLine.TestItemFactory;
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

  Item := ItemFactory.Get(1200);
  try
    Check(Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;
end;

procedure TestTThLine.TestDrawLineTLtoBR;
begin
  // TopLeft > BottomRight
  DrawLine(10, 10, 100, 100);

  Check(TestLib.GetControlPixelColor(FCanvas, 20, 20) = ItemShapeDefaultColor, 'TopLeft > BottomRight - 1');
  Check(TestLib.GetControlPixelColor(FCanvas, 90, 90) = ItemShapeDefaultColor, 'TopLeft > BottomRight - 2');
end;

procedure TestTThLine.TestDrawLineTRtoBL;
begin
  // TopRight > BottomLeft
  DrawLine(100, 10, 10, 100);

  Check(TestLib.GetControlPixelColor(FCanvas, 90, 20) = ItemShapeDefaultColor, 'TopRight > BottomLeft - 1');
  Check(TestLib.GetControlPixelColor(FCanvas, 20, 90) = ItemShapeDefaultColor, 'TopRight > BottomLeft - 2');

end;

procedure TestTThLine.TestDrawLineBLtoTR;
begin
  // BottomLeft > TopRight
  DrawLine(10, 100, 100, 10);

  Check(TestLib.GetControlPixelColor(FCanvas, 20, 90) = ItemShapeDefaultColor, 'BottomLeft > TopRight - 1');
  Check(TestLib.GetControlPixelColor(FCanvas, 90, 20) = ItemShapeDefaultColor, 'BottomLeft > TopRight - 2');
end;

procedure TestTThLine.TestDrawLineBRtoTL;
begin
  // BottomRight > TopLeft
  DrawLine(100, 100, 10, 10);

  Check(TestLib.GetControlPixelColor(FCanvas, 20, 20) = ItemShapeDefaultColor, 'BottomRight > TopLeft - 1');
  Check(TestLib.GetControlPixelColor(FCanvas, 90, 90) = ItemShapeDefaultColor, 'BottomRight > TopLeft - 2');
end;

procedure TestTThLine.TestLineMouseOverHighlight;
var
  AC: TAlphaColor;
begin
  // �߰�
  DrawLine(10, 10, 100, 100);
  // ����
  TestLib.RunMouseClick(50, 50);

  FCanvas.BgColor := claPink;

  // ��������
  TestLib.RunMouseClick(150, 150);
  AC := TestLib.GetControlPixelColor(FCanvas, 100 + (ItemHighlightSize - 1), 100 + (ItemHighlightSize - 1));
  Check(AC <> ItemHighlightColor, 'Color is not highlight color');

  MousePath.New
  .Add(150, 150)
  .Add(50, 50);
//  .Add(101, 101);
  TestLib.RunMouseMove(MousePath.Path);

  // �׸��� Ȯ��
  AC := TestLib.GetControlPixelColor(FCanvas, 100 + (ItemHighlightSize), 100 + (ItemHighlightSize));
  Check(AC = ItemHighlightColor, 'Not matching Color');
//  Check(AC = claGray, 'Not matching Color');
end;

procedure TestTThLine.BugTestLineOutOfRange;
begin
  // �߰�
  DrawLine(10, 10, 100, 100);

  // ������ ����
  TestLib.RunMouseClick(150, 150);

  Check(not Assigned(FCanvas.SelectedItem), 'Out of range');

  // ����
  TestLib.RunMouseClick(50, 50);

  // ��������
  TestLib.RunMouseClick(150, 150);

  Check(not Assigned(FCanvas.SelectedItem), 'Unselect');
end;

procedure TestTThLine.TestLineSelectionRange;
begin
  // �߰�
  DrawLine(10, 10, 100, 100);

  // ���� ���� �� ����
  TestLib.RunMouseClick(80, 50);
  CheckNull(FCanvas.SelectedItem, 'Invalid select area');

  // ����
  TestLib.RunMouseClick(50, 50);
//  CheckTrue(Assigned(FCanvas.SelectedItem));
  CheckNotNull(FCanvas.SelectedItem);
end;

procedure TestTThLine.TestLineHorizon;
begin
  DrawLine(10, 200, 20, 200);
  FCanvas.ClearSelection;

  Check(TestLib.GetControlPixelColor(FCanvas, 10, 197) = ItemShapeDefaultColor, 'Start');
  Check(TestLib.GetControlPixelColor(FCanvas, 20, 197) = ItemShapeDefaultColor, 'End');
end;

procedure TestTThLine.TestLineVertical;
begin
  DrawLine(10, 10, 10, 20);
  FCanvas.ClearSelection;

  Check(TestLib.GetControlPixelColor(FCanvas, 7, 10) = ItemShapeDefaultColor, 'Start');
  Check(TestLib.GetControlPixelColor(FCanvas, 7, 20) = ItemShapeDefaultColor, 'End');
end;

procedure TestTThLine.TestLineMinimumSize;
begin
  DrawLine(10, 10, 20, 10);

  TestLib.RunMouseClick(15, 10);
  Check(Assigned(FCanvas.SelectedItem));
  Check(FCanvas.SelectedItem.Width = 300, Format('W: %f', [FCanvas.SelectedItem.Width]));

  TestLib.RunMouseClick(150, 150);
  TestLib.RunMouseClick(ItemMinimumSize-1, 10);
  Check(Assigned(FCanvas.SelectedItem));
end;

procedure TestTThLine.TestRangeSelectHorizonOverY;
begin
  DrawLine(10, 10, 100, 10);

  TestLib.RunMouseClick(10, 7);
  Check(Assigned(FCanvas.SelectedItem), 'Top');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(10, 13);
  Check(Assigned(FCanvas.SelectedItem), 'Bottom');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(7, 10);
  Check(Assigned(FCanvas.SelectedItem), 'Left');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(103, 10);
  Check(Assigned(FCanvas.SelectedItem), 'Right');
end;

procedure TestTThLine.TestRangeSelectVerticalOverX;
begin
  DrawLine(10, 10, 10, 100);

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(10, 7);
  Check(Assigned(FCanvas.SelectedItem), 'Top');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(13, 10);
  Check(Assigned(FCanvas.SelectedItem), 'Right');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(7, 10);
  Check(Assigned(FCanvas.SelectedItem), 'Left');

  TestLib.RunMouseClick(100, 100);
  TestLib.RunMouseClick(10, 103);
  Check(Assigned(FCanvas.SelectedItem), 'Bottom');
end;

procedure TestTThLine.TestRangeSelectLineTLtoBR;
var
  Rect: TRectF;
  P, P2, B, DP: TPointF;
  D, R: Single;
begin
  Rect := RectF(10, 10,100, 100);
  DrawLine(Rect);

  D := (ItemLineSelectionThickness - 1) / 2;
  P := Rect.CenterPoint;
  R := ArcTan(Rect.Height/Rect.Width);

  B := PointF(Sin(R) * D, Cos(R) * D);
  DP := PointF(B.X, -B.Y);
{
  P2 := P.Add(PointF(DP.X, -DP.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Top D Point(%f, %f)', [P2.X, P2.Y]));

  P2 := P.Add(PointF(-DP.X, DP.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Bottom D Point(%f, %f)', [P2.X, P2.Y]));
}
  P2 := P.Add(PointF(0, -D/Cos(R) + 1));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Center Top(%f, %f)', [P2.X, P2.Y]));

  P2 := P.Add(PointF(-D/Sin(R)+1, 0));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Center Left(%f, %f)', [P2.X, P2.Y]));

  P2 := P.Add(PointF(D/Sin(R)-1, 0));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Center Right(%f, %f)', [P2.X, P2.Y]));

  P2 := P.Add(PointF(0, D/Cos(R)-1));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('Center Bottom(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.TopLeft.Add(PointF(-B.X, B.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('TopLeft Left(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.TopLeft.Add(PointF(B.X, - B.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('TopLeft Top(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.TopLeft.Add(PointF(-B.Y, -B.X));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('TopLeft TopLeft(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.BottomRight.Add(PointF(B.X, -B.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('BottomRight Right(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.BottomRight.Add(PointF(-B.X, B.Y));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('BottomRight Bottom(%f, %f)', [P2.X, P2.Y]));

  P2 := Rect.BottomRight.Add(PointF(B.Y, B.X));
  FCanvas.ClearSelection;
  TestLib.RunMouseClick(P2.X, P2.Y);
  Check(Assigned(FCanvas.SelectedItem), Format('BottomRight BottomRight(%f, %f)', [P2.X, P2.Y]));
end;

procedure TestTThLine.TestResizeLine;
begin
  DrawLine(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(180, 180);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(Round(FCanvas.SelectedItem.Width) = 1500, Format('Width : %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThLine.TestResizeLineBRtoBottom;
begin
  DrawLine(50, 50, 150, 150);

  MousePath.New
  .Add(150, 150)
  .Add(50, 150);
  TestLib.RunMousePath(MousePath.Path);

  FCanvas.ClearSelection;
  TestLib.RunMouseClick(50, 120);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (FCanvas.SelectedItem.Width = 1) and (FCanvas.SelectedItem.Height = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThLine.TestResizeLineBottomtoBLOver;
begin
  DrawLine(250, 50, 250, 150);
  TestLib.RunMouseClick(250, 100);

  MousePath.New
  .Add(250, 150)
  .Add(245, 150)
  .Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(200, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

procedure TestTThLine.TestResizeLineTLtoBROver;
begin
  DrawLine(50, 50, 150, 150);
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

procedure TestTThLine.TestResizeLineTLtoRightOver;
begin
  DrawLine(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  MousePath.New
  .Add(50, 50)
  .Add(180, 55)
  .Add(250, 50);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(200, 100);
  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');
  Check(
    (Round(FCanvas.SelectedItem.Width) = 1000) and (Round(FCanvas.SelectedItem.Height) = 1000),
    Format('W: %f, H: %f', [FCanvas.SelectedItem.Width, FCanvas.SelectedItem.Height])
  );
end;

// S - 100 ũ�⸦ 20���� �ٿ����� 30�� �Ǿ�� �Ѵ�.
procedure TestTThLine.TestResizeMinimum;
var
  SizeP: TPointF;
begin
  DrawLine(50, 50, 150, 150);
  TestLib.RunMouseClick(100, 100);

  //ũ�� ����
  MousePath.New
  .Add(150, 150)
  .Add(180, 180)
  .Add(70, 70);
  TestLib.RunMousePath(MousePath.Path);

  SizeP := DistanceSize(RectF(500, 500, 800, 800), ItemMinimumSize / CanvasZoomScaleDefault);

  Check(Assigned(FCanvas.SelectedItem), 'Not assigned');

  Check(RoundTo(FCanvas.SelectedItem.Width, 3) = RoundTo(SizeP.X, 3), Format('W: %f', [FCanvas.SelectedItem.Width]));
end;

initialization
  RegisterTest(TestTThLine.Suite);

end.
