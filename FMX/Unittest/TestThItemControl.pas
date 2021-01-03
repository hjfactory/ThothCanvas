unit TestThItemControl;

interface

uses
  TestFramework, BaseTestUnit, FMX.Platform, FMX.StdCtrls,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #140 �������� ĵ�������� �����Ѵ�.
  TestTThItemControl = class(TThCanvasBaseTestUnit)
  published
    // #141 ������ ���� �� ĵ������ ǥ�õ��� �ʾƾ� �Ѵ�.
    procedure TestItemDelete;

    // #142 ������ ���� �� ������ ���ڰ� �����ؾ� �Ѵ�.
    procedure TestItemDeleteCheckContentsCount;

    // #65 Shift Ű�� ������ ��ü�� �����ϸ� ������ �߰��ȴ�.
    procedure TestItemMultiSelectShiftKey;

    // #67 ĵ������ �����ϸ� ��� ������ ��ҵȴ�.
    procedure TestItemMultiSelectionCancel;

    // #66 Shift Ű�� ������ �̹� ���õ� ��ü�� �����ϸ� ������ ��ҵȴ�.
    procedure TestItemMultiUnselect;

    // #146 ���� ���� �� �̵��Ǿ�� �Ѵ�.
    procedure TestMultiselectAndMove;

    // #147 ���� ���� �� ������ �� �־�� �Ѵ�.
    procedure TestMultiselectAndDelete;

    // #151 ���߼��� �� Shift ���� �� ������ Tracking ����
    procedure BugTestMultiselectShiftMove;

    // #150 �ٸ���Ʈ�ѿ��� Shift ���� �� ������ ���� �� ������ ���õ��� ����
    procedure BugTestAnotherContrlShfitPressAndMultiselect;

    // #158 A������ ���� �� Shift�� B �ߺ����� �� B�� �ܵ����õǾ�� �Ѵ�.
    procedure TestItemDeleteAndSelectionClear;

    // #168 Selected �� Selection�� �ݿ��Ǿ� �Ѵ�.
    procedure TestItemSelectToProcessSelction;

    // #169 ���콺 Ŭ������ selected �� Selection�� �ݿ��Ǿ� �Ѵ�.
    procedure TestItemMouseClickToProcessSelction;

    // #171 ���� ���� �� Selection�� �ݿ��Ǿ� �Ѵ�.
    procedure TestItemSelectToShowSelection;
    procedure TestItemUnselectToHideSelection;

    // #172 �ٸ������� ���� �� ������ �̵� �� ������ ����Ǿ� �Ѵ�.
    procedure TestAnotherSelectedAndMoveChangeSelection;

    // #173 2���� ������ ���� ���¿��� �̵� �� 2�� ������ �����Ǿ�� �Ѵ�.
    procedure TestMultiselectionAndMove;

    // #174 2���� ������ ���� ���¿��� �ϳ� Ŭ�� �� Ŭ���� ������ ��������
    procedure TestMultiselectAndShiftClickSingleSelect;

    // #184 ���� ���� �� ��� �����ۿ� �ܰ����� ǥ�õǾ�� �Ѵ�.
    procedure TestMultiselectDrawOutline;

    // #186 ���� ���� ���� �� �ϳ��� ���õǴ� ��� ResizeSpot�� ǥ�õǾ�� �Ѵ�.
    procedure TestMultiselectAndUnselectResizeSpot;

    // #185 ���� ���õ� ResizeSpot�� ���콺�� ũ�� ������ ���� �ʾƾ� �Ѵ�.
    procedure TestMultiselectCantResize;

    // #182 ���� ���� �� ���̶���Ʈ�� ���콺������ ��츸 ǥ�õǾ� �Ѵ�.
    procedure TestShowHighlightOnlyMouseOver;

    // #229 ������ Resize(ResizeSpot mouse over) �� Highligher�� ������
    procedure BugHighlightResizeSpotMouserOver;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor, ThConsts,
  ThItem, ThShapeItem, ThItemFactory, FMX.Controls, UITypes;

{ TestTThItemDelete }

procedure TestTThItemControl.TestItemDelete;
begin
  DrawRectangle(10, 10, 100, 100);

  TestLib.RunMouseClick(50, 50);

  FCanvas.DeleteSelection;

  Check(not Assigned(FCanvas.SelectedItem), 'Delete selection');

  TestLib.RunMouseClick(50, 50);

  Check(not Assigned(FCanvas.SelectedItem), 'Not selected item');
end;

procedure TestTThItemControl.TestItemDeleteCheckContentsCount;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(20, 20, 110, 110);
  DrawRectangle(30, 30, 120, 120);

  TestLib.RunMouseClick(50, 50);

  FCanvas.DeleteSelection;

  Check(not Assigned(FCanvas.SelectedItem), 'Delete selection');

  TestLib.RunMouseClick(50, 50);

  Check(FCanvas.ItemCount = 2);
end;

procedure TestTThItemControl.TestItemMultiSelectShiftKey;
begin
  DrawRectangle(10, 10, 100, 100);

  DrawRectangle(150, 150, 250, 250);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(200, 200);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2);
end;

procedure TestTThItemControl.TestItemMultiSelectionCancel;
begin
  DrawRectangle(10, 10, 100, 100);

  DrawRectangle(150, 150, 250, 250);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(200, 200);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2, 'Select');

  TestLib.RunMouseClick(110, 10);

  Check(FCanvas.SelectionCount = 0, 'Unselect');
end;

procedure TestTThItemControl.TestItemMultiUnselect;
begin
  DrawRectangle(10, 10, 100, 100);

  DrawRectangle(150, 150, 250, 250);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(170, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2, 'Select');

  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(201, 200);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 1, 'Unselect');
end;

procedure TestTThItemControl.TestMultiselectAndMove;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 200, 200);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(170, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2, 'Select');

  MousePath.New
  .Add(170, 180)
  .Add(170, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  TestLib.RunMouseClick(0, 0);
  TestLib.RunMouseClick(50, 50);

  Check(Assigned(FCanvas.SelectedItem), 'Assigned');
  Check(Round(FCanvas.SelectedItem.Position.X) = -110, Format('X: %f', [FCanvas.SelectedItem.Position.X]));
end;

procedure TestTThItemControl.TestMultiselectAndDelete;
begin
  DrawRectangle(10, 10, 50, 50);
  DrawRectangle(10, 60, 50, 100);
  DrawRectangle(110, 110, 200, 200);
  DrawRectangle(10, 110, 70, 200);

  TestLib.RunMouseClick(40, 40);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(170, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2, 'Select');

  FCanvas.DeleteSelection;

  Check(not Assigned(FCanvas.SelectedItem), 'Delete selection');

  TestLib.RunMouseClick(50, 50);

  Check(FCanvas.ItemCount = 2);
end;

procedure TestTThItemControl.BugTestMultiselectShiftMove;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2, 'Select');
{
  TestLib.RunMouseClick(1, 1);
  TestLib.RunMouseClick(170, 175);
  Check(FCanvas.SelectionCount = 1, 'Select2');
exit;
}
  TestLib.RunKeyDownShift;
  MousePath.New
  .Add(150, 150)
  .Add(171, 181)
  .Add(172, 182)
  .Add(173, 183)
  .Add(174, 184)
  .Add(180, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);
  TestLib.RunKeyUpShift;

  TestLib.RunMouseClick(0, 0);
  TestLib.RunMouseClick(100, 100);

  Check(Assigned(FCanvas.SelectedItem), 'Assigned');
  Check(Round(FCanvas.SelectedItem.Position.X) = -90, Format('Item.X: %f', [FCanvas.SelectedItem.Position.X]));

  TestLib.RunMouseClick(200, 200);
  Check(Assigned(FCanvas.SelectedItem), 'Assigned 2');
  Check(Round(FCanvas.SelectedItem.Position.X) = 10, Format('Item2.X: %f', [FCanvas.SelectedItem.Position.X]));
end;

procedure TestTThItemControl.BugTestAnotherContrlShfitPressAndMultiselect;
var
  Button: TButton;
begin
  Button := TButton.Create(FForm);
  Button.Parent := FForm;
  Button.Position.Point := PointF(0,0);
  Button.Width := 50;
  Button.Height := 100;
  Button.SendToBack;

  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;

  // Button click
  TestLib.RunMouseClick(-10, -10);

  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 2);
end;

procedure TestTThItemControl.TestItemDeleteAndSelectionClear;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);

  FCanvas.DeleteSelection;

  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyUpShift;

  Check(FCanvas.SelectionCount = 1);
end;

procedure TestTThItemControl.TestItemSelectToProcessSelction;
begin
  DrawRectangle(10, 10, 100, 100);

  TestLib.RunMouseClick(50, 50);

  CheckNotNull(FCanvas.SelectedItem, 'Click');

  FCanvas.SelectedItem.Selected := False;

  CheckNull(FCanvas.SelectedItem, 'Unselectd');
end;

procedure TestTThItemControl.TestItemMouseClickToProcessSelction;
begin
  DrawRectangle(10, 10, 100, 100);

  TestLib.RunMouseClick(50, 50);
  CheckNotNull(FCanvas.SelectedItem, 'Click');

  TestLib.RunMouseClick(150, 150);
  CheckNull(FCanvas.SelectedItem, 'Unselectd');
end;

procedure TestTThItemControl.TestItemSelectToShowSelection;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 10) = ItemResizeSpotOutColor, Format('Not matching color TopLeft(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotOutColor]));
end;

procedure TestTThItemControl.TestItemUnselectToHideSelection;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunMouseClick(150, 150);
  Check(FCanvas.SelectionCount = 1, Format('Count: %d', [FCanvas.SelectionCount]));
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 10) <> ItemResizeSpotOutColor, Format('Not matching color TopLeft(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotOutColor]));
end;

procedure TestTThItemControl.TestAnotherSelectedAndMoveChangeSelection;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);

  MousePath.New
  .Add(150, 150)
  .Add(180, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);
  Check(FCanvas.SelectionCount = 1, Format('Count: %d', [FCanvas.SelectionCount]));
  Check(Round(FCanvas.SelectedItem.Position.X) = 10, Format('Change selection failed X: %f', [FCanvas.SelectedItem.Position.X]));
  Check(FCanvas.SelectedItem.Width = 70, 'Change selection failed');
end;

procedure TestTThItemControl.TestMultiselectionAndMove;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(150, 150);
  TestLib.RunKeyUpShift;
  Check(FCanvas.SelectionCount = 2, Format('Selection Count: %d', [FCanvas.SelectionCount]));

  MousePath.New
  .Add(150, 150)
  .Add(180, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);
  Check(FCanvas.SelectionCount = 2, Format('Count: %d', [FCanvas.SelectionCount]));
end;

procedure TestTThItemControl.TestMultiselectAndShiftClickSingleSelect;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(150, 150);
  TestLib.RunKeyUpShift;
  Check(FCanvas.SelectionCount = 2, Format('Selection Count: %d', [FCanvas.SelectionCount]));

  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(160, 160);
  TestLib.RunKeyUpShift;
  Check(FCanvas.SelectionCount = 1, Format('Count: %d', [FCanvas.SelectionCount]));
  Check(FCanvas.SelectedItem.Position.X = -140, FOrmat('Position %f',[FCanvas.SelectedItem.Position.X]));
end;

procedure TestTThItemControl.TestMultiselectDrawOutline;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyUpShift;

  // ResizeSpot ǥ�� Ȯ��
  Check(TestLib.GetControlPixelColor(FCanvas, 10, 10) = ItemResizeSpotDisableColor,   Format('Not matching color TopLeft(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotDisableColor]));
  Check(TestLib.GetControlPixelColor(FCanvas, 180, 180) = ItemResizeSpotDisableColor, Format('Not matching color BottomRight(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotDisableColor]));
end;

procedure TestTThItemControl.TestMultiselectAndUnselectResizeSpot;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(50, 50);
  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyUpShift;

  Check(TestLib.GetControlPixelColor(FCanvas, 180, 180) = ItemResizeSpotOutColor, Format('Not matching color BottomRight(%d, %d)', [TestLib.GetControlPixelColor(FCanvas, 10, 10), ItemResizeSpotOutColor]));
end;

procedure TestTThItemControl.TestMultiselectCantResize;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyUpShift;

  MousePath.New
  .Add(180, 180)
  .Add(180, 200)
  .Add(200, 200);
  TestLib.RunMousePath(MousePath.Path);

  FCanvas.ClearSelection;
  TestLib.RunMouseClick(150, 150);
  CheckNotNull(FCanvas.SelectedItem);
  Check(FCanvas.SelectedItem.Width = 70, Format('Width: %f', [FCanvas.SelectedItem.Width]));
end;

procedure TestTThItemControl.TestShowHighlightOnlyMouseOver;
var
  AC: TAlphaColor;
begin
  DrawRectangle(10, 10, 100, 100);
  DrawRectangle(110, 110, 180, 180);

  TestLib.RunMouseClick(169, 170);
  TestLib.RunKeyDownShift;
  TestLib.RunMouseClick(50, 50);
  TestLib.RunKeyUpShift;

  MousePath.New
  .Add(250, 250);
  TestLib.RunMouseMove(MousePath.Path);

  AC := TestLib.GetControlPixelColor(FCanvas, 20, 100 + (ItemHighlightSize - 1));
  Check(AC <> ItemHighlightColor);
end;

procedure TestTThItemControl.BugHighlightResizeSpotMouserOver;
var
  AC: TAlphaColor;
begin
Exit;

  DrawRectangle(10, 10, 100, 100);
  TestLib.RunMouseClick(50, 50);

  TestLib.RunMouseMove(MousePath.Add(100, 100).Path);


  AC := TestLib.GetControlPixelColor(FCanvas, 50, 100 + (ItemHighlightSize - 1));
  Check(AC = ItemHighlightColor);
end;

initialization
  RegisterTest(TestTThItemControl.Suite);

end.


