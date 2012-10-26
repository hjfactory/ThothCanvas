unit TestThItemControl;

interface

uses
  TestFramework, BaseTestUnit, FMX.Platform,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #140 아이템을 캔버스에서 제거한다.
  TestTThItemControl = class(TBaseTestUnit)
  published
    // #141 아이템 삭제 후 캔버스에 표시되지 않아야 한다.
    procedure TestItemDelete;

    // #142 아이템 삭제 후 컨텐츠 숫자가 감소해야 한다.
    procedure TestItemDeleteCheckContentsCount;

    // #65 Shift 키를 누르고 객체를 선택하면 선택이 추가된다.
    procedure TestItemMultiSelectShiftKey;

    // #67 캔버스를 선택하면 모든 선택이 취소된다.
    procedure TestItemMultiSelectionCancel;

    // #66 Shift 키를 누르고 이미 선택된 객체를 선택하면 선택이 취소된다.
    procedure TestItemMultiUnselect;

    // #146 다중 선택 후 이동되어야 한다.
    procedure TestMultiselectAndMove;

    // #147 다중 선택 후 삭제할 수 있어야 한다.
    procedure TestMultiselectAndDelete;

    // #151 다중선택 후 Shift 누른 후 아이템 Tracking 오류
    procedure BugTestMultiselectShiftMove;

    // #150 다른컨트롤에서 Shift 누른 후 여러개 선택 시 여러개 선택되지 않음
    procedure BugTestAnotherContrlShfitPressAndMultiselect;

    // #158 A아이템 삭제 후 Shift로 B 중복선택 시 B가 단독선택되어야 한다.
    procedure TestItemDeleteAndSelectionClear;

    // #168 Selected 시 Selection에 반영되야 한다.
    procedure TestItemSelectToProcessSelction;

    // #169 마우스 클릭으로 selected 시 Selection에 반영되야 한다.
    procedure TestItemMouseClickToProcessSelction;

    // #171 다중 선택 시 Selection에 반영되야 한다.
    procedure TestItemSelectToShowSelection;
    procedure TestItemUnselectToHideSelection;

    // #172 다른아이템 선택 시 아이템 이동 시 선택이 변경되야 한다.
    procedure TestAnotherSelectedAndMoveChangeSelection;

    // #173 2개의 아이템 선택 상태에서 이동 시 2개 선택이 유지되어야 한다.
    procedure TestMultiselectionAndMove;

    // #174 2개의 아이템 선택 상태에서 하나 클릭 시 클릭한 아이템 선택해제
    procedure TestMultiselectAndShiftClickSingleSelect;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor, ThConsts,
  ThItem, ThShape, ThItemFactory, CommonUtils, FMX.Controls;

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
  Check(FCanvas.SelectedItem.Position.X = 40, Format('X: %f', [FCanvas.SelectedItem.Position.X]));
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
  Check(FCanvas.SelectedItem.Position.X = 60, Format('Item.X: %f', [FCanvas.SelectedItem.Position.X]));

  TestLib.RunMouseClick(200, 200);
  Check(Assigned(FCanvas.SelectedItem), 'Assigned 2');
  Check(FCanvas.SelectedItem.Position.X = 160, Format('Item2.X: %f', [FCanvas.SelectedItem.Position.X]));
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
  Check(FCanvas.SelectedItem.Position.X = 160, 'Change selection failed');
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
  Check(FCanvas.SelectedItem.Position.X = 10, FOrmat('Position %f',[FCanvas.SelectedItem.Position.X]));
end;

initialization
  RegisterTest(TestTThItemControl.Suite);

end.


