unit TestThCommandHistory;

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #23 Undo/Redo기능을 이용하여 명령을 되돌린다.
  TestTThCommandHistory = class(TBaseCommandTestUnit)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // #23 Basic test
    procedure TestCommandHistoryAdd;
    procedure TestCommandHistoryDelete;
    procedure TestCommandHistoryMove;
    procedure TestCommandHistoryResize;
  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThCanvas, ThCanvasEditor,
  ThItem, ThShape, ThItemFactory;

{ TestTThCommandHistory }

procedure TestTThCommandHistory.SetUp;
begin
  inherited;

end;

procedure TestTThCommandHistory.TearDown;
begin
  inherited;

end;

procedure TestTThCommandHistory.TestCommandHistoryAdd;
begin
//  ShowForm;

  DrawRectangle(10, 10, 100, 100);

  FThothController.Undo;

  TestLib.RunMouseClick(50, 50);
  CheckNull(FCanvas.SelectedItem, 'Undo');

  FThothController.Redo;
  CheckNotNull(FCanvas.SelectedItem, 'Redo');
end;

procedure TestTThCommandHistory.TestCommandHistoryDelete;
begin

end;

procedure TestTThCommandHistory.TestCommandHistoryMove;
begin

end;

procedure TestTThCommandHistory.TestCommandHistoryResize;
begin

end;

initialization
  RegisterTest(TestTThCommandHistory.Suite);

end.

