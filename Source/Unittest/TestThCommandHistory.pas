unit TestThCommandHistory;

interface

uses
  TestFramework, BaseTestUnit,
  System.Types, System.SysUtils, FMX.Types, FMX.Objects, System.UIConsts;

type
  // #23 Undo/Redo기능을 이용하여 명령을 되돌린다.
  TestTThCommandHistory = class(TBaseTestUnit)
  published

  end;

implementation

uses
  UnitTestForm, FMX.TestLib, ThContainer, ThCanvasEditor,
  ThItem, ThShape, ThItemFactory;

initialization
  RegisterTest(TestTThCommandHistory.Suite);

end.

