unit TestThLine;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, ThShape, FMX.Types, FMX.Forms;

type
  // Test methods for class TThLine

  TestTThLine = class(TTestCase)
  strict private
    FForm: TForm;
    FThLine: TThLine;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
  end;

implementation

procedure TestTThLine.SetUp;
begin
  FForm := TForm.Create(nil);
  FForm.Width := 600;
  FForm.Height := 600;

  FThLine := TThLine.Create(nil);
end;

procedure TestTThLine.TearDown;
begin
  FThLine.Free;
  FThLine := nil;

  FForm.Free;
end;

procedure TestTThLine.TestAdd;
begin

end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTThLine.Suite);
end.
