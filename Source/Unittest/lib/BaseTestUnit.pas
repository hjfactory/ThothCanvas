unit BaseTestUnit;

interface

uses
  TestFramework, ThCanvasEditor,
  System.Types, FMX.Types, FMX.Forms;

type
  // Test methods for class TThCanvasEditor

  TBaseTestUnit = class(TTestCase)
  private
    FPos: TPointF;
  protected
    FForm: TForm;
    FCanvas: TThCanvasEditor;
    function GetInitialPoint: TPointF;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  UnitTestForm, FMX.Platform;

{ TBastTestUnit }

function TBaseTestUnit.GetInitialPoint: TPointF;
begin
  Result := IControl(FCanvas).LocalToScreen(PointF(0, 0));
end;

procedure TBaseTestUnit.SetUp;
begin
  FForm := TfrmUnitTest.Create(nil);
  FForm.Width := 600;
  FForm.Height := 600;
  FForm.Top := 300;
  FForm.Left := 300;
  FForm.Show;

  FCanvas := TThCanvasEditor.Create(FForm);
  FCanvas.Parent := FForm;
  FCanvas.Width := 300;
  FCanvas.Height := 300;
  FCanvas.Position.Point := PointF(50, 50);

  Application.ProcessMessages;
end;

procedure TBaseTestUnit.TearDown;
begin
  FCanvas.Free;
  FForm.Free;
end;

end.
