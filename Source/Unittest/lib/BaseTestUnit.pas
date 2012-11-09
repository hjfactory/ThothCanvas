unit BaseTestUnit;

interface

uses
  TestFramework, ThCanvasEditor, ThothController, ThCanvasController,
  System.Types, FMX.Types, FMX.Forms, System.SysUtils;

type
  // Test methods for class TThCanvasEditor

  TBaseTestUnit = class(TTestCase)
  protected
    FClosing: Boolean;
    FForm: TForm;
    FCanvas: TThCanvasEditor;
    function GetInitialPoint: TPointF;
    procedure SetTestControl(var FormRect, CanvasRect: TRectF); virtual;

    procedure FormDestroy(Sender: TObject);
  private
    procedure CreateObject; virtual;
    procedure DestroyObject; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure ShowForm;
    function DistanceSize(R: TRectF; D: Single): TPointF;

    procedure DrawRectangle(Left, Top, Right, Bottom: Single); overload;
    procedure DrawRectangle(R: TRectF); overload;

    procedure DrawLine(Left, Top, Right, Bottom: Single); overload;
    procedure DrawLine(R: TRectF); overload;

    procedure DrawCircle(Left, Top, Right, Bottom: Single); overload;
    procedure DrawCircle(R: TRectF); overload;
  end;

  TBaseCommandTestUnit = class(TBaseTestUnit)
  private
    procedure CreateObject; override;
    procedure DestroyObject; override;
  protected
    FThothController: TThothController;
    FCanvasController: TThCanvasEditorController;
  end;

implementation

uses
  UnitTestForm, FMX.Platform, FMX.TestLib, ThConsts;

{ TBastTestUnit }

function TBaseTestUnit.GetInitialPoint: TPointF;
begin
  Result := IControl(FCanvas).LocalToScreen(PointF(0, 0));
end;

procedure TBaseTestUnit.SetTestControl(var FormRect, CanvasRect: TRectF);
begin
  FormRect.Top := 300;
  FormRect.Left := 300;
  FormRect.Width := 600;
  FormRect.Height := 600;

  CanvasRect := RectF(50, 50, 350, 350);
end;

procedure TBaseTestUnit.SetUp;
var
  FormRect, CanvasRect: TRectF;
begin
  FClosing := True;

  SetTestControl(FormRect, CanvasRect);

  FForm := TfrmUnitTest.Create(nil);
  FForm.Top     := Round(FormRect.Top);
  FForm.Left    := Round(FormRect.Left);
  FForm.Width   := Round(FormRect.Width);
  FForm.Height  := Round(FormRect.Height);
  FForm.OnDestroy := FormDestroy;
  FForm.Show;

  FCanvas := TThCanvasEditor.Create(FForm);
  FCanvas.Parent := FForm;
  FCanvas.Position.Point  := CanvasRect.TopLeft;
  FCanvas.Width           := CanvasRect.Width;
  FCanvas.Height          := CanvasRect.Height;

  CreateObject;

  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;
end;

procedure TBaseTestUnit.TearDown;
begin
  if not FClosing then
    Exit;

  FForm.Free;
end;

procedure TBaseTestUnit.FormDestroy(Sender: TObject);
begin
  DestroyObject;
end;

procedure TBaseTestUnit.CreateObject;
begin
end;

procedure TBaseTestUnit.DestroyObject;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TBaseTestUnit.ShowForm;
begin
  FClosing := False;
end;

procedure TBaseTestUnit.DrawLine(Left, Top, Right, Bottom: Single);
begin
  DrawLine(RectF(Left, Top, Right, Bottom));
end;

function TBaseTestUnit.DistanceSize(R: TRectF; D: Single): TPointF;
var
  Rad: Single;
begin
  Rad := ArcTan(R.Height / R.Width);

  Result := PointF(Cos(Rad) * D, Sin(Rad) * D);
end;

procedure TBaseTestUnit.DrawCircle(Left, Top, Right, Bottom: Single);
begin
  DrawCircle(RectF(Left, Top, Right, Bottom));
end;

procedure TBaseTestUnit.DrawCircle(R: TRectF);
begin
  FCanvas.DrawItemID := ItemFactoryIDCircle;
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.CenterPoint)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);
end;

procedure TBaseTestUnit.DrawLine(R: TRectF);
begin
  FCanvas.DrawItemID := ItemFactoryIDLine;   // 1100 is Rectangles ID
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.Left + 1, R.Top)
  .Add(R.CenterPoint)
  .Add(R.Left, R.Top + 1)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);
end;

procedure TBaseTestUnit.DrawRectangle(Left, Top, Right, Bottom: Single);
begin
  DrawRectangle(RectF(Left, Top, Right, Bottom));
end;

procedure TBaseTestUnit.DrawRectangle(R: TRectF);
begin
  FCanvas.DrawItemID := ItemFactoryIDRectangle;
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.CenterPoint)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);
end;

{ TBaseCommandTestUnit }

procedure TBaseCommandTestUnit.CreateObject;
begin
  inherited;

  FThothController := TThothController.Create;
  FCanvasController := TThCanvasEditorController.Create;
  FCanvasController.SetSubject(FThothController);
  FCanvasController.SetThCanvas(FCanvas);
end;

procedure TBaseCommandTestUnit.DestroyObject;
begin
  inherited;

  FCanvasController.Free;
  FThothController.Free;
end;

end.
