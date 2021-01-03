unit BaseTestUnit;

interface

uses
  UnitTestForm,
  TestFramework, ThCanvasEditor, ThothController, ThCanvasController,
  System.Types, System.Classes, FMX.Types, FMX.Forms, System.SysUtils, ThItem;

type
  // Test methods for class TThCanvasEditor

  TThCanvasBaseTestUnit = class(TTestCase)
  protected
    FClosing: Boolean;
    FForm: TfrmUnitTest;
    FCanvas: TThCanvasEditor;

    procedure CreateObject; virtual;
    procedure DestroyObject; virtual;

    function GetInitialPoint: TPointF;
    procedure SetTestControl(var FormRect, CanvasRect: TRectF); virtual;

    procedure FormDestroy(Sender: TObject);

    function GetCenterPos: TPointF;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure Fail(msg: string; ErrorAddrs: Pointer = nil); override;

    procedure ShowForm;

    function DistanceSize(R: TRectF; D: Single): TPointF;

    function DrawRectangle(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem; overload;
    function DrawRectangle(TopLeft, BottomRight: TPointF; AName: string = ''): TThItem; overload;
    function DrawRectangle(R: TRectF; AName: string = ''): TThItem; overload;

    function DrawLine(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem; overload;
    function DrawLine(R: TRectF; AName: string = ''): TThItem; overload;

    function DrawCircle(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem; overload;
    function DrawCircle(R: TRectF; AName: string = ''): TThItem; overload;

    function GetItem(X, Y: Single): TThItem; overload;
    function GetItem(Pt: TPointF): TThItem; overload;
    function GetImagePath: string;

    property CenterPos: TPointF read GetCenterPos;
  end;

  TBaseCommandHistoryTestUnit = class(TThCanvasBaseTestUnit)
  protected
    FThothController: TThothController;
    FCanvasController: TThCanvasEditorController;

    procedure CreateObject; override;
    procedure DestroyObject; override;
  end;

implementation

uses
  FMX.Platform, FMX.Graphics, FMX.TestLib, ThConsts;

{ TBastTestUnit }

function TThCanvasBaseTestUnit.GetCenterPos: TPointF;
begin
  Result := FCanvas.BoundsRect.CenterPoint;
end;

function TThCanvasBaseTestUnit.GetImagePath: string;
var
  Bitmap: TBitmap;
  Stream: TResourceStream;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'TEST_IMAGE.PNG';
  // Create Image file from resource
  if not FileExists(Result) then
  begin
    Bitmap := TBitmap.Create(0, 0);
    Stream := TResourceStream.Create(HInstance, 'TEST_IMAGE', RT_RCDATA);
    try
      Bitmap.LoadFromStream(Stream);
      Bitmap.SaveToFile(Result);
    finally
      Bitmap.Free;
      Stream.Free;
    end;
  end;
end;

function TThCanvasBaseTestUnit.GetInitialPoint: TPointF;
begin
  Result := IControl(FCanvas).LocalToScreen(PointF(0, 0));
end;

function TThCanvasBaseTestUnit.GetItem(Pt: TPointF): TThItem;
begin
  Result := GetItem(Pt.X, Pt.Y);
end;

function TThCanvasBaseTestUnit.GetItem(X, Y: Single): TThItem;
begin
  TestLib.RunMouseClick(X, Y);
  Result := FCanvas.SelectedItem;
end;

procedure TThCanvasBaseTestUnit.SetTestControl(var FormRect, CanvasRect: TRectF);
begin
  FormRect.Top := 300;
  FormRect.Left := 300;
  FormRect.Width := 600;
  FormRect.Height := 600;

  CanvasRect := RectF(50, 50, 350, 350);
end;

procedure TThCanvasBaseTestUnit.SetUp;
var
  FormRect, CanvasRect: TRectF;
begin
  FClosing := True;

  SetTestControl(FormRect, CanvasRect);

  FForm := TfrmUnitTest.Create(Application);
  FForm.Top     := Round(FormRect.Top);
  FForm.Left    := Round(FormRect.Left);
  FForm.Width   := Round(FormRect.Width);
  FForm.Height  := Round(FormRect.Height);
  FForm.OnDestroy := FormDestroy;
  FForm.Show;

  FCanvas := TThCanvasEditor.Create(FForm);
  FCanvas.Parent := FForm.Panel1;
  FCanvas.Position.Point  := CanvasRect.TopLeft;
//  FCanvas.Align := TAlignLayout.Client;
  FCanvas.Width           := CanvasRect.Width;
  FCanvas.Height          := CanvasRect.Height;
  FCanvas.Initialize;

  CreateObject;

  TestLib.SetInitialMousePoint(GetInitialPoint);
  Application.ProcessMessages;
end;

procedure TThCanvasBaseTestUnit.TearDown;
begin
  if not FClosing then
    Exit;

  FForm.Free;
end;

procedure TThCanvasBaseTestUnit.Fail(msg: string; ErrorAddrs: Pointer);
begin
  ShowForm;

  inherited;
end;

procedure TThCanvasBaseTestUnit.FormDestroy(Sender: TObject);
begin
  DestroyObject;
end;

procedure TThCanvasBaseTestUnit.CreateObject;
begin
end;

procedure TThCanvasBaseTestUnit.DestroyObject;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TThCanvasBaseTestUnit.ShowForm;
begin
  FClosing := False;
end;

function TThCanvasBaseTestUnit.DistanceSize(R: TRectF; D: Single): TPointF;
var
  Rad: Single;
begin
  Rad := ArcTan(R.Height / R.Width);

  Result := PointF(Cos(Rad) * D, Sin(Rad) * D);
end;

function TThCanvasBaseTestUnit.DrawCircle(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem;
begin
  Result := DrawCircle(RectF(Left, Top, Right, Bottom), AName);
end;

function TThCanvasBaseTestUnit.DrawCircle(R: TRectF; AName: string = ''): TThItem;
begin
  FCanvas.DrawItemID := ItemFactoryIDCircle;
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.CenterPoint)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);

  Result := GetItem(R.CenterPoint);
  if Assigned(Result) then
    Result.Name := AName;
end;

function TThCanvasBaseTestUnit.DrawLine(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem;
begin
  Result := DrawLine(RectF(Left, Top, Right, Bottom), AName);
end;

function TThCanvasBaseTestUnit.DrawLine(R: TRectF; AName: string = ''): TThItem;
begin
  FCanvas.DrawItemID := ItemFactoryIDLine;   // 1100 is Rectangles ID
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.Left + 1, R.Top)
  .Add(R.CenterPoint)
  .Add(R.Left, R.Top + 1)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);

  Result := GetItem(R.CenterPoint);
  if Assigned(Result) then
    Result.Name := AName;
end;

function TThCanvasBaseTestUnit.DrawRectangle(TopLeft, BottomRight: TPointF;
  AName: string): TThItem;
begin
  Result := DrawRectangle(RectF(TopLeft.X, TopLeft.Y, BottomRight.X, BottomRight.Y), AName);
end;

function TThCanvasBaseTestUnit.DrawRectangle(Left, Top, Right, Bottom: Single; AName: string = ''): TThItem;
begin
  Result := DrawRectangle(RectF(Left, Top, Right, Bottom), AName);
end;

function TThCanvasBaseTestUnit.DrawRectangle(R: TRectF; AName: string = ''): TThItem;
begin
  FCanvas.DrawItemID := ItemFactoryIDRectangle;
  MousePath.New
  .Add(R.TopLeft)
  .Add(R.CenterPoint)
  .Add(R.BottomRight);
  TestLib.RunMousePath(MousePath.Path);

//  Result := GetItem(R.CenterPoint);
  Result := FCanvas.SelectedItem;
  if Assigned(Result) then
    Result.Name := AName;
end;

{ TBaseCommandTestUnit }

procedure TBaseCommandHistoryTestUnit.CreateObject;
begin
  inherited;

  FThothController := TThothController.Create;
  FCanvasController := TThCanvasEditorController.Create(FCanvas);
  FCanvasController.SetSubject(FThothController);
//  FCanvasController.SetThCanvas(FCanvas);
end;

procedure TBaseCommandHistoryTestUnit.DestroyObject;
begin
  inherited;

  FCanvasController.Free;
  FThothController.Free;
end;

end.
