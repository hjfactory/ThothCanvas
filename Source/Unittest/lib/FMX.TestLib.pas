unit FMX.TestLib;

interface

uses
  System.Types, System.UITypes, System.SysUtils, System.Classes, FMX.Types, FMX.Forms, FMX.Objects,
  System.UIConsts;

type
  PointFArray = array of array[0..1] of Single;

  TTestMousePath = class;

  TTestLib = class(TObject)
  protected
    FInitialMousePoint: TPointF;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseWheel(WheelDelta: Integer); virtual; abstract;

    procedure KeyDown(Key: Word); virtual; abstract;
    procedure KeyUp(Key: Word); virtual; abstract;
  public
    procedure SetInitialMousePoint(Pos: TPointF);

    procedure RunMouseClick(X, Y: Single); overload;
    procedure RunMouseClick(Pt: TPointF); overload;

    procedure RunMousePath(AMousePathData: TTestMousePath); overload;
    procedure RunMousePath(Path: array of TPointF); overload;
    procedure RunMouseMove(Path: array of TPointF);

    procedure RunKeyDownShift; virtual; abstract;
    procedure RunKeyUpShift; virtual; abstract;
    procedure RunKeyPress(Keys: array of Word);

    procedure RunMouseWheelUp(X, Y: Single);
    procedure RunMouseWheelDown(X, Y: Single);

    function GetControlPixelColor(const Control: TControl; const X, Y: Single): TAlphaColor;
    function GetBitmapPixelColor(const X, Y: Single): TAlphaColor;

    procedure TakeScreenshot(Dest: TBitmap); virtual; abstract;

    procedure Delay(ms: Integer);
  end;

  TTestLibClass = class of TTestLib;

  TMousePathData = array of TPointF;
  TTestMousePath = class
  private
    FPaths: array of TPointF;
    function GetPath: TMousePathData;
    function GetAddPoint(const X, Y: Single): TTestMousePath;
  public
    procedure Clear;
    function New: TTestMousePath;
    function Add(const X, Y: Single): TTestMousePath; overload;
    function Add(Pos: TPointF): TTestMousePath; overload;
    property Path: TMousePathData read GetPath;
    property AddPoint[const X, Y: Single]: TTestMousePath read GetAddPoint; default;
  end;

var
  TestLib: TTestLib;
  MousePath: TTestMousePath;

implementation

uses
  System.Diagnostics,
{$IFDEF MACOS}
  FMX.TestLib.Mac;
{$ENDIF}

{$IFDEF MSWINDOWS}
  FMX.TestLib.Win;
{$ENDIF}

{ TTestLib }

procedure TTestLib.SetInitialMousePoint(Pos: TPointF);
begin
  FInitialMousePoint := Pos;
end;

procedure TTestLib.Delay(ms: Integer);
var
  StopWatch: TStopWatch;
begin
  StopWatch := TStopWatch.Create;

  StopWatch.Start;
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until StopWatch.ElapsedMilliseconds >= ms;
end;

function TTestLib.GetBitmapPixelColor(const X,
  Y: Single): TAlphaColor;
var
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
begin
  Result := claBlack;

  Application.ProcessMessages;
  Sleep(0);

  Bitmap := TBitmap.Create(0, 0);
  TakeScreenshot(Bitmap);

  try
    if not Assigned(Bitmap) then
      Exit;
//    Bitmap.SaveToFile('C:\Users\hjFactory\Downloads\test.bmp');
    Bitmap.Map(TMapAccess.maRead, BitmapData);
    try
      Result := BitmapData.GetPixel(Round(X), Round(Y));
    finally
      Bitmap.Unmap(BitmapData);
    end;
  finally
    Bitmap.Free;
  end;
end;

function TTestLib.GetControlPixelColor(const Control: TControl; const X, Y: Single): TAlphaColor;
var
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
begin
  Result := claBlack;

//  Control.PaintTo();

  Bitmap := Control.MakeScreenshot;
  try
    if not Assigned(Bitmap) then
      Exit;
    Bitmap.Map(TMapAccess.maRead, BitmapData);
    try
      Result := BitmapData.GetPixel(Round(X), Round(Y));
    finally
      Bitmap.Unmap(BitmapData);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TTestLib.RunKeyPress(Keys: array of Word);
var
  I: Integer;
  W: Word;
begin
  for W in Keys do
    KeyDown(W);
  for I := Length(Keys) - 1 downto 0 do
    KeyUp(Keys[I]);
end;

procedure TTestLib.RunMouseClick(X, Y: Single);
begin
  MouseDown(TMouseButton.mbLeft, [], FInitialMousePoint.X+X, FInitialMousePoint.Y+Y);
  MouseUp(TMouseButton.mbLeft, [], FInitialMousePoint.X+X, FInitialMousePoint.Y+Y);
  Application.ProcessMessages;
end;

procedure TTestLib.RunMouseClick(Pt: TPointF);
begin
  RunMouseClick(Pt.X, Pt.Y);
end;

procedure TTestLib.RunMousePath(Path: array of TPointF);
var
  I: Integer;
  Pos: TPointF;
begin
  for I := Low(Path) to High(Path) do
  begin
    Pos := FInitialMousePoint.Add(Path[I]);

    if I = Low(Path) then
      MouseDown(TMouseButton.mbLeft, [], Pos.X, Pos.Y)
    else if I = High(Path) then
      MouseUp(TMouseButton.mbLeft, [], Pos.X, Pos.Y)
    else
      MouseMove([], Pos.X, Pos.Y)
    ;
    Application.ProcessMessages;
  end;
end;

procedure TTestLib.RunMouseMove(Path: array of TPointF);
var
  I: Integer;
  Pos: TPointF;
begin
  for I := Low(Path) to High(Path) do
  begin
    Pos := FInitialMousePoint.Add(Path[I]);

    MouseMove([], Pos.X, Pos.Y);
    Application.ProcessMessages;
  end;
end;

procedure TTestLib.RunMousePath(AMousePathData: TTestMousePath);
begin
  RunMousePath(AMousePathData.Path);
end;

procedure TTestLib.RunMouseWheelDown(X, Y: Single);
var
  Pos: TPointF;
begin
  Pos := FInitialMousePoint.Add(PointF(X, Y));

  MouseMove([], Pos.X, Pos.Y);
  MouseWheel(120);
end;

procedure TTestLib.RunMouseWheelUp(X, Y: Single);
var
  Pos: TPointF;
begin
  Pos := FInitialMousePoint.Add(PointF(X, Y));

  MouseMove([], Pos.X, Pos.Y);
  MouseWheel(-120);
end;

{ TTestMousePath }

procedure TTestMousePath.Clear;
begin
  SetLength(FPaths, 0);
end;

function TTestMousePath.Add(const X, Y: Single): TTestMousePath;
begin
  Add(PointF(X, Y));

  Result := Self;
end;

function TTestMousePath.Add(Pos: TPointF): TTestMousePath;
begin
  SetLength(Fpaths, Length(FPaths) + 1);
  FPaths[High(FPaths)] := Pos;

  Result := Self;
end;

function TTestMousePath.GetAddPoint(const X, Y: Single): TTestMousePath;
begin
  Result := Add(X, Y);
end;

function TTestMousePath.GetPath: TMousePathData;
begin
  Result := TMousePathData(FPaths);
end;

function TTestMousePath.New: TTestMousePath;
begin
  Clear;
  Result := Self;
end;

initialization
  TestLib := GetTestLibClass.Create;
  MousePath := TTestMousePath.Create;

finalization
  if Assigned(TestLib) then
    TestLib.Free;
  if Assigned(MousePath) then
    MousePath.Free;

end.
