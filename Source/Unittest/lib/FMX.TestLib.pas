unit FMX.TestLib;

interface

uses
  System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Forms;

type
  TTestLib = class(TObject)
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseWheel(WheelDelta: Integer); virtual; abstract;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar); virtual; abstract;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar); virtual; abstract;

    procedure RunMousePath(Path: array of TPointF);
  end;

  TTestLibClass = class of TTestLib;

type
  TMousePathData = array of TPointF;
  TTestMousePath = class
  private
    FInitialPoint: TPointF;
    FPaths: array of TPointF;
    function GetPath: TMousePathData;
  public
    procedure SetInitialPoint(Pos: TPointF);
    procedure Clear;
    procedure Add(const X, Y: Single); overload;
    procedure Add(Pos: TPointF); overload;
    property Path: TMousePathData read GetPath;
  end;

var
  TestLib: TTestLib;
  MousePath: TTestMousePath;

implementation

uses
{$IFDEF MACOS}
  FMX.TestLib.Mac;
{$ENDIF}

{$IFDEF MSWINDOWS}
  FMX.TestLib.Win;
{$ENDIF}

{ TTestLib }

procedure TTestLib.RunMousePath(Path: array of TPointF);
var
  I: Integer;
begin
  for I := Low(Path) to High(Path) do
  begin
    if I = Low(Path) then
      MouseDown(TMouseButton.mbLeft, [], Path[I].X, Path[I].Y)
    else if I = High(Path) then
      MouseUp(TMouseButton.mbLeft, [], Path[I].X, Path[I].Y)
    else
      MouseMove([], Path[I].X, Path[I].Y)
    ;
    Application.ProcessMessages;
  end;
end;

{ TTestMousePath }

procedure TTestMousePath.Clear;
begin
  SetLength(FPaths, 0);
end;

procedure TTestMousePath.Add(const X, Y: Single);
begin
  Add(PointF(X, Y));
end;

procedure TTestMousePath.Add(Pos: TPointF);
begin
  SetLength(Fpaths, Length(FPaths) + 1);
  FPaths[High(FPaths)] := FInitialPoint.Add(Pos);
end;

function TTestMousePath.GetPath: TMousePathData;
begin
  Result := TMousePathData(FPaths);
end;

procedure TTestMousePath.SetInitialPoint(Pos: TPointF);
begin
  FInitialPoint := Pos;
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
