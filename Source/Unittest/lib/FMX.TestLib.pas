unit FMX.TestLib;

interface

uses
  System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Forms, FMX.Platform.Win;

type
  TTestLib = class(TObject)
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure MouseWheel(WheelDelta: Integer); virtual; abstract;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar); virtual; abstract;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar); virtual; abstract;
  end;

  TTestLibClass = class of TTestLib;

var
  TestLib: TTestLib;

implementation

uses
{$IFDEF MACOS}
  FMX.TestLib.Mac;
{$ENDIF}

{$IFDEF MSWINDOWS}
  FMX.TestLib.Win;
{$ENDIF}

initialization
  TestLib := GetTestLibClass.Create;

finalization
  if Assigned(TestLib) then
    TestLib.Free;

end.
