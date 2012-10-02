{

  Reference
    - mouse_event : http://msdn.microsoft.com/en-us/library/windows/desktop/ms646260(v=vs.85).aspx
    - keybd_event : http://msdn.microsoft.com/en-us/library/windows/desktop/ms646304(v=vs.85).aspx
}

unit FMX.TestLib.Win;

interface

uses
  System.Types, System.UITypes, System.Classes, FMX.TestLib;

function GetTestLibClass: TTestLibClass;

implementation

uses
  Winapi.Windows, Winapi.ShellAPI, FMX.Platform;

type
  TTestLibWin = class(TTestLib)
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(WheelDelta: Integer); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar); override;
  end;

function GetTestLibClass: TTestLibClass;
begin
  Result := TTestLibWin;
end;

{ TTestLibWin }

procedure TTestLibWin.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  mouse_event(MOUSEEVENTF_MOVE, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Flag: DWORD;
begin
  case Button of
    TMouseButton.mbLeft: Flag := MOUSEEVENTF_LEFTDOWN;
    TMouseButton.mbRight: Flag := MOUSEEVENTF_RIGHTDOWN;
    TMouseButton.mbMiddle: Flag := MOUSEEVENTF_MIDDLEDOWN;
  end;

  SetCursorPos(Round(X), Round(Y));
  mouse_event(Flag, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Flag: DWORD;
begin
  case Button of
    TMouseButton.mbLeft: Flag := MOUSEEVENTF_LEFTUP;
    TMouseButton.mbRight: Flag := MOUSEEVENTF_RIGHTUP;
    TMouseButton.mbMiddle: Flag := MOUSEEVENTF_MIDDLEUP;
  end;

  SetCursorPos(Round(X), Round(Y));
  mouse_event(Flag, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseWheel(WheelDelta: Integer);
begin
  inherited;

end;

procedure TTestLibWin.KeyDown(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

procedure TTestLibWin.KeyUp(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

end.
