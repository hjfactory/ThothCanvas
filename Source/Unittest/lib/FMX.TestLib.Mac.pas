unit FMX.TestLib.Mac;

interface
uses
  System.Types, System.UITypes, System.Classes, FMX.TestLib;

function GetTestLibClass: TTestLibClass;

implementation

uses
  Macapi.Foundation,
  Macapi.AppKit, FMX.Platform.Mac;

type
  TTestLibMac = class(TTestLib)
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
  Result := TTestLibMac;
end;

{ TTestLibMac }

procedure TTestLibMac.KeyDown(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

procedure TTestLibMac.KeyUp(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

procedure TTestLibMac.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
end;

procedure TTestLibMac.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
//  NSEvent(
end;

procedure TTestLibMac.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TTestLibMac.MouseWheel(WheelDelta: Integer);
begin
  inherited;

end;

end.
