unit Debug;

interface

uses
  FMX.Types, System.Types, FMX.Forms;

procedure DebugMsg(Value: string);

function ScreenToLocal(AControl: TControl; APt: TPointF): TPointF;

implementation

uses
  WinAPI.Windows;

procedure DebugMsg(Value: string);
begin
  OutputDebugString(PChar(Value));
end;

function ScreenToLocal(AControl: TControl; APt: TPointF): TPointF;
begin
  if Assigned(AControl.Parent) and (AControl.Parent is TControl) then
    Result := ScreenToLocal(TControl(AControl.Parent), APt)
  else if Assigned(AControl.Parent) and (AControl.Parent is TForm) then
    Result := TForm(AControl.Parent).ScreenToClient(APt)
  ;
end;

end.
