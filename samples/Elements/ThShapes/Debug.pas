unit Debug;

interface

procedure DebugMsg(Value: string);

implementation

uses
  WinAPI.Windows;

procedure DebugMsg(Value: string);
begin
  OutputDebugString(PChar(Value));
end;

end.
