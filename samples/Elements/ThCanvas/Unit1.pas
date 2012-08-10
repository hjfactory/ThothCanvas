unit Unit1;

interface

procedure Debug(Value: string);

implementation

uses
  WinAPI.Windows;

procedure Debug(Value: string);
begin
  OutputDebugString(PChar(Value));
end;

end.
