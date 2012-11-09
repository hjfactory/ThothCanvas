unit Unit2;

interface

procedure Debug(str: string);

implementation

uses
  WinAPI.Windows;

procedure Debug(str: string);
begin
  OutputDebugString(PChar(str));
end;

end.
