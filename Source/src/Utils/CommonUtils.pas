unit CommonUtils;

interface

procedure Debug(ALog: string); overload;
procedure Debug(ALog: string; args: array of const); overload;

implementation
  uses
    System.SysUtils
{$IFDEF MACOS}

{$ENDIF}

{$IFDEF MSWINDOWS}
    , WinAPI.Windows
{$ENDIF}
  ;


procedure Debug(ALog: string);
begin
{$IFNDEF DEBUG}
  Exit;
{$ENDIF}

{$IFDEF MACOS}

{$ENDIF}

{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(ALog));
{$ENDIF}
end;

procedure Debug(ALog: string; args: array of const);
begin
  Debug(Format(ALog, args));
end;

end.
