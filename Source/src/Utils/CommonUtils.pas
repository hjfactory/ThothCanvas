unit CommonUtils;

interface

procedure Debug(ALog: string);

implementation
{$IFDEF MACOS}

{$ENDIF}

{$IFDEF MSWINDOWS}
  uses
    WinAPI.Windows;
{$ENDIF}



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

end.
