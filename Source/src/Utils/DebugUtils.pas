unit DebugUtils;

interface

uses
  System.Types, System.Classes;

//function PtInCircle(const Point, Center: TPointF; Radius: Single): Boolean; overload;
//function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean; overload;


procedure Debug(ALog: string); overload;
procedure Debug(ALog: string; args: array of const); overload;
procedure AddDebug(ALog: string); overload;
procedure AddDebug(ALog: string; args: array of const); overload;
procedure PrintLog;
procedure ConditionalDebug(ALog: string; args: array of const); overload;

var
  DebugStart: Boolean = False;
  List: TStringList;

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

procedure AddDebug(ALog: string);
begin
  if not Assigned(List) then
    List := TStringList.Create;

  List.Add(ALog);
end;

procedure AddDebug(ALog: string; args: array of const);
begin
  AddDebug(Format(ALog, args));
end;

procedure PrintLog;
var
  Log: string;
begin
  if not Assigned(List) then
    Exit;
  for Log in List do
    Debug(Log);
end;

procedure ConditionalDebug(ALog: string; args: array of const);
begin
  if DebugStart then
    Debug(Format(ALog, args));
end;

//function PtInCircle(const Point, Center: TPointF; Radius: Single): Boolean;
//begin
//  if Radius > 0 then
//  begin
//    Result := Sqr(Point.X - Center.X) + Sqr(Point.Y - Center.Y) <= Sqr(Radius);
////    Result := Sqr((Point.X - Center.X) / Radius) +
////      Sqr((Point.Y - Center.Y) / Radius) <= 1;
//  end
//  else
//  begin
//    Result := False;
//  end;
//end;

initialization

finalization
  if Assigned(List) then
    List.Free;

end.
