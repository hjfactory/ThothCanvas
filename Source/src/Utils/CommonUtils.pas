unit CommonUtils;

interface

uses
  System.Types;

//function PtInCircle(const Point, Center: TPointF; Radius: Single): Boolean; overload;
//function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean; overload;


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

end.
