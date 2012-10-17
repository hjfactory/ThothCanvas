unit CommonUtils;

interface

uses
  System.Types;

procedure Debug(ALog: string); overload;
procedure Debug(ALog: string; args: array of const); overload;

function PtInTriangle(Pt, Pt1, Pt2, Pt3: TPointF): Boolean;

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

function Orientation(const Pt, Pt1, Pt2: TPointF): Integer;
var
  Orin: Double;
begin
  (* Linear determinant of the 3 points *)
  Orin := (Pt2.X - Pt1.X) * (Pt.Y - Pt1.Y) - (Pt.X - Pt1.X) * (Pt2.Y - Pt1.Y);

  if Orin > 0.0 then
    Result := +1             (* Orientaion is to the right-hand side *)
  else if Orin < 0.0 then
    Result := -1             (* Orientaion is to the left-hand side  *)
  else
    Result := 0;             (* Orientaion is neutral aka collinear  *)
end;

function PtInTriangle(Pt, Pt1, Pt2, Pt3: TPointF): Boolean;
var
  Or1: Integer;
  Or2: Integer;
  Or3: Integer;
begin
  Or1 := Orientation(Pt, Pt1, Pt2);
  Or2 := Orientation(Pt, Pt2, Pt3);
  Or3 := Orientation(Pt, Pt3, Pt1);

  if (Or1 = Or2) and (Or2 = Or3) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 = 0) or (Or3 = 0)
  else if Or2 = 0 then
    Result := (Or1 = 0) or (Or3 = 0)
  else if Or3 = 0 then
    Result := (Or2 = 0) or (Or1 = 0)
  else
    Result := False;end;

end.
