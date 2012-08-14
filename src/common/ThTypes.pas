unit ThTypes;

interface

uses
  System.UITypes, System.Classes, System.Types;

  function ScaleRect(const R: TRectF; dX, dY: Single): TRectF;

  procedure Debug(const Value: string; const Args: array of const); overload;
  procedure Debug(Value: string); overload;

implementation

uses
  WinAPI.Windows, System.SysUtils;

function ScaleRect(const R: TRectF; dX, dY: Single): TRectF;
begin
  Result.Left   := R.Left * dX;
  Result.Top    := R.Top * dY;
  Result.Right  := R.Right * dX;
  Result.Bottom := R.Bottom * dY;
end;

procedure Debug(Value: string);
begin
  OutputDebugString(PChar(Value));
end;

procedure Debug(const Value: string; const Args: array of const);
begin
  Debug(Format(Value, Args));
end;

end.
