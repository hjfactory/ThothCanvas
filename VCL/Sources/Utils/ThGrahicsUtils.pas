unit ThGrahicsUtils;

interface

uses
  GR32, clipper;

type
  TFloatRectHelper = record helper for TFloatRect
    function Rect: TRect;
  end;

// Graphics32 <> clipper
function AAFloatPoint2AAPoint(const APolyPoly: TArrayOfArrayOfFloatPoint;
  Decimals: Integer = 0): TPaths; overload;
function AAFloatPoint2AAPoint(const APoly: TArrayOfFloatPoint;
  Decimals: Integer = 0): TPath; overload;
function AAPoint2AAFloatPoint(const APaths: TPaths;
  Decimals: Integer = 0): TArrayOfArrayOfFloatPoint;


implementation

uses
  System.Math;

{ TFloatRectHelper }

function TFloatRectHelper.Rect: TRect;
begin
  Result.Left   := Round(Left);
  Result.Top    := Round(Top);
  Result.Right  := Round(Right);
  Result.Bottom := Round(Bottom);
end;

function AAFloatPoint2AAPoint(const APolyPoly: TArrayOfArrayOfFloatPoint;
  Decimals: Integer = 0): TPaths; overload;
var
  I, J, DecScale: integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APolyPoly));
  for I := 0 to high(APolyPoly) do
  begin
    Setlength(Result[I], Length(APolyPoly[I]));
    for J := 0 to High(APolyPoly[I]) do
    begin
      Result[I][J].X := Round(APolyPoly[I][J].X * DecScale);
      Result[I][J].Y := Round(APolyPoly[I][J].Y * DecScale);
    end;
  end;
end;

function AAFloatPoint2AAPoint(const APoly: TArrayOfFloatPoint;
  Decimals: Integer = 0): TPath; overload;
var
  I, DecScale: Integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APoly));
  for I := 0 to High(APoly) do
  begin
    Result[I].X := Round(APoly[I].X * DecScale);
    Result[I].Y := Round(APoly[I].Y * DecScale);
  end;
end;

//------------------------------------------------------------------------------

function AAPoint2AAFloatPoint(const APaths: TPaths;
  Decimals: Integer = 0): TArrayOfArrayOfFloatPoint;
var
  I, J, DecScale: Integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APaths));
  for I := 0 to High(APaths) do
  begin
    Setlength(Result[i], Length(APaths[i]));
    for J := 0 to High(APaths[I]) do
    begin
      Result[I][J].X := APaths[I][J].X / DecScale;
      Result[I][J].Y := APaths[I][J].Y / DecScale;
    end;
  end;
end;

end.
