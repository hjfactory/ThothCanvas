unit ThUtils;

interface

uses
  ThTypes,
  GR32, clipper;

type
  TFloatRectHelper = record helper for TFloatRect
    function Rect: TRect;
  end;

function PtInPolyPolygon(const APoint: TFloatPoint; const APolyPoly: TThPolyPoly): Boolean;
function EmptyRect: TFloatRect;
function EmptyPoint: TFloatPoint;

// Graphics32 <> clipper
function AAFloatPoint2AAPoint(const APolyPoly: TArrayOfArrayOfFloatPoint;
  Decimals: Integer = 0): TPaths; overload;
function AAFloatPoint2AAPoint(const APoly: TArrayOfFloatPoint;
  Decimals: Integer = 0): TPath; overload;
function AAPoint2AAFloatPoint(const APaths: TPaths;
  Decimals: Integer = 0): TArrayOfArrayOfFloatPoint;

function ScaleRect(AFR: TFloatRect; AScale: TFloatPoint): TFloatRect;
function OffsetRect(AFR: TFloatRect; AOffset: TFloatPoint): TFloatRect;

implementation

uses
  System.Math, GR32_Geometry;

{ TFloatRectHelper }

function TFloatRectHelper.Rect: TRect;
begin
  Result.Left   := Round(Left);
  Result.Top    := Round(Top);
  Result.Right  := Round(Right);
  Result.Bottom := Round(Bottom);
end;

function PtInPolyPolygon(const APoint: TFloatPoint; const APolyPoly: TThPolyPoly): Boolean;
var
  Poly: TArrayOfFloatPoint;
begin
  Result := False;
  for Poly in APolyPoly do
  begin
    if PointInPolygon(APoint, Poly) then
      Exit(True);
  end;
end;

function EmptyRect: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function EmptyPoint: TFloatPoint;
begin
  Result := TFloatPoint.Zero;
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

function ScaleRect(AFR: TFloatRect; AScale: TFloatPoint): TFloatRect;
begin
  Result.Left   := AFR.Left * AScale.X;
  Result.Top    := AFR.Top * AScale.Y;
  Result.Right  := AFR.Right * AScale.X;
  Result.Bottom := AFR.Bottom * AScale.Y;
end;

function OffsetRect(AFR: TFloatRect; AOffset: TFloatPoint): TFloatRect;
begin
  Result.Left   := AFR.Left + AOffset.X;
  Result.Top    := AFR.Top + AOffset.Y;
  Result.Right  := AFR.Right + AOffset.X;
  Result.Bottom := AFR.Bottom + AOffset.Y;
end;

end.
