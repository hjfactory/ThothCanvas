unit ThUtils;

interface

uses
  ThTypes,
  GR32, clipper;

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

function IntfEquals(AIntf1, AIntf2: IInterface): Boolean;

//function LocalToViewPort(
type
  TFloatPointHelper = record helper for TFloatPoint
  public
    function Scale(Value: TFloatPoint): TFloatPoint;
    function Offset(Value: TFloatPoint): TFloatPoint;
  end;

  TFloatRectHelper = record helper for TFloatRect
  private
    function GetBottomLeft: TFloatPoint;
    function GetTopRight: TFloatPoint;
    procedure SetBottomLeft(const Value: TFloatPoint);
    procedure SetTopRight(const Value: TFloatPoint);
    function GetWidth: TFloat;
    procedure SetWidth(const Value: TFloat);
    function GetHeight: TFloat;
    procedure SetHeight(const Value: TFloat);
  public
    procedure Realign;

    property TopRight: TFloatPoint read GetTopRight write SetTopRight;
    property BottomLeft: TFloatPoint read GetBottomLeft write SetBottomLeft;

    property Width: TFloat read GetWidth write SetWidth;
    property Height: TFloat read GetHeight write SetHeight;
  end;

implementation

uses
  System.Math, GR32_Geometry;

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

function IntfEquals(AIntf1, AIntf2: IInterface): Boolean;
begin
  Result := AIntf1 as IInterface = AIntf2 as IInterface;
end;

{ TFloatRectHelper }

function TFloatRectHelper.GetBottomLeft: TFloatPoint;
begin
  Result := FloatPoint(Self.Bottom, Self.Left);
end;

function TFloatRectHelper.GetHeight: TFloat;
begin
  Result := Abs(Self.Bottom - Self.Top);
end;

function TFloatRectHelper.GetTopRight: TFloatPoint;
begin
  Result := FloatPoint(Self.Top, Self.Right);
end;

function TFloatRectHelper.GetWidth: TFloat;
begin
  Result := Abs(Self.Right - Self.Left);
end;

procedure TFloatRectHelper.Realign;
var
  X, Y: TFloat;
begin
  if Self.Right < Self.Left then
  begin
    X := Self.Left;
    Self.Left := Self.Right;
    Self.Right := X;
  end;

  if Self.Bottom < Self.Top then
  begin
    Y := Self.Top;
    Self.Top := Self.Bottom;
    Self.Bottom := Y;
  end;
end;

procedure TFloatRectHelper.SetBottomLeft(const Value: TFloatPoint);
begin
  Self.Bottom := Value.Y;
  Self.Left := Value.X;
end;

procedure TFloatRectHelper.SetHeight(const Value: TFloat);
begin
  Self.Bottom := Self.Top + Value;
end;

procedure TFloatRectHelper.SetTopRight(const Value: TFloatPoint);
begin
  Self.Top := Value.Y;
  Self.Right := value.X;
end;

procedure TFloatRectHelper.SetWidth(const Value: TFloat);
begin
  Self.Right := Self.Left + Value;
end;

{ TFloatPointHelper }

function TFloatPointHelper.Offset(Value: TFloatPoint): TFloatPoint;
begin
  Result.X := Self.X + Value.X;
  Result.Y := Self.Y + Value.Y;
end;

function TFloatPointHelper.Scale(Value: TFloatPoint): TFloatPoint;
begin
  Result.X := Self.X * Value.X;
  Result.Y := Self.Y * Value.Y;
end;

end.
