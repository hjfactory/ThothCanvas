unit SpotCornerUtils;

interface

uses
  ThTypes;

const
  HORIZON_CORNERS   = TSpotCorner(Ord(scLeft) or Ord(scRight));
  VERTICAL_CORNERS  = TSpotCorner(Ord(scTop) or Ord(scBottom));

function AndSpotCorner(D1, D2: TSpotCorner): TSpotCorner;
function IfThenSpotCorner(AValue: Boolean; const ATrue: TSpotCorner; const AFalse: TSpotCorner): TSpotCorner;

function ContainSpotCorner(Source, SC: TSpotCorner): Boolean;
function SetHorizonSpotCorner(Source, SC: TSpotCorner): TSpotCorner;
function SetVerticalSpotCorner(Source, SC: TSpotCorner): TSpotCorner;

function ChangeHorizonSpotCorner(D1, D2: TSpotCorner): Boolean;
function ChangeVerticalSpotCorner(D1, D2: TSpotCorner): Boolean;
function SupportedHorizonSpotCorner(SC: TSpotCorner): Boolean;
function SupportedVerticalSpotCorner(SC: TSpotCorner): Boolean;

function HorizonSpotCornerExchange(D: TSpotCorner): TSpotCorner;
function VerticalSpotCornerExchange(D: TSpotCorner): TSpotCorner;
function SpotCornerExchange(D: TSpotCorner): TSpotCorner;

implementation

function AndSpotCorner(D1, D2: TSpotCorner): TSpotCorner;
begin
  Result := TSpotCorner(Ord(D1) and Ord(D2))
end;

function OrSpotCorner(D1, D2: TSpotCorner): TSpotCorner;
begin
  Result := TSpotCorner(Ord(D1) or Ord(D2))
end;

function IfThenSpotCorner(AValue: Boolean; const ATrue: TSpotCorner; const AFalse: TSpotCorner): TSpotCorner;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function ContainSpotCorner(Source, SC: TSpotCorner): Boolean;
begin
  Result := AndSpotCorner(Source, SC) = SC;
end;

function SetHorizonSpotCorner(Source, SC: TSpotCorner): TSpotCorner;
begin
  Result := AndSpotCorner(Source, VERTICAL_CORNERS);  // Horizon ¹ö¸²
  Result := OrSpotCorner(Result, SC);
end;

function SetVerticalSpotCorner(Source, SC: TSpotCorner): TSpotCorner;
begin
  Result := AndSpotCorner(Source, HORIZON_CORNERS);
  Result := OrSpotCorner(Result, SC);
end;

function ChangeHorizonSpotCorner(D1, D2: TSpotCorner): Boolean;
begin
  Result := AndSpotCorner(D1, HORIZON_CORNERS) <> AndSpotCorner(D2, HORIZON_CORNERS);
end;

function ChangeVerticalSpotCorner(D1, D2: TSpotCorner): Boolean;
begin
  Result := AndSpotCorner(D1, VERTICAL_CORNERS) <> AndSpotCorner(D2, VERTICAL_CORNERS);
end;

function SupportedHorizonSpotCorner(SC: TSpotCorner): Boolean;
begin
  Result := AndSpotCorner(SC, HORIZON_CORNERS) <> scUnknown;
end;

function SupportedVerticalSpotCorner(SC: TSpotCorner): Boolean;
begin
  Result := AndSpotCorner(SC, VERTICAL_CORNERS) <> scUnknown;
end;

function HorizonSpotCornerExchange(D: TSpotCorner): TSpotCorner;
begin
  Result := D;
  if AndSpotCorner(Result, HORIZON_CORNERS) <> scUnknown then
    Result := TSpotCorner(Ord(Result) xor Ord(HORIZON_CORNERS))
end;

function VerticalSpotCornerExchange(D: TSpotCorner): TSpotCorner;
begin
  Result := D;
  if AndSpotCorner(Result, VERTICAL_CORNERS) <> scUnknown then
  Result := TSpotCorner(Ord(D) xor Ord(VERTICAL_CORNERS))
end;

function SpotCornerExchange(D: TSpotCorner): TSpotCorner;
begin
  Result := D;
  Result := HorizonSpotCornerExchange(Result);
  Result := VerticalSpotCornerExchange(Result);
end;

end.
