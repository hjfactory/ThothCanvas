unit ThTypes;

interface

uses
  GR32, System.Generics.Collections;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;
type
  TThDrawMode = (dmFree, dmShape);
  TThPath = TList<TFloatPoint>;

  TThPathHelper = class helper for TThPath
    function Add(X, Y: Single): Integer; overload;
  end;

implementation

{ TThPathHelper }

function TThPathHelper.Add(X, Y: Single): Integer;
begin
  Result := Add(FloatPoint(X, Y));
end;

end.
