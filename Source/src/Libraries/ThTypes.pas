unit ThTypes;

interface

uses
  FMX.Types, System.Types;


type
  TSpotCorner = (
                              scUnknown      = 0,
                              scTop          = 1,
                              scLeft         = 2,
                              scRight        = 4,
                              scBottom       = 8,
                              scTopLeft      = 3{rsdTop or rsdLeft},
                              scTopRight     = 5{rsdTop or rsdRight},
                              scBottomLeft   = 10{rsdBottom or rsdLeft},
                              scBottomRight  = 12{rsdBottom or rsdRight}{, spCustom});

type
  TControlHelper = class helper for TControl
    function LocalToAbsoluteRect(ARect: TRectF): TRectF;
  end;

implementation

{ TControlHelper }

function TControlHelper.LocalToAbsoluteRect(ARect: TRectF): TRectF;
begin
  Result.TopLeft := LocalToAbsolute(ARect.TopLeft);
  Result.BottomRight := LocalToAbsolute(ARect.BottomRight);
end;

end.
