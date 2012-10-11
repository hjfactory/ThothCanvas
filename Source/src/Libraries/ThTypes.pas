unit ThTypes;

interface

uses
  FMX.Types, System.Types;

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
