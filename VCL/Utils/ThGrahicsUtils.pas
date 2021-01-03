unit ThGrahicsUtils;

interface

uses
  GR32;

type
  TFloatRectHelper = record helper for TFloatRect
    function Rect: TRect;
  end;


implementation

{ TFloatRectHelper }

function TFloatRectHelper.Rect: TRect;
begin
  Result.Left   := Round(Left);
  Result.Top    := Round(Top);
  Result.Right  := Round(Right);
  Result.Bottom := Round(Bottom);
end;

end.
