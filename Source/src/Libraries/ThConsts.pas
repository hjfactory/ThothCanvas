unit ThConsts;

interface

uses
  System.UIConsts;

const
{$IFDEF RELEASE}
  ItemDefaultOpacity  = 0.8;
{$ELSE}
  ItemDefaultOpacity  = 1;
{$ENDIF}

  ItemMinimumSize = 30;
  ItemShapeDefaultColor = claGreen;
  ItemLineThickness = 7;

  ItemHighlightSize = 3;
  ItemHighlightColor = $FFAAAAAA;

  ItemResizeSpotOverColor = claRed;
  ItemResizeSpotOutColor = claWhite;
  ItemResizeSpotRadius = 8;

type
  TTrackEvent = procedure(Sender: TObject; X, Y: Single) of object;

implementation

end.
