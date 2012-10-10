unit ThConsts;

interface

uses
  System.UIConsts;

const
  ItemDefaultOpacity  = 0.8;

  ItemHighlightSize = 5;
  ItemHighlightColor = $FFAAAAAA;

  ItemResizeSpotOverColor = claRed;
  ItemResizeSpotOutColor = claWhite;
  ItemResizeSpotRadius = 8;

type
  TTrackEvent = procedure(Sender: TObject; X, Y: Single) of object;

implementation

end.
