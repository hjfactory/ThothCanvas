unit ThConsts;

interface

uses
  System.UIConsts;

const
  ItemDefaultOpacity  = 0.8;

  ItemHighlightSize = 5;
  ItemHighlightColor = $FFAAAAAA;

  ItemResizableSpotOverColor = claRed;
  ItemResizableSpotOutColor = claWhite;
  ItemResizableSpotRadius = 8;

type
  TTrackEvent = procedure(Sender: TObject; X, Y: Single) of object;

implementation

end.
