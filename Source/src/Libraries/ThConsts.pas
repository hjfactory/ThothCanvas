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

  ItemShapeDefaultColor = claGreen;

  ItemHighlightSize = 5;
  ItemHighlightColor = $FFAAAAAA;

  ItemResizeSpotOverColor = claRed;
  ItemResizeSpotOutColor = claWhite;
  ItemResizeSpotRadius = 8;

type
  TTrackEvent = procedure(Sender: TObject; X, Y: Single) of object;

implementation

end.
