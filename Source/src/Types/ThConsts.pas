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
  ItemLineSelectionThickness = 15;    // 선 선택 범위
  ItemFocusMinimumSize = ItemMinimumSize * 0.5;

  ItemHighlightSize = 3;
//  ItemHighlightColor = $FFAAAAAA;
  ItemHighlightColor = claGray;

  ItemResizeSpotOverColor = claRed;
  ItemResizeSpotOutColor = claWhite;
  ItemResizeSpotDisableColor = claDarkgray;
  ItemResizeSpotRadius = 6;

  // Item factorys Unique id
  ItemFactoryIDRectangle  = 1100;
  ItemFactoryIDLine       = 1200;
  ItemFactoryIDCircle     = 1300;

  CanvasTrackAniCount   = 5;
  CanvasTrackDuration   = 0.5;

  CanvasZoomOutRate      = 0.9;
  CanvasZoomInRate       = 1.1;
  CanvasZoomScaleDefault = 0.1;
  CanvasZoomScaleMax     = 15;
  CanvasZoomScaleMin     = 0.001;

implementation

end.
