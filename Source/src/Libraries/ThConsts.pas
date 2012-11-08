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

implementation

end.
