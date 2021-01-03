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
  ItemLineSelectionThickness = 15;    // �� ���� ����
  ItemFocusMinimumSize = ItemMinimumSize * 0.5;

//{$DEFINE ON_HIGHLIGHT}

  ItemHighlightSize = 3;
//  ItemHighlightColor = $FFAAAAAA;
  ItemHighlightColor = claGray;

  ItemSelectionSize = 3;
  ItemSelectionColor = claSkyblue;

  ItemResizeSpotOverColor = claRed;
  ItemResizeSpotOutColor = claWhite;
  ItemResizeSpotDisableColor = claDarkgray;
  ItemResizeSpotRadius = 6;

  // Item factorys Unique id
  ItemFactoryIDRectangle  = 1100;
  ItemFactoryIDLine       = 1200;
  ItemFactoryIDCircle     = 1300;
  ItemFactoryIDText       = 2000;
  ItemFactoryIDImageFile  = 3000;

  CanvasTrackAniCount: Single   = 5;
  CanvasTrackDuration: Single   = 0.5;

  CanvasZoomOutRate: Single      = 0.9;
  CanvasZoomInRate: Single       = 1.1;
  CanvasZoomScaleDefault: Single = 1;
  CanvasZoomScaleMax: Single     = 20;
  CanvasZoomScaleMin: Single     = 0.05;
//
//  CanvasZoomOutRate: Single      = 0.9;
//  CanvasZoomInRate: Single       = 1.1;
//  CanvasZoomScaleDefault: Single = 0.1;
//  CanvasZoomScaleMax: Single     = 15;
//  CanvasZoomScaleMin: Single     = 0.001;

implementation

end.
