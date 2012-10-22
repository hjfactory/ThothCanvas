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
  ItemResizeSpotRadius = 6;

  ItemFactoryIDRectangle  = 1100;
  ItemFactoryIDLine       = 1200;
  ItemFactoryIDCircle     = 1300;

implementation

end.
