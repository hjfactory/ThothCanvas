unit ThItemResizablerIF;

interface

uses
  System.Types;

type
  TResizableSpotDirection = (rsdTopLeft, rsdTop, rsdTopRight, rsdLeft, rsdRight,
    rsdBottomLeft, rsdBottom, rsdBottomRight{, spCustom});

  IItemResizableSpot = interface
  end;

  IItemResizabler = interface
    function GetResizablerRect: TRectF;
    property ResizablerRect: TRectF read GetResizablerRect;
  end;

implementation

end.
