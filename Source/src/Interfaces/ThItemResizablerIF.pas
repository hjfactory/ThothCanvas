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
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetSpots(Index: Integer): IItemResizableSpot;
    property Spots[Index: Integer] : IItemResizableSpot read GetSpots;
  end;

implementation

end.
