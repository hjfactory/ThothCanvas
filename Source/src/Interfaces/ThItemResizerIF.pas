unit ThItemResizerIF;

interface

uses
  System.Types;

type
  IItemResizeSpot = interface
  end;

  IItemResizer = interface
    function GetResizerRect: TRectF;
    property ResizerRect: TRectF read GetResizerRect;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetSpots(Index: Integer): IItemResizeSpot;
    property Spots[Index: Integer] : IItemResizeSpot read GetSpots;
  end;

implementation

end.
