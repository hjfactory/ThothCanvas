unit ThItemResizerIF;

interface

uses
  System.Types;

type
  IItemResizerObject = interface
    procedure RealignSpot;
  end;

  IItemResizeSpot = interface
  end;

  IItemResizer = interface
    function GetResizerRect: TRectF;
    property ResizerRect: TRectF read GetResizerRect;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetSpots(Index: Integer): IItemResizeSpot;
    property Spots[Index: Integer] : IItemResizeSpot read GetSpots;

    procedure ShowSpots;
    procedure HideSpots;
  end;

implementation

end.
