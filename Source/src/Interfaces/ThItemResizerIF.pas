unit ThItemResizerIF;

interface

uses
  System.Types;

type
  IItemResizeSpot = interface
  end;

  IItemResizerObject = interface
    procedure RealignSpot;
  end;

  IItemResizer = interface
    function GetResizerRect: TRectF;
    property ResizerRect: TRectF read GetResizerRect;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetSpots(Index: Integer): IItemResizeSpot;
    property Spots[Index: Integer] : IItemResizeSpot read GetSpots;

    procedure RealignSpot;

//    function GetParent: IItemResizerObject;
//    procedure SetParent(Parent: IItemResizerObject);
//
//    property Parent: IItemResizerObject read GetParent write SetParent;
  end;

implementation

end.
