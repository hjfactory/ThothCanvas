unit ThTypes;

interface

uses
  FMX.Types, System.Types, System.UITypes;


type
  TSpotCorner = (
    scUnknown      = 0,
    scTop          = 1,
    scLeft         = 2,
    scRight        = 4,
    scBottom       = 8,
    scTopLeft      = 3    {rsdTop + rsdLeft},
    scTopRight     = 5    {rsdTop + rsdRight},
    scBottomLeft   = 10   {rsdBottom + rsdLeft},
    scBottomRight  = 12   {rsdBottom + rsdRight}{, spCustom});

  TTrackEvent = procedure(Sender: TObject; X, Y: Single) of object;
  TSelectedEvent = procedure(Sender: TObject; IsMultiple: Boolean) of object;

type
  IThItem = interface
    function GetItemRect: TRectF;
    function GetMinimumSize: TPointF;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

  IThCanvas = interface
    function IsDrawingItem: Boolean;
    function IsMultiSelected: Boolean;
  end;

  //////////////////////////////////////////////////////////////
  /// Item Highlight
  ///   IItemHighlitObject is IItemHighlighter's parent
  IItemHighlitObject = interface(IThItem)
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
  end;

  IItemHighlighter = interface
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

  //////////////////////////////////////////////////////////////
  /// Item Resizer and ResizeSpot
  ///   IItemResizerObject is IItemResizer's parent
  ///   IItemResizer is IItemResizeSpots parent
  IItemResizerObject = interface(IThItem)
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
