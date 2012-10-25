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

  TTrackingEvent = procedure(Sender: TObject; X, Y: Single) of object;

type
  IThObserver = interface;

  IThCommand = interface
    procedure Execute;
    procedure Rollback;
  end;

  /////////////////////////////////////////////////////////
  ///  Observer Pattern
  IThSubject = interface
    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;


  IThItem = interface
    function GetItemRect: TRectF;
  end;

  IThCanvas = interface
    function IsDrawingItem: Boolean;
    function IsMultiSelected: Boolean;
  end;

  IThCanvasController = interface
    procedure SetThCanvas(ThCanvas: IThCanvas);
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
  function GetMinimumSize: TPointF;
    property MinimumSize: TPointF read GetMinimumSize;
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
