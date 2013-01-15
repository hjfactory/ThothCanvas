unit ThTypes;

interface

uses
  FMX.Types, System.Types, System.UITypes, System.Generics.Collections;

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

  /////////////////////////////////////////////////////////
  ///  Commands
  IThCommand = interface
    procedure Execute;
    procedure Rollback;
  end;

//  IThItemCommand = interface(IThCommand)
//  end;
//
//  IThSystemCommand = interface(IThCommand)
//  end;

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

  // Basic item
  IThItem = interface
    function GetItemRect: TRectF;
    procedure ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);
  end;


  // Basig canvas
  IThCanvas = interface
    procedure DoGrouping(AIItem: IThItem);
    function IsDrawingItem: Boolean;
    function IsMultiSelected: Boolean;
  end;

  // Optional item data(e.g. Image item`s filepath)
  IThItemData = interface
  end;

  // Contain item control
  IThItems =  TList<IThItem>;
  IThItemContainer = interface
    function GetItem(Index: Integer): IThItem;
    function GetItemCount: Integer;
    property Items[Index: Integer]: IThItem read GetItem;
    property ItemCount: Integer read GetItemCount;
    function GetContainChildrenItems(AItem: IThItem; AChildren: IThItems): Boolean;
    function GetContainParentItem(AItem: IThItem): IThItem;
  end;

  IThCanvasController = interface
  end;

  //////////////////////////////////////////////////////////////
  /// Zoom object
  ///  Target is Canvas and Item
  IThZoomObject = interface
    function GetZoomScale: Single;
    property ZoomScale: Single read GetZoomScale;
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
  /// Item Selection and ResizeSpot
  ///   IItemSelectionObject is IItemSelection's parent
  ///   IItemSelection is IItemResizeSpots parent
  IItemSelectionObject = interface(IThItem)
    function GetMinimumSize: TPointF;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

  IItemResizeSpot = interface
  end;

  IItemSelection = interface
    function GetSelectionRect: TRectF;
    property SelectionRect: TRectF read GetSelectionRect;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetSpots(Index: Integer): IItemResizeSpot;
    property Spots[Index: Integer] : IItemResizeSpot read GetSpots;
    function GetIsMouseOver: Boolean;
    property IsMouseOver: Boolean read GetIsMouseOver;

    procedure DrawSelection;

    procedure ShowSpots;
    procedure ShowDisableSpots;
    procedure HideSpots;
    procedure RealignSpot;
  end;

  TPointFHelper = record helper for TPointF
  public
    function Scale(const AFactor: Single): TPointF;
  end;


implementation

{ TPointFHelper }

function TPointFHelper.Scale(const AFactor: Single): TPointF;
begin
  Result.X := X * AFactor;
  Result.Y := Y * AFactor;
end;

end.
