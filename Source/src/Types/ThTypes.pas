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
  ['{DC09EDE9-D127-4EF5-B463-D34918B93F34}']
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
  ['{6E501865-1F0D-4804-B6F0-E9C24A883555}']
    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
  ['{26D4EC95-764F-467B-9AA1-6E52B8AD3F5E}']
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

  // Basic item
  IThItem = interface
  ['{E0963BA6-CA3E-47C6-8BA0-F44A6E7FB85F}']
    function GetItemRect: TRectF;
    procedure ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);
  end;

  // Basic canvas
  IThCanvas = interface
  ['{41BC19D7-6DFC-4507-8B60-F3FD2AB57086}']
//    procedure DoGrouping(AIItem: IThItem);
    function IsDrawingItem: Boolean;
    function IsMultiSelected: Boolean;
  end;

  // Optional item data(e.g. Image item`s filepath)
  IThItemData = interface
  ['{F1D5D66C-7534-4390-8D7C-BB4D24183014}']
  end;

  // Contain item control
  IThItems =  TList<IThItem>;

  IThCanvasController = interface
  ['{DAA1217E-23A2-4622-9704-2026F5D5D678}']
  end;

  //////////////////////////////////////////////////////////////
  /// Zoom object
  ///  Target is Canvas and Item
  IThZoomObject = interface
  ['{A383554D-8195-4BA8-9098-2C4342FC4A26}']
    function GetZoomScale: Single;
    property ZoomScale: Single read GetZoomScale;
  end;

  //////////////////////////////////////////////////////////////
  /// Item Highlight
  ///   IItemHighlitObject is IItemHighlighter's parent
  IItemHighlitObject = interface(IThItem)
  ['{A62D8499-2F18-47B2-8363-BE8B74CA51BB}']
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
  end;

  IItemHighlighter = interface
  ['{1187AA98-5C17-441E-90F7-521735E07002}']
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

  //////////////////////////////////////////////////////////////
  /// Item Selection and ResizeSpot
  ///   IItemSelectionObject is IItemSelection's parent
  ///   IItemSelection is IItemResizeSpots parent
  IItemSelectionObject = interface(IThItem)
  ['{4887F0E3-ECC3-4B67-B45E-7E79ECBBC3F8}']
    function GetMinimumSize: TSizeF;
    property MinimumSize: TSizeF read GetMinimumSize;
  end;

  IItemResizeSpot = interface
  ['{9B408774-991E-438D-95B2-1D0BB1A6EBD3}']
  end;

  IItemSelection = interface
  ['{871374CC-B174-4ED0-A0AD-BAFFA97E21D5}']
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
