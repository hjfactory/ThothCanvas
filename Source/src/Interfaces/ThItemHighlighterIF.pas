unit ThItemHighlighterIF;

interface

uses
  System.Types, FMX.Types, System.UITypes;

type
  IItemHighlitObject = interface;
  IItemHighlighter = interface
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;

    function GetParent: IItemHighlitObject;
    procedure SetParent(Parent: IItemHighlitObject);

    property Parent: IItemHighlitObject read GetParent write SetParent;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

  IItemHighlitObject = interface
    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor);
  end;

implementation

end.
