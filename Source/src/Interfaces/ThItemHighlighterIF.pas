unit ThItemHighlighterIF;

interface

uses
  System.Types, FMX.Types, System.UITypes;

type
  IItemHighlitObject = interface
    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor);
  end;

  IItemHighlighter = interface
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

implementation

end.
