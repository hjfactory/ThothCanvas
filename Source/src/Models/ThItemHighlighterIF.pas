unit ThItemHighlighterIF;

interface

uses
  System.Types, FMX.Types;

type
  IItemHighlitObject = interface;
  IItemHighlighter = interface
    function GetParent: IItemHighlitObject;
    procedure SetParent(Parent: IItemHighlitObject);
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;

    property Parent: IItemHighlitObject read GetParent write SetParent;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

  IItemHighlitObject = interface
    procedure DrawHighlight;
  end;

implementation

end.
