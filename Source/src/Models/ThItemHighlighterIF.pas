unit ThItemHighlighterIF;

interface

uses
  System.Types, FMX.Types;

type
  IThItemHighlitObject = interface;
  IThItemHighlighter = interface
    function GetParent: IThItemHighlitObject;
    procedure SetParent(Parent: IThItemHighlitObject);
    function GetHighlightRect: TRectF;
    procedure DrawHighlight;

    property Parent: IThItemHighlitObject read GetParent write SetParent;
    property HighlightRect: TRectF read GetHighlightRect;
  end;

  IThItemHighlitObject = interface
    procedure DrawHighlight;
  end;

implementation

end.
