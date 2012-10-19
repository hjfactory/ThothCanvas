unit ThItemIF;

interface

uses
  System.Types;

type
  IThItem = interface
    function GetItemRect: TRectF;
    function GetMinimumSize: TPointF;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

implementation

end.
