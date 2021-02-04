unit ThTypes;

interface

uses
  System.Classes,
  System.Generics.Collections,
  GR32;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;

type
  TThCanvasMode = (cmFreeDraw, cmShapeDraw);

  TThDrawMode       = (dmSelect, dmDraw, dmPen, dmEraser);

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThCanvas = interface
  end;

  IThDrawStyle = interface
  end;

  IThDrawItem = interface
  end;

  IThShapeItem = interface(IThDrawItem)
    function MakePolyPoly(ARect: TFloatRect): TThPolyPoly;
  end;

  IThItemSelection = interface
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure Realign;
  end;

  IThDrawObject = interface
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    function GetDrawItem: IThDrawItem;
    property DrawItem: IThDrawItem read GetDrawItem;
  end;

implementation

end.
