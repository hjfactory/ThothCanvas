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

  TThFreeDrawMode   = (fdmPen, fdmEraser);
  TThShapeDrawMode  = (sdmSelect, sdmDraw);

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThCanvas = interface
  end;

  IThDrawItem = interface
  end;

  IThDrawObject = interface
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure DrawItem(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint; AItem: IThDrawItem);

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState);
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState);
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState);

    function CreateItem: TObject;
  end;

  IThDrawStyle = interface
  end;


implementation

end.
