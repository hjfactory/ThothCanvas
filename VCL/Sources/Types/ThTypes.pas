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
  TThCanvasMode = (cmSelection, cmFreeDraw, cmShapeDraw);

  TThFreeDrawMode = (fdmPen, fdmEraser);
  TThShapeMode = (smNone, smRectangle);

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThCanvas = interface
  end;

  IThDrawObject = interface
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    procedure StartMove(const APoint: TFloatPoint);
    procedure Move(const APoint: TFloatPoint);
    procedure DoneMove;

    function CreateItem: TObject;
  end;

  IThDrawStyle = interface
  end;


implementation

end.
