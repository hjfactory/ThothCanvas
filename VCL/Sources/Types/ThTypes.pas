unit ThTypes;

interface

uses
  GR32, System.Generics.Collections;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;
type
  IThCanvas = interface
  end;

  TThCanvasMode = (cmSelection, cmFreeDraw);
  TThFreeDrawMode = (fdmPen, fdmEraser);
  TThShapeMode = (smNone, smRectangle);
  TThPath = TArray<TFloatPoint>;
  TThPercent = 0..100;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

  IThFreeDrawLayer = interface
  ['{2CBF2F86-BE55-41A0-AD08-EA2BB9B2801E}']
    procedure SetFreeDrawMode(const AMode: TThFreeDrawMode);

  end;

implementation

end.
