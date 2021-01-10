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
  TThPath = TArray<TFloatPoint>;
  TThPercent = 0..100;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;
//
//  TThPathHelper = class helper for TThPath
//    function Add(X, Y: Single): Integer; overload;
//  end;

implementation

//{ TThPathHelper }
//
//function TThPathHelper.Add(X, Y: Single): Integer;
//begin
//  Result := Add(FloatPoint(X, Y));
//end;

end.
