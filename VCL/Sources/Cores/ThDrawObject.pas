unit ThDrawObject;

interface

uses
  GR32;

type
  TThDrawObject = class
  public
    procedure Move(const X, Y: TFloat); overload; virtual;
    procedure Move(const APoint: TFloatPoint); overload; virtual;
  end;

  TThBrushObject = class(TThDrawObject)
  end;

  TThPenObject = class(TThBrushObject)
  end;

  TThEraserObject = class(TThBrushObject)
  end;

  TThShapeObject = class(TThDrawObject)
  end;

//  TTHDraw

implementation

{ TThDrawObject }

{ TThDrawObject }

procedure TThDrawObject.Move(const APoint: TFloatPoint);
begin

end;

procedure TThDrawObject.Move(const X, Y: TFloat);
begin
  Move(FloatPoint(X, Y));
end;

end.
