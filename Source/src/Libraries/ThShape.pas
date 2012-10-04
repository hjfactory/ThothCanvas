unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils,
  FMX.Types, ThItem;

type
  TThShape = class(TThItem)
  strict private
    procedure Paint; override;
  protected
    procedure PaintShape; virtual; abstract;
    procedure PaintHighlight; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    function GetShapeRect: TRectF; virtual;
    function PtInShape(APt: TPointF): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThLineShape = class(TThShape)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThFillShape = class(TThShape)
  end;

  TThRectangle = class(TThFillShape)
  protected
    procedure PaintShape; override;
  end;

  TThLine = class(TThLineShape)
  protected
    procedure PaintShape; override;
  end;

implementation

uses
  ThItemFactory, System.UIConsts, CommonUtils;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;
//  FOpacity := 0.8;

  FWidth := 1;
  FHeight := 1;
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  SetSelected((Button = TMouseButton.mbLeft) and (PtInShape(PointF(X, Y))));
end;

procedure TThShape.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThShape.Paint;
var
  S: string;
begin
  PaintShape;

{$IFDEF DEBUG}
  S := Format('Position(%f, %f)', [Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.taCenter);
{$ENDIF}
end;

procedure TThShape.PaintHighlight;
begin

end;

function TThShape.PtInShape(APt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, APt);
end;

{ TThLineShape }

constructor TThLineShape.Create(AOwner: TComponent);
begin
  inherited;

end;

{ TThRectangle }

procedure TThRectangle.PaintShape;
var
  R: TRectF;
begin
  R := GetShapeRect;

  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := claGreen;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
//  Debug(Format('Rectangle - R : %f %f %f %f', [R.Left, R.Top, R.Right, R.Bottom]));
end;

{ TThLine }

procedure TThLine.PaintShape;
begin

end;

initialization
  RegisterItem(1100, TThRectangle);

end.
