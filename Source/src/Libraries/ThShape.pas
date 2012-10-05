unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils,
  FMX.Types, ThItem;

type
  TThShape = class(TThHighlightItem)
  strict private
    procedure Paint; override;
  private
    FBackgroundColor: TAlphaColor;
    procedure SetBackgroundColor(const Value: TAlphaColor);
  protected
    procedure DrawShape; virtual; abstract;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    function GetShapeRect: TRectF; virtual; abstract;
    function PtInShape(APt: TPointF): Boolean; virtual;

    procedure DoRealign; override; //hjf
  public
    constructor Create(AOwner: TComponent); override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
  end;

  TThLineShape = class(TThShape)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThFillShape = class(TThShape)
  end;

  TThRectangle = class(TThFillShape)
  protected
    function GetClipRect: TRectF; override;

    function GetShapeRect: TRectF; override;

    procedure DrawShape; override;
    procedure DrawHighlight; override;
  end;

  TThLine = class(TThLineShape)
  protected
    procedure DrawShape; override;
  end;

implementation

uses
  ThItemFactory, System.UIConsts, CommonUtils;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  ClipChildren := False;
  AutoCapture := True;

  FWidth := 1;
  FHeight := 1;

  FOpacity := 0.8;
  FBackgroundColor := claGreen;
end;

procedure TThShape.DoRealign;
begin
  SetBounds(0, 0, Width + 10, Height + 10);
  FRecalcUpdateRect := True; // need to recalc
  Realign;
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
  if FSelected then
    DrawHighlight;

  DrawShape;

{$IFDEF DEBUG}
  S := Format('Position(%f, %f)', [Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.taCenter);
{$ENDIF}
end;

function TThShape.PtInShape(APt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, APt);
end;

procedure TThShape.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor = Value then
    Exit;

  FBackgroundColor := Value;
  Repaint;
end;

{ TThLineShape }

constructor TThLineShape.Create(AOwner: TComponent);
begin
  inherited;
end;

{ TThRectangle }

function TThRectangle.GetClipRect: TRectF;
begin
  Result := inherited GetClipRect;
  Result.Bottom := Result.Bottom + 10;
  Result.Right := Result.Right + 10;
end;

function TThRectangle.GetShapeRect: TRectF;
begin
  Result := LocalRect;
//  Result.Right := Result.Right - FHighlightSize;
//  Result.Bottom := Result.Bottom - FHighlightSize;
end;

procedure TThRectangle.DrawShape;
var
  R: TRectF;
begin
//Exit;
  R := GetShapeRect;

  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := FBackgroundColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
//  Debug(Format('Rectangle - R : %f %f %f %f', [R.Left, R.Top, R.Right, R.Bottom]));
end;

procedure TThRectangle.DrawHighlight;
var
  R: TRectF;
begin
  R := GetShapeRect;
  R.Offset(HighlightSize, HighlightSize);

  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := HighlightColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

{ TThLine }

procedure TThLine.DrawShape;
begin

end;

initialization
  RegisterItem(1100, TThRectangle);

end.
