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

    function GetShapeRect: TRectF; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
  end;

  TThLineShape = class(TThShape)
  end;

  TThFillShape = class(TThShape)
  end;

  TThRectangle = class(TThFillShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawShape; override;
    procedure DrawHighlight; override;
  end;

  TThLine = class(TThLineShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawShape; override;
    procedure DrawHighlight; override;
  end;

implementation

uses
  ThItemFactory, System.UIConsts, CommonUtils;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FWidth := 1;
  FHeight := 1;

  FOpacity := 0.8;
  FBackgroundColor := claGreen;
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TThShape.Paint;
var
  S: string;
begin
  DrawShape;

{$IFDEF DEBUG}
  S := Format('Position(%f, %f)', [Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.taCenter);
{$ENDIF}
end;

procedure TThShape.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor = Value then
    Exit;

  FBackgroundColor := Value;
  Repaint;
end;

{ TThRectangle }

procedure TThRectangle.DrawHighlight;
var
  R: TRectF;
begin
  R := GetHighlightRect;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := HighlightColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

procedure TThRectangle.DrawShape;
var
  R: TRectF;
begin
  R := GetShapeRect;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := FBackgroundColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

function TThRectangle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, Pt);
end;

{ TThLine }

procedure TThLine.DrawHighlight;
begin
  inherited;

end;

procedure TThLine.DrawShape;
begin

end;

function TThLine.PtInItem(Pt: TPointF): Boolean;
begin

end;

initialization
  RegisterItem(1100, TThRectangle);

end.
