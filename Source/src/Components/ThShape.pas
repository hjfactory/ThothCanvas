unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils,
  FMX.Types, ThItem, ThItemHighlighterIF, ThItemHighlighter;

type
{
  ShapeRect : LocalRect
  ClipRect  : ShapeRect + ShadowRect
}

  TThShape = class(TThItem, IItemHighlitObject)
  private
    FBackgroundColor: TAlphaColor;
    procedure SetBackgroundColor(const Value: TAlphaColor);
  strict protected
    procedure Paint; override;
  protected
    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;

    function GetShapeRect: TRectF; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
//    property MinSize: TPointF read FMinSize write FMinSize;
  end;
{
  TThLineShape = class(TThShape)
  end;

  TThFillShape = class(TThShape)
  end;
}
  TThRectangle = class(TThShape)
  private
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); override;
//    procedure DrawHighlight; override;
  end;

  TThLine = class(TThShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  end;

implementation

uses
  ThItemFactory, System.UIConsts, CommonUtils;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FHighlighter := TThItemShadowHighlighter.Create(Self);

  FWidth := MinimumSize.X;
  FHeight := MinimumSize.Y;

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
  DrawItem(GetShapeRect, FBackgroundColor);

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

procedure TThRectangle.DrawItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  R: TRectF;
begin
  R := ARect;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := AFillColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

function TThRectangle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, Pt);
end;

{ TThLine }

procedure TThLine.DrawItem(ARect: TRectF; AFillColor: TAlphaColor);
begin

end;

function TThLine.PtInItem(Pt: TPointF): Boolean;
begin
  Result := False;
end;

initialization
  RegisterItem(1100, TThRectangle);

end.
