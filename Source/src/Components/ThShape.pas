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
//    FMinSize: TPointF;
    procedure SetBackgroundColor(const Value: TAlphaColor);
    function GetHighlighter: TThItemShadowHighlighter;
  strict protected
    procedure Paint; override;
  protected
    procedure DrawShape; virtual; abstract;
    procedure DrawHighlight; virtual; abstract;

    function GetShapeRect: TRectF; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property Highlighter: TThItemShadowHighlighter read GetHighlighter;
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
    class function GetMinSize: TPointF; static;
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawShape; override;
    procedure DrawHighlight; override;

    class property MinSize: TPointF read GetMinSize;
  end;

  TThLine = class(TThShape)
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

  FHighlighter := TThItemShadowHighlighter.Create(Self);

  FWidth := 1;
  FHeight := 1;

  FOpacity := 0.8;
  FBackgroundColor := claGreen;
end;

function TThShape.GetHighlighter: TThItemShadowHighlighter;
begin
  Result := TThItemShadowHighlighter(FHighlighter);
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
  if not Assigned(FHighlighter) then
    Exit;

  R := FHighlighter.HighlightRect;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := Highlighter.HighlightColor;
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

class function TThRectangle.GetMinSize: TPointF;
begin
  Result := PointF(50, 50);
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
  Result := False;
end;

initialization
  RegisterItem(1100, TThRectangle);

end.
