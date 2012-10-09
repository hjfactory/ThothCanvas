unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils,
  FMX.Types, ThItem, ThItemHighlighterIF, ThItemResizablerIF, System.UIConsts;

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
    function CreateHighlighter: IItemHighlighter; override;
    function CreateResizabler: IItemResizabler; override;

    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    procedure ShowResizableSpots; virtual;
    procedure HideResizableSpots;

    procedure DoSelected(Selected: Boolean); override;

    function GetShapeRect: TRectF; virtual;
    procedure ResizableSpotTrack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
  CommonUtils, ThConsts, ThItemFactory, ThItemHighlighter, ThItemResizabler;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FWidth := MinimumSize.X;
  FHeight := MinimumSize.Y;

  FOpacity := 0.8;
  FBackgroundColor := claGreen;
end;

destructor TThShape.Destroy;
begin

  inherited;
end;

function TThShape.CreateHighlighter: IItemHighlighter;
var
  Highlighter: TThItemShadowHighlighter;
begin
  Highlighter := TThItemShadowHighlighter.Create(Self);
  Highlighter.HighlightColor := ItemHighlightColor;
  Highlighter.HighlightSize := ItemHighlightMin;

  Result := Highlighter;
end;

function TThShape.CreateResizabler: IItemResizabler;
var
  Resizabler: TThItemFillResizabler;
begin
  Resizabler := TThItemFillResizabler.Create(Self);
  Resizabler.SetSpotClass(TThItemCircleResizableSpot);
  Resizabler.SetResizableSpots([rsdTopLeft, rsdTopRight, rsdBottomLeft, rsdBottomRight]);
  Resizabler.OnTrack := ResizableSpotTrack;

  Result := Resizabler;
end;

procedure TThShape.DoSelected(Selected: Boolean);
begin
  inherited;

  if Selected then
    ShowResizableSpots
  else
    HideResizableSpots;
end;

procedure TThShape.ShowResizableSpots;
var
  I: Integer;
  P: TPointF;
  R: TRectF;
  Spot: TItemResizableSpot;
begin
  R := GetShapeRect;

  for I := 0 to FResizabler.Count - 1 do
  begin
    Spot := TItemResizableSpot(FResizabler.Spots[I]);

    case Spot.Direction of
      rsdTopLeft:      P := PointF(R.Left, R.Top);
      rsdTop:          P := PointF(RectWidth(R) / 2, R.Top);
      rsdTopRight:     P := PointF(R.Right, R.Top);
      rsdLeft:         P := PointF(R.Left, RectHeight(R) / 2);
      rsdRight:        P := PointF(R.Right, RectHeight(R) / 2);
      rsdBottomLeft:   P := PointF(R.Left, R.Bottom);
      rsdBottom:       P := PointF(RectWidth(R) / 2, R.Bottom);
      rsdBottomRight:  P := PointF(R.Right, R.Bottom);
    end;

    Spot.Position.Point := P;
    Spot.Visible := True;
  end;
  Repaint;
end;

procedure TThShape.HideResizableSpots;
var
  I: Integer;
begin
  for I := 0 to FResizabler.Count - 1 do
    TControl(FResizabler.Spots[I]).Visible := False;
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

  if FSelected and (FUpdating = 0) then
    ShowResizableSpots;
end;

procedure TThShape.ResizableSpotTrack(Sender: TObject);
var
  R: TRectF;
  P: TPointF;
  Spot: TThItemCircleResizableSpot absolute Sender;
begin
  Debug('');
  R := GetShapeRect;
  R.Offset(Position.Point);
//  R.TopLeft := LocalTo
  P := Spot.Position.Point;
  P.Offset(Position.Point);

  /////////////////////
  // Refactoring ÇÊ¿ä
  /////////////////////

  if Spot.Direction = rsdBottomRight then
  begin
    if P.X < R.Left then
    begin
      R.Right := R.Left;
      R.Left := P.X;
    end
    else
      R.Right := P.X;

    if P.Y < R.Top then
    begin
      R.Bottom := R.Top;
      R.Top := R.Bottom;
    end
    else
      R.Bottom := P.Y
    ;
  end;
  SetBoundsRect(R);
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
