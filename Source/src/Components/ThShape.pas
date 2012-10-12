unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils,
  FMX.Types, ThItem, ThItemHighlighterIF, ThItemResizerIF, System.UIConsts;

type
{
  ShapeRect : LocalRect
  ClipRect  : ShapeRect + ShadowRect
}

  TThShape = class(TThItem, IItemHighlitObject, IItemResizerObject)
  private
    procedure SetBackgroundColor(const Value: TAlphaColor);
  strict protected
    procedure Paint; override;
  protected
    FBackgroundColor: TAlphaColor;

    function CreateHighlighter: IItemHighlighter; override;
    function CreateResizer: IItemResizer; override;

    // Abstract method
    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    function PtInItem(Pt: TPointF): Boolean; override; abstract;
    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); virtual; abstract;   // Spot 이동 시 Shape 크기 조정
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); virtual; abstract; // Spot의 SpotCorner 조정

    procedure RealignSpot; virtual;

    function GetShapeRect: TRectF; virtual;
    procedure ResizeSpotTrack(Sender: TObject; X, Y: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
  end;

  TThRectangle = class(TThShape)
  private
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); override;

    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); override;
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;
  end;

  TThLine = class(TThShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure DrawItem(ARect: TRectF; AFillColor: TAlphaColor); override;

    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); override;
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;
  end;

implementation

uses
  CommonUtils, ThConsts, ThItemFactory, ThItemHighlighter, ThItemResizer;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FWidth := MinimumSize.X;
  FHeight := MinimumSize.Y;

  FOpacity := ItemDefaultOpacity;
  FBackgroundColor := ItemShapeDefaultColor;
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
  Highlighter.HighlightSize := ItemHighlightSize;

  Result := Highlighter;
end;

function TThShape.CreateResizer: IItemResizer;
var
  Resizer: TThItemFillResizer;
begin
  Resizer := TThItemFillResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
  Resizer.OnTrack := ResizeSpotTrack;

  Result := Resizer;
end;

procedure TThShape.Paint;
{$IFDEF DEBUG}
var
  S: string;
{$ENDIF}
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
    RealignSpot;
end;

procedure TThShape.RealignSpot;
var
  I: Integer;
  SpotP: TPointF;
  ShapeR: TRectF;
  Spot: TAbstractItemResizeSpot;
begin
  ShapeR := GetShapeRect;

  for I := 0 to FResizer.Count - 1 do
  begin
    Spot := TAbstractItemResizeSpot(FResizer.Spots[I]);

    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ShapeR.Left, ShapeR.Top);
      scTop:          SpotP := PointF(RectWidth(ShapeR) / 2, ShapeR.Top);
      scTopRight:     SpotP := PointF(ShapeR.Right, ShapeR.Top);
      scLeft:         SpotP := PointF(ShapeR.Left, RectHeight(ShapeR) / 2);
      scRight:        SpotP := PointF(ShapeR.Right, RectHeight(ShapeR) / 2);
      scBottomLeft:   SpotP := PointF(ShapeR.Left, ShapeR.Bottom);
      scBottom:       SpotP := PointF(RectWidth(ShapeR) / 2, ShapeR.Bottom);
      scBottomRight:  SpotP := PointF(ShapeR.Right, ShapeR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  Repaint;
end;

procedure TThShape.ResizeSpotTrack(Sender: TObject; X, Y: Single);
begin
  ResizeShapeBySpot(TThItemCircleResizeSpot(Sender));
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TThShape.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor = Value then
    Exit;

  FBackgroundColor := Value;
  Repaint;
end;

{ TThRectangle }

function TThRectangle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, Pt);
end;

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

procedure TThRectangle.ResizeShapeBySpot(ASpot: IItemResizeSpot);
var
  ShapeR, BeforeR: TRectF;
  SpotPos: TPointF;
  ActiveSpot: TThItemCircleResizeSpot;
  Exchanged: Boolean;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);

  ShapeR := GetShapeRect;
  BeforeR := ClipRect;;
  SpotPos := ActiveSpot.Position.Point;

  Exchanged := False;

// Spot이 변경된 영역을 계산

  // Left to Right
  if scLeft = AndSpotCorner(ActiveSpot.SpotCorner, scLeft) then
  begin
    if SpotPos.X > ShapeR.Right then
    begin
      ShapeR.Left := ShapeR.Right;
      ShapeR.Right := SpotPos.X;
      Exchanged := True;
    end
    else
    begin
      ShapeR.Left := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Left := ShapeR.Left - (MinimumSize.X - ShapeR.Width);
    end;
  end;

  // Top to Bottom
  if scTop = AndSpotCorner(ActiveSpot.SpotCorner, scTop) then
  begin

    if SpotPos.Y > ShapeR.Bottom then
    begin
      ShapeR.Top := ShapeR.Bottom;
      ShapeR.Bottom := SpotPos.Y;
      Exchanged := True;
    end
    else
    begin
      ShapeR.Top := SpotPos.Y;
      if ShapeR.Height < MinimumSize.Y then
        ShapeR.Top := ShapeR.Top - (MinimumSize.Y - ShapeR.Height);
    end;
  end;

  // Right to Left
  if scRight = AndSpotCorner(ActiveSpot.SpotCorner, scRight) then
  begin
    if SpotPos.X < ShapeR.Left then
    begin
      ShapeR.Right := ShapeR.Left;
      ShapeR.Left := SpotPos.X;
      Exchanged := True;
    end
    else
    begin
      ShapeR.Right := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Width := MinimumSize.X;
    end;
  end;

  // Bottom to Top
  if scBottom = AndSpotCorner(ActiveSpot.SpotCorner, scBottom) then
  begin
    if SpotPos.Y < ShapeR.Top then
    begin
      ShapeR.Bottom := ShapeR.Top;
      ShapeR.Top := ShapeR.Bottom;
      Exchanged := True;
    end
    else
    begin
      ShapeR.Bottom := SpotPos.Y;
      if ShapeR.Height < MinimumSize.Y then
        ShapeR.Height := MinimumSize.Y;
    end;
  end;

  ShapeR.Offset(Position.Point);
  SetBoundsRect(ShapeR);

{$IFDEF DEBUG}
  if Width < 30 then
  begin
    Debug('[SpotCorner: %d] ShapeR: %f,%f,%f,%f / Shp : %f,%f,%f,%f',
      [
        Ord(ActiveSpot.SpotCorner),
        ShapeR.Left, ShapeR.Top, ShapeR.Right, ShapeR.Bottom,
        Position.X, Position.Y, Width, Height
      ]);
  end;

{$ENDIF}

  if Exchanged then
    NormalizeSpotCorner(ActiveSpot);
end;

procedure TThRectangle.NormalizeSpotCorner(ASpot: IItemResizeSpot);
  function _IsHorizontalExchange(D1, D2: TSpotCorner): Boolean;
  begin
    Result := AndSpotCorner(D1, HORIZONTAL_CORNERS) <> AndSpotCorner(D2, HORIZONTAL_CORNERS);
  end;

  function _IsVertialExchange(D1, D2: TSpotCorner): Boolean;
  begin
    Result := AndSpotCorner(D1, VERTICAL_CORNERS) <> AndSpotCorner(D2, VERTICAL_CORNERS);
  end;

  function _HorizontalExchange(D: TSpotCorner): TSpotCorner;
  begin
    Result := TSpotCorner(Ord(D) xor Ord(HORIZONTAL_CORNERS))
  end;

  function _VertialExchange(D: TSpotCorner): TSpotCorner;
  begin
    Result := TSpotCorner(Ord(D) xor Ord(VERTICAL_CORNERS))
  end;

var
  I: Integer;
  R: TRectF;
  Spot: TAbstractItemResizeSpot;
  ActiveSpot: TAbstractItemResizeSpot;
  SpotCorner: TSpotCorner;
begin
  R := GetShapeRect;

  ActiveSpot := TAbstractItemResizeSpot(ASpot);
  SpotCorner := ActiveSpot.SpotCorner;

// 1, ActiveSpot(변경중인)의 변경 할 SpotCorner 계산

// 2, 본인 제외한 Spot에 대해 (1)에서
  // 2-1, 가로가 변경된 경우 가로 SpotCorner 변경
  // 2-2, 세로가 변경된 경우 세로 SpotCorner 변경

// 3, 본인 SpotCorner 적용

{1, }
  // Left to Right
  if scLeft = AndSpotCorner(ActiveSpot.SpotCorner, scLeft) then
    if ActiveSpot.Position.X > R.Right then
      SpotCorner := _HorizontalExchange(SpotCorner);

  // Top to Bottom
  if scTop = AndSpotCorner(ActiveSpot.SpotCorner, scTop) then
    if ActiveSpot.Position.Y > R.Bottom then
      SpotCorner := _VertialExchange(SpotCorner);

  // Right to Left
  if scRight = AndSpotCorner(ActiveSpot.SpotCorner, scRight) then
    if ActiveSpot.Position.X < R.Left then
      SpotCorner := _HorizontalExchange(SpotCorner);

  // Bottom to Top
  if scBottom = AndSpotCorner(ActiveSpot.SpotCorner, scBottom) then
    if ActiveSpot.Position.Y < R.Top then
      SpotCorner := _VertialExchange(SpotCorner);

{2, }
  for I := 0 to FResizer.Count - 1 do
  begin
    Spot := TAbstractItemResizeSpot(FResizer.Spots[I]);

    if Spot = ActiveSpot then
      Continue;

{2.1, }
    // Switch width spot
    if _IsHorizontalExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := _HorizontalExchange(Spot.SpotCorner);

{2.2, }
    // Switch height spot
    if _IsVertialExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := _VertialExchange(Spot.SpotCorner);
  end;

{3, }
  ActiveSpot.SpotCorner := SpotCorner;
end;

{ TThLine }

function TThLine.PtInItem(Pt: TPointF): Boolean;
begin
  Result := False;
end;

procedure TThLine.DrawItem(ARect: TRectF; AFillColor: TAlphaColor);
begin
  Canvas.StrokeThickness := 7;
  Canvas.Stroke.Color := AFillColor;
  Canvas.StrokeCap := TStrokeCap.scRound;
  Canvas.DrawLine(ARect.TopLeft, ARect.BottomRight, 1);

//  Canvas.draw
end;

procedure TThLine.ResizeShapeBySpot(ASpot: IItemResizeSpot);
begin

end;

procedure TThLine.NormalizeSpotCorner(ASpot: IItemResizeSpot);
begin

end;

initialization
  RegisterItem(1100, TThRectangle);
  RegisterItem(1200, TThLine);

end.
