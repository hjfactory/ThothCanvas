unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, ThTypes, ThItem, ThItemHighlighterIF, ThItemResizerIF;

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
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    function PtInItem(Pt: TPointF): Boolean; override; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
  end;

  TThRectangle = class(TThShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawingWithMouse(AFrom, ATo: TPointF); override;
  end;

  TThLine = class(TThShape)
  private
    function IsTopLeftToBottomRight: Boolean;
    function IsHorizon: Boolean;
    function IsVertical: Boolean;
  protected
    function CreateResizer: IItemResizer; override;

    function GetMinimumSize: TPointF; override;

    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawingWithMouse(AFrom, ATo: TPointF); override;
  end;

implementation

uses
  CommonUtils, System.Math, ThConsts, ThItemFactory, ThItemHighlighter,
  ResizeUtils, ThItemResizer;

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
  Resizer: TThItemResizer;
begin
  Resizer := TThItemResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
  Resizer.OnTrack := nil;

  Result := Resizer;
end;

procedure TThShape.Paint;
{$IFDEF DEBUG}
var
  S: string;
{$ENDIF}
begin
  PaintItem(GetItemRect, FBackgroundColor);

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

function TThRectangle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetItemRect, Pt);
end;

procedure TThRectangle.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
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

procedure TThRectangle.DrawingWithMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.X then
    ATo.X := AFrom.X + IfThen(AFrom.X < ATo.X, 1, -1) * MinimumSize.X;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Y then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y < ATo.Y, 1, -1) * MinimumSize.Y;

  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
end;

{ TThLine }

function TThLine.CreateResizer: IItemResizer;
begin
  Result := inherited;

  TThItemResizer(Result).SetResizeSpots([scTopLeft, scBottomRight]);
end;

function TThLine.IsTopLeftToBottomRight: Boolean;
begin
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).SpotCorner in [scTopLeft, scBottomRight];
end;

function TThLine.IsHorizon: Boolean;
begin
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).Position.Y = TThItemCircleResizeSpot(FResizer.Spots[1]).Position.Y;
end;

function TThLine.IsVertical: Boolean;
begin
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).Position.X = TThItemCircleResizeSpot(FResizer.Spots[1]).Position.X;
end;

function TThLine.GetMinimumSize: TPointF;
var
  Rad: Single;
  R: TRectF;
begin
  R := TThItemResizer(FResizer).GetActiveSpotsItemRect;

  if R.Height = 0 then
    Result := PointF(ItemMinimumSize, 0)
  else if R.Width = 0 then
    Result := PointF(0, ItemMinimumSize)
  else
  begin
    Rad := ArcTan(R.Height / R.Width);
    Result.X := Cos(Rad) * ItemMinimumSize;
    Result.Y := Sin(Rad) * ItemMinimumSize;
  end;
end;

function TThLine.PtInItem(Pt: TPointF): Boolean;
var
  RangeD: Single;
  Rect: TRectF;
  PtX, PtY: Single;

  Rad: Single;
  BaseP: TPointF;
  TopP, LeftP, RightP, BottomP: TPointF;
  ExtendRect: TRectF;
  ExtendX, ExtendY: Single;

  Y1, Y2: Single;
begin
  Result := False;

  Rect := GetItemRect;

  RangeD := (ItemLineThickness-1)/2;

  if IsHorizon then
  begin
    Rect.Height := 0;
    if (Pt.X >= Rect.Left) and (Pt.X <= Rect.Right) then
    begin
      Result := Abs(Pt.Y) <= RangeD;
    end
    else
    begin
      if Pt.X < Rect.Left then
        Result := PtInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(RangeD))
      else
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD))
      ;
    end;
  end
  else if IsVertical then
  begin
    Rect.Width := 0;
    if (Pt.Y >= Rect.Top) and (Pt.Y <= Rect.Bottom) then
    begin
      Result := Abs(Pt.X) <= RangeD;
    end
    else
    begin
      if Pt.Y < Rect.Top then
        Result := PtInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(RangeD))
      else
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD))
      ;
    end;
  end
  else
  begin
    PtX := Pt.X;
    PtY := IfThen(IsTopLeftToBottomRight, Pt.Y, Rect.Height - Pt.Y);

    // 꼭지점의 원 포함 확인
    if not Result then
      Result := PtInCircle(PointF(PtX, PtY).Truncate, Rect.TopLeft.Truncate, Trunc(RangeD)) or
                PtInCircle(PointF(PtX, PtY).Truncate, Rect.BottomRight.Truncate, Trunc(RangeD));

    // 꼭지점과 직각인 사각형 포인트의 영역(ExtendRect)계산
    Rad := ArcTan(Rect.Height / Rect.Width);
    BaseP := PointF(Sin(Rad) * RangeD, Cos(Rad) * RangeD);
    LeftP   := Rect.TopLeft.Add(PointF(-BaseP.X, BaseP.Y)).Add(BaseP);
    TopP    := Rect.TopLeft.Add(PointF(BaseP.X, -BaseP.Y)).Add(BaseP);
    RightP  := Rect.BottomRight.Add(PointF(BaseP.X, -BaseP.Y)).Add(BaseP);
    BottomP := Rect.BottomRight.Add(PointF(-BaseP.X, BaseP.Y)).Add(BaseP);
    // 대각선에서 범위
    ExtendRect := RectF(LeftP.X, TopP.Y, RightP.X, BottomP.Y);

    if (not Result) and PtInRect(ExtendRect, PointF(PtX, PtY)) then
    begin
      ExtendX := PtX;
      ExtendY := PtY;

      Result := PtInRect(RectF(LeftP.X, TopP.Y, TopP.X, LeftP.Y), PointF(ExtendX, ExtendY)) or
                PtInRect(RectF(BottomP.X, RightP.Y, RightP.X, BottomP.Y), PointF(ExtendX, ExtendY));
      if not Result then
      begin
        Y1 := Tan(Rad) * (ExtendX-TopP.X);
        Y2 := ExtendRect.Height - Tan(Rad) * (BottomP.X - ExtendX);
        Result := InRange(ExtendY, Y1, Y2);
      end;
    end;
  end;
end;

procedure TThLine.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  P1, P2: TPointF;
begin
  Canvas.StrokeThickness := ItemLineThickness;
  Canvas.Stroke.Color := AFillColor;
  Canvas.StrokeCap := TStrokeCap.scRound;

  if IsTopLeftToBottomRight then
  begin
    P1 := ARect.TopLeft;
    P2 := ARect.BottomRight;
  end
  else if IsHorizon then
  begin
    P1 := PointF(ARect.Left, ARect.Top);
    P2 := PointF(ARect.Right, ARect.Top);
  end
  else if IsVertical then
  begin
    P1 := PointF(ARect.Left, ARect.Top);
    P2 := PointF(ARect.Left, ARect.Bottom);
  end
  else
  begin
    P1 := PointF(ARect.Left, ARect.Bottom);
    P2 := PointF(ARect.Right, ARect.Top);
  end;

  Canvas.DrawLine(P1, P2, 1);
end;

procedure TThLine.DrawingWithMouse(AFrom, ATo: TPointF);
var
  Rect: TRectF;
  BaseSpot, ActiveSpot: TItemResizeSpot;
  Min: TPointF;
begin
  Rect := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  Rect.NormalizeRect;

  BaseSpot    := TThItemCircleResizeSpot(FResizer.Spots[0]);
  ActiveSpot  := TThItemCircleResizeSpot(FResizer.Spots[1]);
  BaseSpot.Position.Point   := AFrom.Subtract(Position.Point);;
  ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

  Min := MinimumSize;
  if (AFrom.Distance(ATo) < ItemMinimumSize) and ((Rect.Width < Min.X) or (Rect.Height < Min.Y)) then
  begin
    if InRange(Rect.Width, 1, Min.X - 1) then
      ATo.X := AFrom.X + Min.X * IfThen(AFrom.X > ATo.X, -1, 1);
    if InRange(Rect.Height, 1, Min.Y - 1) then
      ATo.Y := AFrom.Y + Min.Y * IfThen(AFrom.Y > ATo.Y, -1, 1);

    ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

    Rect := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
    Rect.NormalizeRect;
  end;

  if Rect.Width < 1 then   Rect.Width := 1;
  if Rect.Height < 1 then  Rect.Height := 1;
  BoundsRect := Rect;

  if AFrom.X = ATo.X then // vertical
  begin
    BaseSpot.SpotCorner := IfThenSpotCorner(AFrom.Y > ATo.Y, scBottom, scTop);
    ActiveSpot.SpotCorner := IfThenSpotCorner(AFrom.Y > ATo.Y, scTop, scBottom);
  end
  else if AFrom.Y = ATo.Y then  // horizon
  begin
    BaseSpot.SpotCorner := IfThenSpotCorner(AFrom.X > ATo.X, scRight, scLeft);
    ActiveSpot.SpotCorner := IfThenSpotCorner(AFrom.X > ATo.X, scLeft, scRight);
  end
  else
  begin
    if AFrom.X > ATo.X then
      ActiveSpot.SpotCorner := IfThenSpotCorner(AFrom.Y > ATo.Y, scBottomRight, scTopRight)
    else
      ActiveSpot.SpotCorner := IfThenSpotCorner(AFrom.Y > ATo.Y, scBottomLeft, scTopLeft);
    BaseSpot.SpotCorner := HorizonSpotCornerExchange(ActiveSpot.SpotCorner);
    BaseSpot.SpotCorner := VerticalSpotCornerExchange(BaseSpot.SpotCorner);
  end;
end;

initialization
  RegisterItem(1100, TThRectangle);
  RegisterItem(1200, TThLine);

end.
