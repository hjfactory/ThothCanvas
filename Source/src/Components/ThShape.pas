{
  ThShape를 상속받아 도형을 구현
   - RegistItem에 반드시 추가 할 것

  주요 추상메소드
   - PaintItem  : 아이템을 캔버스에 그린다.(Paint에서 호출)
   - PtInItem   : Pt가 도형에 포함되는지 여부 반환
}
unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, ThTypes, ThItem;

type
  TThShape = class(TThItem, IItemHighlitObject, IItemResizerObject)
  private
    procedure SetBgColor(const Value: TAlphaColor);
  strict protected
    procedure Paint; override;
  protected
    FBgColor: TAlphaColor;

    function CreateHighlighter: IItemHighlighter; override;
    function CreateResizer: IItemResizer; override;

    // Abstract method
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    function PtInItem(Pt: TPointF): Boolean; override; abstract;

    function GetMinimumSize: TPointF; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

  TThRectangle = class(TThShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
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
    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
  end;

  TThCircle = class(TThShape)
  protected
    function CreateResizer: IItemResizer; override;

    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
  end;

implementation

uses
  System.Math, ThConsts, ThItemFactory, ThItemHighlighter,
  ResizeUtils, ThItemResizer, DebugUtils;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FWidth := MinimumSize.X;
  FHeight := MinimumSize.Y;

  FOpacity := ItemDefaultOpacity;
  FBgColor := ItemShapeDefaultColor;
end;

destructor TThShape.Destroy;
begin

  inherited;
end;

function TThShape.GetMinimumSize: TPointF;
var
  MinSize: Single;
begin
  MinSize := ItemMinimumSize / AbsoluteScale.X;

  Result := PointF(MinSize, MinSize);
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
  Resizer.OnTracking := nil;

  Result := Resizer;
end;

procedure TThShape.Paint;
{$IFDEF DEBUG}
var
  S: string;
{$ENDIF}
begin
  PaintItem(GetItemRect, FBgColor);

{$IFDEF DEBUG}
  S := Format('Position(%f, %f)', [Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.taCenter);
{$ENDIF}
end;

procedure TThShape.SetBgColor(const Value: TAlphaColor);
begin
  if FBgColor = Value then
    Exit;

  FBgColor := Value;
  Repaint;
end;

{$REGION RECTANGLE}
{ TThRectangle }

function TThRectangle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := False;
  if (AbsoluteRect.Width < ItemFocusMinimumSize) and (AbsoluteRect.Height < ItemFocusMinimumSize) then
    Exit;

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

procedure TThRectangle.DrawItemAtMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.X then
    ATo.X := AFrom.X + IfThen(AFrom.X > ATo.X, -1, 1) * MinimumSize.X;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Y then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y > ATo.Y, -1, 1) * MinimumSize.Y;

  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
end;
{$ENDREGION}

{$REGION LINE}
{ TThLine }

function TThLine.CreateResizer: IItemResizer;
var
  Resizer: TThItemResizer;
begin
  Resizer := TThLineResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scTopLeft, scBottomRight]);
  Resizer.OnTracking := nil;

  Result := Resizer;
end;

function TThLine.IsTopLeftToBottomRight: Boolean;
begin
  Result := TItemResizeSpot(FResizer.Spots[0]).SpotCorner in [scTopLeft, scBottomRight];
end;

function TThLine.IsHorizon: Boolean;
begin
  Result := TItemResizeSpot(FResizer.Spots[0]).Position.Y = TItemResizeSpot(FResizer.Spots[1]).Position.Y;
end;

function TThLine.IsVertical: Boolean;
begin
  Result := TItemResizeSpot(FResizer.Spots[0]).Position.X = TItemResizeSpot(FResizer.Spots[1]).Position.X;
end;

function TThLine.GetMinimumSize: TPointF;
var
  MinSize,
  Rad: Single;
  R: TRectF;
begin
  R := TThItemResizer(FResizer).GetActiveSpotsItemRect;

  MinSize := ItemMinimumSize / AbsoluteScale.X;

  if R.Height = 0 then
    Result := PointF(MinSize, 0)
  else if R.Width = 0 then
    Result := PointF(0, MinSize)
  else
  begin
    Rad := ArcTan(R.Height / R.Width);
    Result.X := Cos(Rad) * MinSize;
    Result.Y := Sin(Rad) * MinSize;
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
  RangeD := (ItemLineSelectionThickness-1)/2 / AbsoluteScale.X;

  if (Rect.TopLeft.Distance(Rect.BottomRight) < ItemFocusMinimumSize) then
    Exit;

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
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD));
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
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD));
    end;
  end
  else
  begin
    PtX := Pt.X;
    PtY := IfThen(IsTopLeftToBottomRight, Pt.Y, Rect.Height - Pt.Y);

    // 꼭지점의 원 포함 확인
    if not Result then
      Result := PtInCircle(PointF(PtX, PtY).Truncate, Rect.TopLeft.Truncate, Trunc(RangeD+1)) or  // 소수점 보정(+1)
                PtInCircle(PointF(PtX, PtY).Truncate, Rect.BottomRight.Truncate, Trunc(RangeD+1));

    if not Result then
    begin
      // 꼭지점과 직각인 사각형 포인트의 영역(ExtendRect)계산
      Rad := ArcTan(Rect.Height / Rect.Width);
      BaseP := PointF(RangeD / Sin(Rad), RangeD / Cos(Rad));
      LeftP   := Rect.TopLeft.Add(PointF(-BaseP.X, BaseP.Y));
      TopP    := Rect.TopLeft.Add(PointF(BaseP.X, -BaseP.Y));
      RightP  := Rect.BottomRight.Add(PointF(BaseP.X, -BaseP.Y));
      BottomP := Rect.BottomRight.Add(PointF(-BaseP.X, BaseP.Y));
      // 대각선에서 범위
      ExtendRect := RectF(LeftP.X, TopP.Y, RightP.X, BottomP.Y);

      if PtInRect(ExtendRect, PointF(PtX, PtY)) then
      begin
        ExtendX := PtX - BaseP.X;
        ExtendY := PtY - BaseP.Y;

        Result := PtInRect(RectF(LeftP.X, TopP.Y, TopP.X, LeftP.Y), PointF(PtX, PtY)) or
                  PtInRect(RectF(BottomP.X, RightP.Y, RightP.X, BottomP.Y), PointF(PtX, PtY));
        if not Result then
        begin
          Y1 := Round(Tan(Rad) * ExtendX - BaseP.Y);
          Y2 := Round(Tan(Rad) * ExtendX + BaseP.Y);
//          Result := InRange(ExtendY, Y1, Y2);
          Result := InRange(Round(ExtendY), Y1, Y2);
        end;
      end;
    end;
  end;
end;

procedure TThLine.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  P1, P2: TPointF;
  LineThickness: Single;
begin
  LineThickness := ItemLineThickness / AbsoluteScale.X;

  Canvas.StrokeThickness := LineThickness;
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

procedure TThLine.DrawItemAtMouse(AFrom, ATo: TPointF);
var
  Rect: TRectF;
  BaseSpot, ActiveSpot: TItemResizeSpot;
  Min: TPointF;
begin
  Rect := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  Rect.NormalizeRect;

  BaseSpot    := TItemResizeSpot(FResizer.Spots[0]);
  ActiveSpot  := TItemResizeSpot(FResizer.Spots[1]);
  BaseSpot.Position.Point   := AFrom.Subtract(Position.Point);;
  ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

  Min := MinimumSize;
  if (AFrom.Distance(ATo) < ItemMinimumSize / AbsoluteScale.X) and ((Rect.Width < Min.X) or (Rect.Height < Min.Y)) then
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
{$ENDREGION}

{$REGION Circle}
{ TThCircle }

function TThCircle.CreateResizer: IItemResizer;
var
  Resizer: TThItemResizer;
begin
  Resizer := TThCircleResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scLeft, scTop, scRight, scBottom]);
  Resizer.OnTracking := nil;

  Result := Resizer;
end;

procedure TThCircle.DrawItemAtMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.X then
    ATo.X := AFrom.X + IfThen(AFrom.X > ATo.X, -1, 1) * MinimumSize.X;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Y then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y > ATo.Y, -1, 1) * MinimumSize.Y;

  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
end;

procedure TThCircle.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  R: TRectF;
begin
  R := ARect;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := AFillColor;

  Canvas.FillEllipse(R, AbsoluteOpacity);
  Canvas.DrawEllipse(R, AbsoluteOpacity);
end;

function TThCircle.PtInItem(Pt: TPointF): Boolean;
begin
  Result := False;
  if (AbsoluteRect.Width < ItemFocusMinimumSize) and (AbsoluteRect.Height < ItemFocusMinimumSize) then
    Exit;

  if Width * Height = 0 then
    Exit;
  if (Sqr((Pt.X * 2 - Width) / Width) + Sqr((Pt.Y * 2 - Height) / Height) <= 1)
  then
  begin
    Result := True;
  end;
end;
{$ENDREGION}

initialization
  RegisterItem(1100, TThRectangle);
  RegisterItem(1200, TThLine);
  RegisterItem(1300, TThCircle);

end.
