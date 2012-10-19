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
    procedure ResizeSpotTrack(Sender: TObject; X, Y: Single);
  strict protected
    procedure Paint; override;
  protected
    FBackgroundColor: TAlphaColor;

    function CreateHighlighter: IItemHighlighter; override;
    function CreateResizer: IItemResizer; override;

    // Abstract method
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    function PtInItem(Pt: TPointF): Boolean; override; abstract;

    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); virtual;    // Spot 이동 시 Shape 크기 조정
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); virtual;  // Spot의 SpotCorner 조정

    procedure RealignSpot; virtual;

    function GetShapeRect: TRectF; virtual;
    function GetActiveSpotsShapeRect(ASpot: IItemResizeSpot = nil): TRectF;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
  end;

  TThRectangle = class(TThShape)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;

    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;

//    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); override;
//    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot; ExchangedHorz, ExchangedVert: Boolean); override;
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
    function GetShapeRect: TRectF; override;

    function PtInItem(Pt: TPointF): Boolean; override;

    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;

    function GetMinimumSize: TPointF; override;
//    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot; var ExchangedHorz, ExchangedVert: Boolean); override;
//    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot; ExchangedHorz, ExchangedVert: Boolean); override;
  public
    procedure DrawingWithMouse(AFrom, ATo: TPointF); override;
  end;

implementation

uses
  CommonUtils, System.Math, ThConsts, ThItemFactory, ThItemHighlighter, ThItemResizer;

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

function TThShape.GetActiveSpotsShapeRect(ASpot: IItemResizeSpot): TRectF;
var
  ActiveSpot,
  OppositeSpot: TAbstractItemResizeSpot;
begin
  if not Assigned(ASpot) then
    ASpot := FResizer.Spots[0];
  ActiveSpot := TAbstractItemResizeSpot(ASpot);
  OppositeSpot := TAbstractItemResizer(FResizer).GetSpot(SpotCornerExchange(ActiveSpot.SpotCorner));
  if not Assigned(OppositeSpot) then
    Exit;
  Result.TopLeft := ActiveSpot.Position.Point;
  Result.BottomRight := OppositeSpot.Position.Point;
  Result.NormalizeRect;
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
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
  PaintItem(GetShapeRect, FBackgroundColor);

{$IFDEF DEBUG}
  S := Format('Position(%f, %f)', [Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.taCenter);
{$ENDIF}

//  if FSelected and (FUpdating = 0) then
//    RealignSpot;
end;

procedure TThShape.ResizeSpotTrack(Sender: TObject; X, Y: Single);
var
  ActiveSpot: TThItemCircleResizeSpot absolute Sender;
begin
  ResizeShapeBySpot(ActiveSpot);
  NormalizeSpotCorner(ActiveSpot);
  RealignSpot;
end;

procedure TThShape.ResizeShapeBySpot(ASpot: IItemResizeSpot);
var
  MinSize: TPointF;
  ShapeR: TRectF;
  ActiveSpot, OppositeSpot: TThItemCircleResizeSpot;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);
  OppositeSpot := TThItemCircleResizeSpot(TAbstractItemResizer(FResizer).GetSpot(SpotCornerExchange(ActiveSpot.SpotCorner)));

  ShapeR.TopLeft := ActiveSpot.Position.Point;
  ShapeR.BottomRight := OppositeSpot.Position.Point;
  ShapeR.NormalizeRect;

  MinSize := MinimumSize;
  if ShapeR.Width < MinSize.X then
  begin
    if ShapeR.Right = ActiveSpot.Position.X then
      ShapeR.Right := ShapeR.Left + MinSize.X
    else
      ShapeR.Left := ShapeR.Right - MinSize.X;
  end;

  if ShapeR.Height < MinSize.Y then
  begin
    if ShapeR.Bottom = ActiveSpot.Position.Y then
      ShapeR.Bottom := ShapeR.Top + MinSize.Y
    else
      ShapeR.Top := ShapeR.Bottom - MinSize.Y;
  end;

  if ShapeR.Width = 0 then  ShapeR.Width := 1;
  if ShapeR.Height = 0 then  ShapeR.Height := 1;

  ShapeR.Offset(Position.Point);
  SetBoundsRect(ShapeR);
end;

procedure TThShape.NormalizeSpotCorner(ASpot: IItemResizeSpot);
var
  I: Integer;
  SpotRect: TRectF;
  Spot: TAbstractItemResizeSpot;
  ActiveSpot: TAbstractItemResizeSpot;
  SpotPos: TPointF;
  SpotCorner: TSpotCorner;
begin
  SpotRect := GetActiveSpotsShapeRect(ASpot);

  ActiveSpot := TAbstractItemResizeSpot(ASpot);
  SpotPos := ActiveSpot.Position.Point;

  if SpotRect.Width = 0 then
    SpotCorner := IfThenSpotCorner(SpotRect.Top = SpotPos.Y, scTop, scBottom)
  else if SpotRect.Height = 0 then
    SpotCorner := IfThenSpotCorner(SpotRect.Left = SpotPos.X, scLeft, scRight)
  else
  begin
    SpotCorner := scUnknown;
    if SpotRect.Left = SpotPos.X then
      SpotCorner := SetHorizonSpotCorner(SpotCorner, scLeft)
    else if SpotRect.Right = SpotPos.X then
      SpotCorner := SetHorizonSpotCorner(SpotCorner, scRight);

    if SpotRect.Top = SpotPos.Y then
      SpotCorner := SetHorizonSpotCorner(SpotCorner, scTop)
    else if SpotRect.Bottom = SpotPos.Y then
      SpotCorner := SetHorizonSpotCorner(SpotCorner, scBottom);
  end;

  for I := 0 to FResizer.Count - 1 do
  begin
    Spot := TAbstractItemResizeSpot(FResizer.Spots[I]);

    if Spot = ActiveSpot then
      Continue;

    if not SupportedHorizonSpotCorner(SpotCorner) and SupportedHorizonSpotCorner(Spot.SpotCorner) then
    begin
      Spot.SpotCorner := VerticalSpotCornerExchange(SpotCorner);
      Continue;
    end;

    if SupportedHorizonSpotCorner(SpotCorner) and not SupportedHorizonSpotCorner(Spot.SpotCorner) then
    begin
      Spot.SpotCorner := SpotCornerExchange(SpotCorner);
      Continue;
    end;

    if not SupportedVerticalSpotCorner(SpotCorner) and SupportedVerticalSpotCorner(Spot.SpotCorner) then
    begin
      Spot.SpotCorner := HorizonSpotCornerExchange(SpotCorner);
      Continue;
    end;

    if SupportedVerticalSpotCorner(SpotCorner) and not SupportedVerticalSpotCorner(Spot.SpotCorner) then
    begin
      Spot.SpotCorner := SpotCornerExchange(SpotCorner);
      Continue;
    end;

    // Switch horizon spot
    if ChangeHorizonSpotCorner(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := HorizonSpotCornerExchange(Spot.SpotCorner);

    // Switch vertical spot
    if ChangeVerticalSpotCorner(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := VerticalSpotCornerExchange(Spot.SpotCorner);
  end;

  ActiveSpot.SpotCorner := SpotCorner;
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
{
    // Line의 경우 수직/수평선인 경우 두께가 1이지만 0 포인트에 표시해 주어야 함
      // NormalizeSpotCorner 조건에 안맞을 수 있음
}
    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ShapeR.Left, ShapeR.Top);
      scTop:          SpotP := PointF(IfThen(ShapeR.Width = 1, 0, RectWidth(ShapeR)/2), ShapeR.Top);
      scTopRight:     SpotP := PointF(ShapeR.Right, ShapeR.Top);
      scLeft:         SpotP := PointF(ShapeR.Left, IfThen(ShapeR.Height = 1, 0, RectHeight(ShapeR)/2));
      scRight:        SpotP := PointF(ShapeR.Right, IfThen(ShapeR.Height = 1, 0, RectHeight(ShapeR)/2));
      scBottomLeft:   SpotP := PointF(ShapeR.Left, ShapeR.Bottom);
      scBottom:       SpotP := PointF(IfThen(ShapeR.Width = 1, 0, RectWidth(ShapeR)/2), ShapeR.Bottom);
      scBottomRight:  SpotP := PointF(ShapeR.Right, ShapeR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  Repaint;
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

  TThItemFillResizer(Result).SetResizeSpots([scTopLeft, scBottomRight]);
end;

function TThLine.GetMinimumSize: TPointF;
var
  Rad: Single;
  R: TRectF;
begin
  R:= GetActiveSpotsShapeRect;

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

function TThLine.GetShapeRect: TRectF;
begin
  Result := LocalRect;
end;

function TThLine.PtInItem(Pt: TPointF): Boolean;
var
  R: Single;
  D: Single;
  Rect: TRectF;
  RectW, RectH,
  PtX, PtY: Single;

  P: TPointF;
  TopP, LeftP, RightP, BottomP: TPointF;
  ExtendRect: TRectF;
  ExtendX, ExtendY: Single;

  Y1, Y2: Single;
begin
  Result := False;

  Rect := GetShapeRect;

  RectW := Rect.Width;
  RectH := Rect.Height;

  D := (ItemLineThickness-1)/2;

  if IsHorizon then
  begin
    Rect.Height := 0;
    if (Pt.X >= Rect.Left) and (Pt.X <= Rect.Right) then
    begin
      Result := Abs(Pt.Y) <= D;
    end
    else
    begin
      if Pt.X < Rect.Left then
        Result := PtInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(D))
      else
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(D))
      ;
    end;
  end
  else if IsVertical then
  begin
    Rect.Width := 0;
    if (Pt.Y >= Rect.Top) and (Pt.Y <= Rect.Bottom) then
    begin
      Result := Abs(Pt.X) <= D;
    end
    else
    begin
      if Pt.Y < Rect.Top then
        Result := PtInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(D))
      else
        Result := PtInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(D))
      ;
    end;
  end
  else
  begin
    PtX := Pt.X;
    PtY := IfThen(IsTopLeftToBottomRight, Pt.Y, RectH - Pt.Y);

    // 꼭지점의 원 포함 확인
    if not Result then
      Result := PtInCircle(PointF(PtX, PtY).Truncate, Rect.TopLeft.Truncate, Trunc(D)) or
                PtInCircle(PointF(PtX, PtY).Truncate, Rect.BottomRight.Truncate, Trunc(D));

    // 꼭지점과 직각인 사각형 포인트의 영역(ExtendRect)계산
    R := ArcTan(RectH / RectW);
    P := PointF(Sin(R) * D, Cos(R) * D);
    LeftP   := Rect.TopLeft.Add(PointF(-P.X, P.Y)).Add(P);
    TopP    := Rect.TopLeft.Add(PointF(P.X, -P.Y)).Add(P);
    RightP  := Rect.BottomRight.Add(PointF(P.X, -P.Y)).Add(P);
    BottomP := Rect.BottomRight.Add(PointF(-P.X, P.Y)).Add(P);
    ExtendRect := RectF(LeftP.X, TopP.Y, RightP.X, BottomP.Y);
//    ExtendRect.Offset(-LeftP.X, -TopP.Y);
    if (not Result) and PtInRect(ExtendRect, PointF(PtX, PtY)) then
    begin
      ExtendX := PtX;
      ExtendY := PtY;

      Result := PtInRect(RectF(LeftP.X, TopP.Y, TopP.X, LeftP.Y), PointF(ExtendX, ExtendY)) or
                PtInRect(RectF(BottomP.X, RightP.Y, RightP.X, BottomP.Y), PointF(ExtendX, ExtendY));
      if not Result then
      begin
        Y1 := Tan(R) * (ExtendX-TopP.X);
        Y2 := ExtendRect.Height - Tan(R) * (BottomP.X - ExtendX);
        Result := InRange(ExtendY, Y1, Y2);
      end;
    end;
  end;
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
  R: TRectF;
  BaseSpot, ActiveSpot: TAbstractItemResizeSpot;
  Min: TPointF;
begin
  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;

  BaseSpot    := TThItemCircleResizeSpot(FResizer.Spots[0]);
  ActiveSpot  := TThItemCircleResizeSpot(FResizer.Spots[1]);
  BaseSpot.Position.Point   := AFrom.Subtract(Position.Point);;
  ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

  Min := MinimumSize;
  if (AFrom.Distance(ATo) < ItemMinimumSize) and ((R.Width < Min.X) or (R.Height < Min.Y)) then
  begin
    if InRange(R.Width, 1, Min.X - 1) then
      ATo.X := AFrom.X + Min.X * IfThen(AFrom.X > ATo.X, -1, 1);
    if InRange(R.Height, 1, Min.Y - 1) then
      ATo.Y := AFrom.Y + Min.Y * IfThen(AFrom.Y > ATo.Y, -1, 1);

    ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

    R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
    R.NormalizeRect;
  end;

  if R.Width < 1 then   R.Width := 1;
  if R.Height < 1 then  R.Height := 1;
  BoundsRect := R;

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
