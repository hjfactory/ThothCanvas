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

    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot; var ExchangedHorz, ExchangedVert: Boolean); virtual;    // Spot 이동 시 Shape 크기 조정
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot; ExchangedHorz, ExchangedVert: Boolean); virtual;  // Spot의 SpotCorner 조정

    procedure RealignSpot; virtual;

    function GetShapeRect: TRectF; virtual;
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

//    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); override;
//    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot; ExchangedHorz, ExchangedVert: Boolean); override;
    function GetMinimumSize: TPointF; override;
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
  ExchangedHorz, ExchangedVert: Boolean;
begin
  ResizeShapeBySpot(ActiveSpot,ExchangedHorz, ExchangedVert);

  if ExchangedHorz or ExchangedVert then
    NormalizeSpotCorner(ActiveSpot, ExchangedHorz, ExchangedVert);

  RealignSpot;
end;

procedure TThShape.ResizeShapeBySpot(ASpot: IItemResizeSpot; var ExchangedHorz, ExchangedVert: Boolean);
var
  ShapeR: TRectF;
  SpotPos: TPointF;
  ActiveSpot: TThItemCircleResizeSpot;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);

  ShapeR := GetShapeRect;
  SpotPos := ActiveSpot.Position.Point;

// Spot이 변경된 영역을 계산

  // Left to Right
  if ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) then
  begin
    if SpotPos.X > ShapeR.Right then
    begin
      ShapeR.Left := ShapeR.Right;
      ShapeR.Right := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Width := MinimumSize.X;
      ExchangedHorz := True;
    end
    else
    begin
      ShapeR.Left := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Left := ShapeR.Left - (MinimumSize.X - ShapeR.Width);
    end;
  end;

  // Right to Left
  if ContainSpotCorner(ActiveSpot.SpotCorner, scRight) then
  begin
    if SpotPos.X < ShapeR.Left then
    begin
      ShapeR.Right := ShapeR.Left;
      ShapeR.Left := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Left := ShapeR.Left - (MinimumSize.X - ShapeR.Width);
      ExchangedHorz := True;
    end
    else
    begin
      ShapeR.Right := SpotPos.X;
      if ShapeR.Width < MinimumSize.X then
        ShapeR.Width := MinimumSize.X;
    end;
  end;

  // Top to Bottom
  if ContainSpotCorner(ActiveSpot.SpotCorner, scTop) then
  begin

    if SpotPos.Y > ShapeR.Bottom then
    begin
      ShapeR.Top := ShapeR.Bottom;
      ShapeR.Bottom := SpotPos.Y;
      if ShapeR.Height < MinimumSize.Y then
        ShapeR.Height := MinimumSize.Y;
      ExchangedVert := True;
    end
    else
    begin
      ShapeR.Top := SpotPos.Y;
      if ShapeR.Height < MinimumSize.Y then
        ShapeR.Top := ShapeR.Top - (MinimumSize.Y - ShapeR.Height);
    end;
  end;

  // Bottom to Top
  if ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) then
  begin
    if SpotPos.Y < ShapeR.Top then
    begin
      ShapeR.Bottom := ShapeR.Top;
      ShapeR.Top := SpotPos.Y;
      if ShapeR.Height < MinimumSize.Y then
        ShapeR.Top := ShapeR.Top - (MinimumSize.Y - ShapeR.Height);
      ExchangedVert := True;
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
end;

procedure TThShape.NormalizeSpotCorner(ASpot: IItemResizeSpot; ExchangedHorz, ExchangedVert: Boolean);
var
  I: Integer;
  R: TRectF;
  Spot: TAbstractItemResizeSpot;
  ActiveSpot: TAbstractItemResizeSpot;
  ActiveSpotP: TPointF;
  SpotCorner: TSpotCorner;
begin
  R := GetShapeRect;
  R.Offset(Position.X, Position.Y);

  ActiveSpot := TAbstractItemResizeSpot(ASpot);
  ActiveSpotP := ActiveSpot.Position.Point;
  ActiveSpotP.Offset(Position.Point);
  SpotCorner := ActiveSpot.SpotCorner;

// 1, ActiveSpot(변경중인)의 변경 할 SpotCorner 계산

// 2, 본인 제외한 Spot에 대해 (1)에서
  // 2-1, 가로가 변경된 경우 가로 SpotCorner 변경
  // 2-2, 세로가 변경된 경우 세로 SpotCorner 변경

// 3, 본인 SpotCorner 적용

{1, }
  if ExchangedHorz then
  begin
    // Left to Right
    if ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) then
      if ActiveSpotP.X >= R.Right then
        SpotCorner := HorizonSpotCornerExchange(SpotCorner);

    // Right to Left
    if ContainSpotCorner(ActiveSpot.SpotCorner, scRight) then
      if ActiveSpotP.X <= R.Left then
        SpotCorner := HorizonSpotCornerExchange(SpotCorner);
  end;
  if ExchangedVert then
  begin
    // Top to Bottom
    if ContainSpotCorner(ActiveSpot.SpotCorner, scTop) then
      if ActiveSpotP.Y >= R.Bottom then
        SpotCorner := VertialSpotCornerExchange(SpotCorner);

    // Bottom to Top
    if ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) then
      if ActiveSpotP.Y <= R.Top then
        SpotCorner := VertialSpotCornerExchange(SpotCorner);
  end;

{2, }
  for I := 0 to FResizer.Count - 1 do
  begin
    Spot := TAbstractItemResizeSpot(FResizer.Spots[I]);

    if Spot = ActiveSpot then
      Continue;

{2.1, }
    // Switch horizon spot
    if IsHorizonExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := HorizonSpotCornerExchange(Spot.SpotCorner);

{2.2, }
    // Switch vertical spot
    if IsVertialExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := VertialSpotCornerExchange(Spot.SpotCorner);
  end;

{3, }
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
  R: TRectF;
  Rad: Single;
begin
  R := GetShapeRect;

//  Rad := ArcTan(R.Height/R.Width);

  Result := PointF(1, 1);
//  Result := PointF(ItemLineThickness, ItemLineThickness);
end;

function TThLine.GetShapeRect: TRectF;
begin
  Result.TopLeft :=   TThItemCircleResizeSpot(FResizer.Spots[0]).Position.Point;
  Result.BottomRight :=   TThItemCircleResizeSpot(FResizer.Spots[1]).Position.Point;
  Result.NormalizeRect;
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

//  if not PtInRect(Rect, Pt) then
//    Exit;

  RectW := Rect.Width;
  RectH := Rect.Height;

  D := (ItemLineThickness-1)/2;

  if RectH = 0 then
  begin
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
  else if RectW = 0 then
  begin
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
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).SpotCorner in [scLeft, scRight];
end;

function TThLine.IsVertical: Boolean;
begin
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).SpotCorner in [scTop, scBottom];
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
{

  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := claBlack;
  Canvas.DrawRect(GetShapeRect, 0, 0, AllCorners, 1);
  Canvas.DrawLine(P1, P2, 1);
}
//  Canvas.draw
//  Debug('Paint item (%f, %f) (%f, %f)', [P1.X, P1.Y, P2.X, P2.Y]);
end;

procedure TThLine.DrawingWithMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
  ExchangedHorz, ExchangedVert: Boolean;
  BaseSpot, ActiveSpot: TAbstractItemResizeSpot;
var
  ToPos, FromPos: TPointF;
begin
  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  if R.Width = 0 then   R.Right := R.Left + 1;
  if R.Height = 0 then  R.Bottom := R.Top + 1;

  R.NormalizeRect;
  BoundsRect := R;

  BaseSpot    := TThItemCircleResizeSpot(FResizer.Spots[0]);
  ActiveSpot  := TThItemCircleResizeSpot(FResizer.Spots[1]);
  BaseSpot.Position.Point   := AFrom.Subtract(Position.Point);;
  ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

  if AFrom.X = ATo.X then
  begin
    if AFrom.Y > ATo.Y then
    begin
      BaseSpot.SpotCorner := scTop;
      ActiveSpot.SpotCorner := scBottom;
    end
    else
    begin
      ActiveSpot.SpotCorner := scTop;
      BaseSpot.SpotCorner := scBottom;
    end;
  end
  else if AFrom.Y = ATo.Y then
  begin
    if AFrom.X > ATo.X then
    begin
      BaseSpot.SpotCorner := scLeft;
      ActiveSpot.SpotCorner := scRight;
    end
    else
    begin
      ActiveSpot.SpotCorner := scLeft;
      BaseSpot.SpotCorner := scRight;
    end;
  end
  else
  begin
    if AFrom.X > ATo.X then
    begin
      if AFrom.Y > ATo.Y then
        ActiveSpot.SpotCorner := scBottomRight
      else
        ActiveSpot.SpotCorner := scTopRight
      ;
    end
    else
    begin
      if AFrom.Y > ATo.Y then
        ActiveSpot.SpotCorner := scBottomLeft
      else
        ActiveSpot.SpotCorner := scTopLeft
      ;
    end;
    BaseSpot.SpotCorner := HorizonSpotCornerExchange(ActiveSpot.SpotCorner);
    BaseSpot.SpotCorner := VertialSpotCornerExchange(BaseSpot.SpotCorner);
  end;
end;

initialization
  RegisterItem(1100, TThRectangle);
  RegisterItem(1200, TThLine);

end.
