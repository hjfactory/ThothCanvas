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
  protected
    function CreateResizer: IItemResizer; override;

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
begin
  ResizeShapeBySpot(TThItemCircleResizeSpot(Sender));
  RealignSpot;
end;

procedure TThShape.ResizeShapeBySpot(ASpot: IItemResizeSpot);
var
  ShapeR: TRectF;
  SpotPos: TPointF;
  ActiveSpot: TThItemCircleResizeSpot;
  ExchangedHorz, ExchangedVert: Boolean;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);

  ShapeR := GetShapeRect;
  SpotPos := ActiveSpot.Position.Point;

  ExchangedHorz := False;
  ExchangedVert   := False;

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

  if ExchangedHorz or ExchangedVert then
    NormalizeSpotCorner(ActiveSpot, ExchangedHorz, ExchangedVert);
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
        SpotCorner := HorizontalExchange(SpotCorner);

    // Right to Left
    if ContainSpotCorner(ActiveSpot.SpotCorner, scRight) then
      if ActiveSpotP.X <= R.Left then
        SpotCorner := HorizontalExchange(SpotCorner);
  end;
  if ExchangedVert then
  begin
    // Top to Bottom
    if ContainSpotCorner(ActiveSpot.SpotCorner, scTop) then
      if ActiveSpotP.Y >= R.Bottom then
        SpotCorner := VertialExchange(SpotCorner);

    // Bottom to Top
    if ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) then
      if ActiveSpotP.Y <= R.Top then
        SpotCorner := VertialExchange(SpotCorner);
  end;

{2, }
  for I := 0 to FResizer.Count - 1 do
  begin
    Spot := TAbstractItemResizeSpot(FResizer.Spots[I]);

    if Spot = ActiveSpot then
      Continue;

{2.1, }
    // Switch horizontal spot
    if IsHorizontalExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := HorizontalExchange(Spot.SpotCorner);

{2.2, }
    // Switch vertical spot
    if IsVertialExchange(ActiveSpot.SpotCorner, SpotCorner) then
      Spot.SpotCorner := VertialExchange(Spot.SpotCorner);
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
var
  Resizer: TThItemFillResizer;
begin
  Resizer := TThItemFillResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scTopLeft, scBottomRight]);
  Resizer.OnTrack := ResizeSpotTrack;

  Result := Resizer;
end;

function TThLine.GetMinimumSize: TPointF;
begin
//  Result := PointF(3,3);
  Result := PointF(ItemLineThickness, ItemLineThickness);
//  Result := PointF(30, ItemLineThickness);
end;

function TThLine.PtInItem(Pt: TPointF): Boolean;
var
  Rad: Single;
  RectW, RectH,
  ValidYofPtX, ValidYofR: Single;
  Range: Single;
  RangeX, RangeY: Single;
  PtX, PtY: Single;
begin
  Result := False;

  RectW := GetShapeRect.Width;
  RectH := GetShapeRect.Height;

  PtX := Pt.X;
  if IsTopLeftToBottomRight then
    PtY := Pt.Y
  else
    PtY := RectH - Pt.Y;

  if RectW = 0 then
  begin
    Result := True;
  end
  else
  begin
    // #43 첨부파일 참고 - 로직 설명 이미지(LineSelect.jpg)
    // 1, PtX의 유효한 Y값 계산
    Rad := ArcTan(RectH/RectW);
    ValidYofPtX  := Tan(Rad) * PtX;

    // 2, 위의 X, Y값에서 Range내의 X, Y(RangeX, RangeY) 값 계산
    Range       := ItemLineThickness / 2;
    RangeX      := Range / Cos(DegToRad(90) - Rad);
    ValidYofR   := Tan(Rad) * (PtX - RangeX);
    RangeY      := Abs(ValidYofR - ValidYofPtX);

    // 3, PtY 값이 위의 RangeY값 내에 있으면 선택
    Result := Abs(ValidYofPtX - PtY) < RangeY;
  end;
end;

function TThLine.IsTopLeftToBottomRight: Boolean;
begin
  Result := TThItemCircleResizeSpot(FResizer.Spots[0]).SpotCorner in [scTopLeft, scBottomRight];
end;

procedure TThLine.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  P1, P2: TPointF;
begin
  Canvas.StrokeThickness := ItemLineThickness;
  Canvas.Stroke.Color := AFillColor;
  Canvas.StrokeCap := TStrokeCap.scRound;

//  if True then

  if IsTopLeftToBottomRight then
  begin
    P1 := ARect.TopLeft;
    P2 := ARect.BottomRight;
  end
  else
  begin
    P1 := PointF(ARect.Right, ARect.Top);
    P2 := PointF(ARect.Left, ARect.Bottom);
  end;

  Canvas.DrawLine(P1, P2, 1);
//  Canvas.draw
//  Debug('Paint item (%f, %f) (%f, %f)', [P1.X, P1.Y, P2.X, P2.Y]);
end;

procedure TThLine.DrawingWithMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
  ExchangedHorz, ExchangedVert: Boolean;
  BaseSpot, ActiveSpot: TAbstractItemResizeSpot;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.X then
    ATo.X := AFrom.X + IfThen(AFrom.X < ATo.X, 1, -1) * MinimumSize.X;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Y then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y < ATo.Y, 1, -1) * MinimumSize.Y;
//  inherited;
  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
//  Debug('DrawingWithMouse (%f, %f) (%f, %f) (%f, %f, %f, %f)',
//    [AFrom.X, AFrom.Y, ATo.X, ATo.Y, R.Left, R.Top, R.Right, R.Bottom]);

  BaseSpot    := TThItemCircleResizeSpot(FResizer.Spots[0]);
  ActiveSpot  := TThItemCircleResizeSpot(FResizer.Spots[1]);
  BaseSpot.Position.Point   := AFrom.Subtract(Position.Point);;
  ActiveSpot.Position.Point := ATo.Subtract(Position.Point);;

  ExchangedHorz := (ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) and (ATo.X = R.Right)) or
                    (ContainSpotCorner(ActiveSpot.SpotCorner, scRight) and (ATo.X = R.Left));
  ExchangedVert := (ContainSpotCorner(ActiveSpot.SpotCorner, scTop) and (ATo.Y = R.Bottom)) or
                    (ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) and (ATo.Y = R.Top));

  if ExchangedHorz or ExchangedVert then
    NormalizeSpotCorner(ActiveSpot, ExchangedHorz, ExchangedVert);
end;

initialization
  RegisterItem(1100, TThRectangle);
  RegisterItem(1200, TThLine);

end.
