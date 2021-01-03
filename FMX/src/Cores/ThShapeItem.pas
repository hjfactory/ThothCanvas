{
  ThShape�� ��ӹ޾� ������ ����
   - RegistItem�� �ݵ�� �߰� �� ��

  �ֿ� �߻�޼ҵ�
   - PaintItem  : �������� ĵ������ �׸���.(Paint���� ȣ��)
   - PtInItem   : Pt�� ������ ���ԵǴ��� ���� ��ȯ
}
unit ThShapeItem;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, ThTypes, ThItem;

type
  TThShapeItem = class(TThItem, IItemHighlightObject, IItemSelectionObject)
  private
    procedure SetBgColor(const Value: TAlphaColor);
  protected
    procedure Paint; override;
  protected
    FBgColor: TAlphaColor;

    function CreateHighlighter: IItemHighlighter; override;
    function CreateSelection: IItemSelection; override;

    // Abstract method
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); virtual; abstract;
    function PtInItem(Pt: TPointF): Boolean; override; abstract;

    function GetMinimumSize: TSizeF; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsContain(AChild: TThItem): Boolean; override;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
    property MinimumSize: TSizeF read GetMinimumSize;
  end;

  TThRectangle = class(TThShapeItem)
  protected
    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
  end;

  TThLine = class(TThShapeItem)
  private
    function IsTopLeftToBottomRight: Boolean;
    function IsHorizon: Boolean;
    function IsVertical: Boolean;
  protected
    function CreateSelection: IItemSelection; override;

    function GetMinimumSize: TSizeF; override;

    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    function IsContain(AChild: TThItem): Boolean; override;

    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
  end;

  TThCircle = class(TThShapeItem)
  protected
    function CreateSelection: IItemSelection; override;

    function PtInItem(Pt: TPointF): Boolean; override;
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor); override;
  public
    procedure DrawItemAtMouse(AFrom, ATo: TPointF); override;
  end;

implementation

uses
  System.Math, FMX.Controls, FMX.Graphics, ThConsts, ThItemFactory, ThItemHighlighter,
  SpotCornerUtils, ThItemSelection, DebugUtils;

{ TThShape }

constructor TThShapeItem.Create(AOwner: TComponent);
begin
  inherited;

  Width := MinimumSize.Width;
  Height := MinimumSize.Height;

  FOpacity := ItemDefaultOpacity;
  FBgColor := ItemShapeDefaultColor;
  SetAcceptsControls(False);
end;

destructor TThShapeItem.Destroy;
begin

  inherited;
end;

function TThShapeItem.GetMinimumSize: TSizeF;
var
  MinSize: Single;
begin
  MinSize := ItemMinimumSize / AbsoluteScale.X;

  Result := PointF(MinSize, MinSize);
end;

function TThShapeItem.IsContain(AChild: TThItem): Boolean;
begin
  Result := AbsoluteRect.Contains(AChild.AbsoluteRect);
end;

function TThShapeItem.CreateHighlighter: IItemHighlighter;
var
  Highlighter: TThItemShadowHighlighter;
begin
  Highlighter := TThItemShadowHighlighter.Create(Self);
  Highlighter.HighlightColor := ItemHighlightColor;
  Highlighter.HighlightSize := ItemHighlightSize;

  Result := Highlighter;
end;

function TThShapeItem.CreateSelection: IItemSelection;
var
  Selection: TThItemSelection;
begin
  Selection := TThItemSelection.Create(Self);
{$IFDEF ON_ALLCORNER_RESIZESPOT}
  Selection.SetResizeSpots([scLeft, scTop, scRight, scBottom, scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
{$ELSE}
  Selection.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
{$ENDIF}
  Selection.OnTracking := SpotTracking;

  Result := Selection;
end;

procedure TThShapeItem.Paint;
{$IFDEF DEBUG}
var
  S: string;
{$ENDIF}
begin
  PaintItem(GetItemRect, FBgColor);

{$IFDEF DEBUG}
  S := Format('(%s<P:%s>)Position(%f, %f)', [Name, Parent.ClassName, Position.X, Position.Y]);
  S := S + Format(' W, H(%f, %f)', [Width, Height]);
  Canvas.Fill.Color := claRed;
  Canvas.Font.Size := 10 / AbsoluteScale.X;
  Canvas.FillText(ClipRect, S, True, 1, [], TTextAlign.Center);
{$ENDIF}
end;

procedure TThShapeItem.SetBgColor(const Value: TAlphaColor);
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

  Canvas.Stroke.Thickness := 0;
  Canvas.Stroke.Color := claNull;
  Canvas.Fill.Color := AFillColor;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.Round);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.Round);
end;

procedure TThRectangle.DrawItemAtMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.Width then
    ATo.X := AFrom.X + IfThen(AFrom.X > ATo.X, -1, 1) * MinimumSize.Width;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Height then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y > ATo.Y, -1, 1) * MinimumSize.Height;

  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
end;
{$ENDREGION}

{$REGION LINE}
{ TThLine }

function TThLine.CreateSelection: IItemSelection;
var
  Selection: TThItemSelection;
begin
  Selection := TThLineSelection.Create(Self);
  Selection.SetResizeSpots([scTopLeft, scBottomRight]);
  Selection.OnTracking := nil;

  Result := Selection;
end;

function TThLine.IsTopLeftToBottomRight: Boolean;
begin
  Result := TItemResizeSpot(FSelection.Spots[0]).SpotCorner in [scTopLeft, scBottomRight];
end;

function TThLine.IsContain(AChild: TThItem): Boolean;
begin
  Result := False;
end;

function TThLine.IsHorizon: Boolean;
begin
  Result := TItemResizeSpot(FSelection.Spots[0]).Position.Y = TItemResizeSpot(FSelection.Spots[1]).Position.Y;
end;

function TThLine.IsVertical: Boolean;
begin
  Result := TItemResizeSpot(FSelection.Spots[0]).Position.X = TItemResizeSpot(FSelection.Spots[1]).Position.X;
end;

function TThLine.GetMinimumSize: TSizeF;
var
  MinSize,
  Rad: Single;
  R: TRectF;
begin
  R := TThItemSelection(FSelection).GetActiveSpotsItemRect;

  MinSize := ItemMinimumSize / AbsoluteScale.X;

  if R.Height = 0 then
    Result := PointF(MinSize, 0)
  else if R.Width = 0 then
    Result := PointF(0, MinSize)
  else
  begin
    Rad := ArcTan(R.Height / R.Width);
    Result.Width := Cos(Rad) * MinSize;
    Result.Height := Sin(Rad) * MinSize;
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

  // ������ ��� 1���Ϸ� ���Ǳ⶧���� ���Ͽ� ���ó���� ����
  Rect := GetItemRect;
  Rect.TopLeft      := ScalePoint(Rect.TopLeft, AbsoluteScale.X, AbsoluteScale.Y);
  Rect.BottomRight  := ScalePoint(Rect.BottomRight, AbsoluteScale.X, AbsoluteScale.Y);
  Pt                := ScalePoint(Pt, AbsoluteScale.X, AbsoluteScale.Y);

  RangeD := (ItemLineSelectionThickness-1) / 2;

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
//        Result := PtInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(RangeD))
        Result := TPoint.PointInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(RangeD))
      else
        Result := TPoint.PointInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD));
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
        Result := TPoint.PointInCircle(Pt.Truncate, Rect.TopLeft.Truncate, Trunc(RangeD))
      else
        Result := TPoint.PointInCircle(Pt.Truncate, Rect.BottomRight.Truncate, Trunc(RangeD));
    end;
  end
  else
  begin
    PtX := Pt.X;
    PtY := IfThen(IsTopLeftToBottomRight, Pt.Y, Rect.Height - Pt.Y);

    // �������� �� ���� Ȯ��
    if not Result then
      Result := TPoint.PointInCircle(PointF(PtX, PtY).Truncate, Rect.TopLeft.Truncate, Trunc(RangeD+1)) or  // �Ҽ��� ����(+1)
                TPoint.PointInCircle(PointF(PtX, PtY).Truncate, Rect.BottomRight.Truncate, Trunc(RangeD+1));

    if not Result then
    begin
      // �������� ������ �簢�� ����Ʈ�� ����(ExtendRect)���
      Rad := ArcTan(Rect.Height / Rect.Width);
      BaseP := PointF(RangeD / Sin(Rad), RangeD / Cos(Rad));

      LeftP   := Rect.TopLeft + PointF(-BaseP.X, BaseP.Y);
      TopP    := Rect.TopLeft + PointF(BaseP.X, -BaseP.Y);
      RightP  := Rect.BottomRight + PointF(BaseP.X, -BaseP.Y);
      BottomP := Rect.BottomRight + PointF(-BaseP.X, BaseP.Y);
      // �밢������ ����
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
//  LineThickness := ItemLineThickness / AbsoluteScale.X;
  LineThickness := ItemLineThickness / CanvasZoomScaleDefault;

  Canvas.Stroke.Thickness := LineThickness;
  Canvas.Stroke.Color := AFillColor;
  Canvas.Stroke.Cap := TStrokeCap.Round;

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

  BaseSpot    := TItemResizeSpot(FSelection.Spots[0]);
  ActiveSpot  := TItemResizeSpot(FSelection.Spots[1]);
  BaseSpot.Position.Point   := AFrom - Position.Point;
  ActiveSpot.Position.Point := ATo - Position.Point;

  Min := MinimumSize;
  if (AFrom.Distance(ATo) < ItemMinimumSize / AbsoluteScale.X) and ((Rect.Width < Min.X) or (Rect.Height < Min.Y)) then
  begin
    if InRange(Rect.Width, 1, Min.X - 1) then
      ATo.X := AFrom.X + Min.X * IfThen(AFrom.X > ATo.X, -1, 1);
    if InRange(Rect.Height, 1, Min.Y - 1) then
      ATo.Y := AFrom.Y + Min.Y * IfThen(AFrom.Y > ATo.Y, -1, 1);

    ActiveSpot.Position.Point := ATo - Position.Point;

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

function TThCircle.CreateSelection: IItemSelection;
var
  Selection: TThItemSelection;
begin
  Selection := TThCircleSelection.Create(Self);
{$IFDEF ON_ALLCORNER_RESIZESPOT}
  Selection.SetResizeSpots([scLeft, scTop, scRight, scBottom, scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
{$ELSE}
  Selection.SetResizeSpots([scLeft, scTop, scRight, scBottom]);
{$ENDIF}
  Selection.OnTracking := SpotTracking;

  Result := Selection;
end;

procedure TThCircle.DrawItemAtMouse(AFrom, ATo: TPointF);
var
  R: TRectF;
begin
  if Abs(AFrom.X - ATo.X) < MinimumSize.Width then
    ATo.X := AFrom.X + IfThen(AFrom.X > ATo.X, -1, 1) * MinimumSize.Width;
  if Abs(AFrom.Y - ATo.Y) < MinimumSize.Height then
    ATo.Y := AFrom.Y + IfThen(AFrom.Y > ATo.Y, -1, 1) * MinimumSize.Height;

  R := RectF(AFrom.X, AFrom.Y, ATo.X, ATo.Y);
  R.NormalizeRect;
  BoundsRect := R;
end;

procedure TThCircle.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  R: TRectF;
  C: TCanvas;
begin
  R := ARect;

  C := Canvas;
  C.Stroke.Thickness := 0;
  C.Stroke.Color := claNull;
  C.Fill.Color := AFillColor;

  C.FillEllipse(R, AbsoluteOpacity);
  C.DrawEllipse(R, AbsoluteOpacity);
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
  RegisterItem(ItemFactoryIDRectangle,  TThRectangle);
  RegisterItem(ItemFactoryIDLine,       TThLine);
  RegisterItem(ItemFactoryIDCircle,     TThCircle);

end.
