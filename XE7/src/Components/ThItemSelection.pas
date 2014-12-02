unit ThItemSelection;

interface

uses
  System.Classes, System.Types, FMX.Types, System.UITypes, FMX.Controls,
  System.Generics.Collections, System.Math.Vectors, FMX.Graphics, ThConsts, ThTypes;

type
  TSpotTrackingEvent = procedure(SpotCorner: TSpotCorner; X, Y: Single; SwapHorz, SwapVert: Boolean) of object;
  TItemResizeSpot = class(TControl, IItemResizeSpot)
  private
    FMouseDownPos: TPointF;
    FDownItemRect: TRectF;
    FSpotCorner: TSpotCorner;
    FOnTracking: TTrackingEvent;
    FParentItem: IThItem;
    FDisabled: Boolean;
    procedure SetSpotCorner(const Value: TSpotCorner);
    procedure SetParentItem(const Value: IThItem);
    procedure SetDisabled(const Value: Boolean);
  protected
    function GetAbsoluteMatrix: TMatrix; override;

    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent; ASpotCorner: TSpotCorner); reintroduce; virtual;
    function PointInObject(X, Y: Single): Boolean; override;

    property SpotCorner: TSpotCorner read FSpotCorner write SetSpotCorner;
    property OnTracking: TTrackingEvent read FOnTracking write FOnTracking;

    property ParentItem: IThItem read FParentItem write SetParentItem;
    property Disabled: Boolean read FDisabled write SetDisabled;

    class function InflateSize: Integer;
  end;

  TThItemCircleResizeSpot = class(TItemResizeSpot)
  protected
    procedure Paint; override;
    function DoGetUpdateRect: TRectF; override;
  public
    constructor Create(AOwner: TComponent; ADirection: TSpotCorner); override;
  end;

  TThResizeSpots = TList<TItemResizeSpot>;
  TResizeSpotClass = class of TItemResizeSpot;
  DefaultResizeSpotClass = TThItemCircleResizeSpot;

  TThItemSelection = class(TInterfacedObject, IItemSelection)
  private
    FParent: IItemSelectionObject;
    FSpotClass: TResizeSpotClass;
    FParentControl: TControl;
    FSpots: TThResizeSpots;
    FOnTracking: TSpotTrackingEvent;
    function GetSpots(Index: Integer): IItemResizeSpot;
    function GetCount: Integer;
    function GetSpot(SpotCorner: TSpotCorner): TItemResizeSpot;

    procedure DoResizeSpotTrack(Sender: TObject; X, Y: Single);
    function GetIsMouseOver: Boolean;
  protected
    function GetSelectionRect: TRectF; virtual;

    procedure ResizeItemBySpot(ASpot: IItemResizeSpot); virtual;    // Spot 이동 시 Item 크기 조정
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); virtual;  // Spot의 SpotCorner 조정

    procedure RealignSpot; virtual;
  public
    constructor Create(AParent: IItemSelectionObject);
    destructor Destroy; override;

    procedure SetResizeSpotClass(ASpotClass: TResizeSpotClass);
    procedure SetResizeSpots(Spots: array of TSpotCorner);

    function GetActiveSpotsItemRect(ASpot: IItemResizeSpot = nil): TRectF; virtual;

    property IsMouseOver: Boolean read GetIsMouseOver;

    procedure ShowSpots;
    procedure ShowDisableSpots;
    procedure HideSpots;

    procedure DrawSelection; virtual;

    property Spots[Index: Integer] : IItemResizeSpot read GetSpots; default;
    property Count: Integer read GetCount;
    property OnTracking: TSpotTrackingEvent read FOnTracking write FOnTracking;
  end;

  // 수직/수평선(Width, Height = 0) 예외처리
  TThLineSelection = class(TThItemSelection)
  protected
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;

    procedure RealignSpot; override;    // Width, Height가 1인 경우 발생
  public
    procedure DrawSelection; override;
  end;

  // 상하/좌우 크기만 변경되도록 예외
  TThCircleSelection = class(TThItemSelection)
  protected
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;
  public
    function GetActiveSpotsItemRect(ASpot: IItemResizeSpot = nil): TRectF; override;
  end;

implementation

uses
  System.UIConsts, SpotCornerUtils, System.Math, DebugUtils;

{ TItemResizeSpot }

constructor TItemResizeSpot.Create(AOwner: TComponent;
  ASpotCorner: TSpotCorner);
begin
  inherited Create(AOwner);

  AutoCapture := True;

  Opacity := 1;
  SpotCorner := ASpotCorner;
end;

procedure TItemResizeSpot.DoMouseEnter;
begin
  inherited;

  TControl(Parent).Repaint;
  Repaint;
end;

procedure TItemResizeSpot.DoMouseLeave;
begin
  inherited;

  TControl(Parent).Repaint;
  Repaint;
end;

function TItemResizeSpot.GetAbsoluteMatrix: TMatrix;
begin
  Result := inherited GetAbsoluteMatrix;

  if not Assigned(Scale) then
    Exit;

  Result.m11 := Scale.X;
  Result.m22 := Scale.Y;
end;

class function TItemResizeSpot.InflateSize: Integer;
begin
  Result := ItemResizeSpotRadius;
end;

procedure TItemResizeSpot.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if FDisabled then
    Exit;

  inherited;

  if Pressed then
  begin
    FMouseDownPos := PointF(X, Y);                  // Spot내의 마우스 위치
    FDownItemRect := TControl(Parent).BoundsRect;   // Item의 범위
    FDownItemRect.Offset(TControl(Parent).Position.Point);
  end;
end;

procedure TItemResizeSpot.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Gap: TPointF;
begin
  if FDisabled then
    Exit;
  inherited;

  if Pressed then
  begin
    Gap := PointF(X, Y) - FMouseDownPos;  // Down and Move Gap
    Position.Point := Position.Point + Gap;

    if Assigned(FOnTracking) then
      FOnTracking(Self, Gap.X, Gap.Y);
  end;
end;

procedure TItemResizeSpot.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if FDisabled then
    Exit;

  inherited;

  if (FDownItemRect <> TControl(Parent).BoundsRect) then
    FParentItem.ItemResizeBySpot(Self, FDownItemRect);
end;

function TItemResizeSpot.PointInObject(X, Y: Single): Boolean;
  function _GetAbsoluteScale: Single;
  begin
    Result := TControl(Parent).AbsoluteScale.X;
  end;

var
  P: TPointF;
  Raduis: Single;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  Raduis := ItemResizeSpotRadius / _GetAbsoluteScale;
  if (Abs(P.X) < Raduis) and (Abs(P.Y) < Raduis) then
    Result := True;
end;

procedure TItemResizeSpot.SetDisabled(const Value: Boolean);
begin
  FDisabled := Value;

  if FDisabled then
    Cursor := crDefault
  else
    SetSpotCorner(FSpotCorner);
end;

procedure TItemResizeSpot.SetParentItem(const Value: IThItem);
begin
  FParentItem := Value;
//  Parent := TFMXObject(Value);
end;

procedure TItemResizeSpot.SetSpotCorner(const Value: TSpotCorner);
begin
  FSpotCorner := Value;

  case FSpotCorner of
    scTopLeft, scBottomRight: Cursor := crSizeNWSE;
    scTopRight, scBottomLeft: Cursor := crSizeNESW;
    scTop, scBottom: Cursor := crSizeNS;
    scLeft, scRight: Cursor := crSizeWE;
  end;
end;

{ TItemSelection }

constructor TThItemSelection.Create(AParent: IItemSelectionObject);
begin
  FParent := AParent;
  FParentControl := TControl(AParent);
  FSpots := TThResizeSpots.Create;

  FSpotClass := DefaultResizeSpotClass;
end;

destructor TThItemSelection.Destroy;
var
  I: Integer;
begin
  for I := FSpots.Count - 1 downto 0 do
    FSpots[I].Free;

  inherited;
end;

procedure TThItemSelection.SetResizeSpots(Spots: array of TSpotCorner);
var
  I: Integer;
  Spot: TItemResizeSpot;
begin
  for I := FSpots.Count - 1 downto 0 do
    FSpots[I].Free;
  FSpots.Clear;
  Assert(Assigned(FSpotClass), 'Not assigned items Spot class');

  for I := 0 to Length(Spots) - 1 do
  begin
    Spot := FSpotClass.Create(TControl(FParent), Spots[I]);
    Spot.ParentItem := FParent;
    Spot.Parent := TControl(FParent);
    Spot.OnTracking := DoResizeSpotTrack;
    Spot.Visible := False;
    FSpots.Add(Spot);
  end;
end;

procedure TThItemSelection.SetResizeSpotClass(ASpotClass: TResizeSpotClass);
begin
  FSpotClass := ASpotClass;
end;

function TThItemSelection.GetCount: Integer;
begin
  Result := FSpots.Count;
end;

function TThItemSelection.GetIsMouseOver: Boolean;
var
  Spot: TItemResizeSpot;
begin
  Result := False;
  for Spot in FSpots do
  begin
    if Spot.IsMouseOver then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TThItemSelection.GetSelectionRect: TRectF;
begin
  Result := TControl(FParent).ClipRect;
  InflateRect(Result, FSpotClass.InflateSize + 1, FSpotClass.InflateSize + 1);
end;

function TThItemSelection.GetSpot(
  SpotCorner: TSpotCorner): TItemResizeSpot;
var
  I: Integer;
begin
  for I := 0 to FSpots.Count - 1 do
  begin
    Result := FSpots[I];
    if Result.SpotCorner = SpotCorner then
      Exit;
  end;

  Result := nil;
end;

function TThItemSelection.GetSpots(Index: Integer): IItemResizeSpot;
begin
  if FSpots.Count > Index then
    Result := IItemResizeSpot(TItemResizeSpot(FSpots[Index]));
end;

procedure TThItemSelection.ShowSpots;
var
  I: Integer;
begin
  for I := 0 to FSpots.Count - 1 do
  begin
    FSpots[I].Disabled := False;
    FSpots[I].Visible := True;
  end;

  RealignSpot;
end;

procedure TThItemSelection.ShowDisableSpots;
var
  I: Integer;
begin
  for I := 0 to FSpots.Count - 1 do
  begin
    FSpots[I].Disabled := True;
    FSpots[I].Visible := True;
  end;

  RealignSpot;
end;

procedure TThItemSelection.HideSpots;
var
  I: Integer;
begin
  for I := 0 to FSpots.Count - 1 do
    TControl(FSpots[I]).Visible := False;
end;

procedure TThItemSelection.DoResizeSpotTrack(Sender: TObject; X, Y: Single);
var
  ActiveSpot: TThItemCircleResizeSpot absolute Sender;
  SpotCorner: TSpotCorner;
  BeforeSize: TSizeF;
  SwapHorz, SwapVert: Boolean;
begin
  SpotCorner := ActiveSpot.SpotCorner;

  BeforeSize := TControl(FParent).BoundsRect.Size;

  ResizeItemBySpot(ActiveSpot);
  NormalizeSpotCorner(ActiveSpot);
  RealignSpot;

  if Assigned(FOnTracking) then
  begin
  //////////////////////////////////////////////
  ///  최소크기 및 반전(RightSpot -> LeftSpot)에 대한 예외사항
    SwapHorz := False;
    SwapVert := False;
    if ChangeHorizonSpotCorner(SpotCorner, ActiveSpot.SpotCorner) then
    begin
      if ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) then
        X := -TControl(FParent).Width
      else
        X := BeforeSize.Width;

      SwapHorz := True;
    end
    else if TControl(FParent).Width <= FParent.MinimumSize.Width then
    begin
      if BeforeSize.Width <= FParent.MinimumSize.Width then
        X := 0
      else if ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) then
        X := BeforeSize.Width - FParent.MinimumSize.Width;
    end;

    if ChangeVerticalSpotCorner(SpotCorner, ActiveSpot.SpotCorner) then
    begin
      if ContainSpotCorner(ActiveSpot.SpotCorner, scTop) then
        Y := -TControl(FParent).Height
      else
        Y := BeforeSize.Height;
      SwapVert := True;
    end
    else if TControl(FParent).Height <= FParent.MinimumSize.Height then
    begin
      if BeforeSize.Height <= FParent.MinimumSize.Height then
        Y := 0
      else if ContainSpotCorner(ActiveSpot.SpotCorner, scTop) then
        Y := BeforeSize.Height - FParent.MinimumSize.Height;
    end;

    FOnTracking(SpotCorner, X, Y, SwapHorz, SwapVert);
  end;
end;

procedure TThItemSelection.DrawSelection;
var
  State: TCanvasSaveState;
begin
  with TControl(FParent) do
  begin
    State := Canvas.SaveState;
    try
      Canvas.Stroke.Color := ItemSelectionColor;
      Canvas.Stroke.Thickness := ItemSelectionSize/AbsoluteScale.X;
      Canvas.DrawRect(ClipRect, 0, 0, AllCorners, 1);
    finally
      Canvas.RestoreState(State);
    end;
  end;
end;

procedure TThItemSelection.ResizeItemBySpot(ASpot: IItemResizeSpot);
var
  MinSize: TPointF;
  ItemR: TRectF;
  ActiveSpot: TThItemCircleResizeSpot;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);

  ItemR := GetActiveSpotsItemRect(ASpot);

  MinSize := FParent.MinimumSize;
  if ItemR.Width < MinSize.X then
  begin
    if (ItemR.Width = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scRight) then
      // 우측에서 좌로 올라올때 아래로 최소크기 적요되는 버그 개선
      ItemR.Left := ItemR.Right - MinSize.X
    else if ItemR.Right = ActiveSpot.Position.X then
    {(ItemR.Width = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) 포함}
      ItemR.Right := ItemR.Left + MinSize.X
    else
      ItemR.Left := ItemR.Right - MinSize.X;
  end;

  if ItemR.Height < MinSize.Y then
  begin
    if (ItemR.Height = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) then
      // 아래에서 위로 올라올때 아래로 최소크기 적요되는 버그 개선
      ItemR.Top := ItemR.Bottom - MinSize.Y
    else if ItemR.Bottom = ActiveSpot.Position.Y then
    {(ItemR.Height = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) 포함}
      ItemR.Bottom := ItemR.Top + MinSize.Y
    else
      ItemR.Top := ItemR.Bottom - MinSize.Y;
  end;

  if ItemR.Width < 1 then    ItemR.Width := 1;
  if ItemR.Height < 1 then   ItemR.Height := 1;

  ItemR.Offset(FParentControl.Position.Point);
  FParentControl.BoundsRect := ItemR;
end;

procedure TThItemSelection.NormalizeSpotCorner(ASpot: IItemResizeSpot);
var
  I: Integer;
  ActivateSpotRect: TRectF;
  SpotPos: TPointF;
  AnotherSpot,
  ActiveSpot: TItemResizeSpot;
  ActiveSpotCorner,
  HSpotCorner, VSpotCorner: TSpotCorner;
begin
  ActivateSpotRect := GetActiveSpotsItemRect(ASpot);

  ActiveSpot := TItemResizeSpot(ASpot);
  SpotPos := ActiveSpot.Position.Point;

  HSpotCorner := scUnknown;
  if ActivateSpotRect.Width = 0 then    // Rect - ActiveSpot의 HorizonSpotCorner를 전환
    HSpotCorner := HorizonSpotCornerExchange(AndSpotCorner(ActiveSpot.SpotCorner, HORIZON_CORNERS))
  else if ActivateSpotRect.Left = SpotPos.X then
    HSpotCorner := scLeft
  else if ActivateSpotRect.Right = SpotPos.X then
    HSpotCorner := scRight;

  VSpotCorner := scUnknown;
  if ActivateSpotRect.Height = 0 then   // Rect - ActiveSpot의 VerticalSpotCorner를 전환
    VSpotCorner := VerticalSpotCornerExchange(AndSpotCorner(ActiveSpot.SpotCorner, VERTICAL_CORNERS))
  else if ActivateSpotRect.Top = SpotPos.Y then
    VSpotCorner := scTop
  else if ActivateSpotRect.Bottom = SpotPos.Y then
    VSpotCorner := scBottom;

  ActiveSpotCorner := scUnknown;
  ActiveSpotCorner := SetHorizonSpotCorner(ActiveSpotCorner, HSpotCorner);
  ActiveSpotCorner := SetVerticalSpotCorner(ActiveSpotCorner, VSpotCorner);

  for I := 0 to Count - 1 do
  begin
    AnotherSpot := TItemResizeSpot(Spots[I]);

    if AnotherSpot = ActiveSpot then
      Continue;

    // Switch horizon spot
    if ChangeHorizonSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := HorizonSpotCornerExchange(AnotherSpot.SpotCorner);

    // Switch vertical spot
    if ChangeVerticalSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := VerticalSpotCornerExchange(AnotherSpot.SpotCorner);
  end;

  ActiveSpot.SpotCorner := ActiveSpotCorner;
end;

procedure TThItemSelection.RealignSpot;
var
  I: Integer;
  SpotP: TPointF;
  ItemR: TRectF;
  Spot: TItemResizeSpot;
begin
  ItemR := FParent.GetItemRect;

  for I := 0 to Count - 1 do
  begin
    Spot := TItemResizeSpot(Spots[I]);

    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ItemR.Left, ItemR.Top);
      scTop:          SpotP := PointF(RectWidth(ItemR)/2, ItemR.Top);
      scTopRight:     SpotP := PointF(ItemR.Right, ItemR.Top);
      scLeft:         SpotP := PointF(ItemR.Left, RectHeight(ItemR)/2);
      scRight:        SpotP := PointF(ItemR.Right, RectHeight(ItemR)/2);
      scBottomLeft:   SpotP := PointF(ItemR.Left, ItemR.Bottom);
      scBottom:       SpotP := PointF(RectWidth(ItemR)/2, ItemR.Bottom);
      scBottomRight:  SpotP := PointF(ItemR.Right, ItemR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  FParentControl.Repaint;
end;

function TThItemSelection.GetActiveSpotsItemRect(ASpot: IItemResizeSpot): TRectF;
var
  ActiveSpot,
  OppositeSpot: TItemResizeSpot;
begin
  if not Assigned(ASpot) then
    ASpot := Spots[0];
  ActiveSpot := TItemResizeSpot(ASpot);
  OppositeSpot := GetSpot(SpotCornerExchange(ActiveSpot.SpotCorner));
  if not Assigned(OppositeSpot) then
    Exit;

  Result.TopLeft := ActiveSpot.Position.Point;
  Result.BottomRight := OppositeSpot.Position.Point;
  Result.NormalizeRect;
end;

{ TThItemCircleResizeSpot }

constructor TThItemCircleResizeSpot.Create(AOwner: TComponent; ADirection: TSpotCorner);
begin
  inherited;

  Width := ItemResizeSpotRadius * 2;
  Height := ItemResizeSpotRadius * 2;
end;

function TThItemCircleResizeSpot.DoGetUpdateRect: TRectF;
begin
  Result := inherited DoGetUpdateRect;
  if Assigned(Canvas) then
  begin
    InflateRect(Result,
      ItemResizeSpotRadius + Canvas.StrokeThickness,
      ItemResizeSpotRadius + Canvas.StrokeThickness);
  end;
end;

procedure TThItemCircleResizeSpot.Paint;
var
  R: TRectF;
begin
  inherited;

  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := $FF222222;

  if FDisabled then
    Canvas.Fill.Color := ItemResizeSpotDisableColor
  else if IsMouseOver then
  begin
//    if SpotCorner = scBottomLeft then
//      Canvas.Fill.Color := claGreen
//    else if SpotCorner = scBottomRight then
//      Canvas.Fill.Color := claBlue
//    else
    Canvas.Fill.Color := ItemResizeSpotOverColor
  end
  else
    Canvas.Fill.Color := ItemResizeSpotOutColor;

  R := R.Empty;
  InflateRect(R, ItemResizeSpotRadius, ItemResizeSpotRadius);
  Canvas.FillEllipse(R, 1);
  Canvas.DrawEllipse(R, 1);
end;

{ TThLineSelection }

procedure TThLineSelection.DrawSelection;
begin
end;

procedure TThLineSelection.NormalizeSpotCorner(ASpot: IItemResizeSpot);
var
  I: Integer;
  ActivateSpotRect: TRectF;
  SpotPos: TPointF;
  AnotherSpot,
  ActiveSpot: TItemResizeSpot;
  ActiveSpotCorner,
  HSpotCorner, VSpotCorner: TSpotCorner;
begin
  ActivateSpotRect := GetActiveSpotsItemRect(ASpot);

  ActiveSpot := TItemResizeSpot(ASpot);
  SpotPos := ActiveSpot.Position.Point;

  if ActivateSpotRect.Width = 0 then        // Line
    ActiveSpotCorner := IfThenSpotCorner(ActivateSpotRect.Top = SpotPos.Y, scTop, scBottom)
  else if ActivateSpotRect.Height = 0 then  // Line
    ActiveSpotCorner := IfThenSpotCorner(ActivateSpotRect.Left = SpotPos.X, scLeft, scRight)
  else
  begin
    HSpotCorner := scUnknown;
    if ActivateSpotRect.Left = SpotPos.X then
      HSpotCorner := scLeft
    else if ActivateSpotRect.Right = SpotPos.X then
      HSpotCorner := scRight;

    VSpotCorner := scUnknown;
    if ActivateSpotRect.Top = SpotPos.Y then
      VSpotCorner := scTop
    else if ActivateSpotRect.Bottom = SpotPos.Y then
      VSpotCorner := scBottom;

    ActiveSpotCorner := scUnknown;
    ActiveSpotCorner := SetHorizonSpotCorner(ActiveSpotCorner, HSpotCorner);
    ActiveSpotCorner := SetVerticalSpotCorner(ActiveSpotCorner, VSpotCorner);
  end;

  for I := 0 to Count - 1 do
  begin
    AnotherSpot := TItemResizeSpot(Spots[I]);

    if AnotherSpot = ActiveSpot then
      Continue;

    // [Line] Top(Bottom)으로 변경한경우 TopLeft, BottomRight에서는 Bottom(Top)으로 처리
    if not SupportedHorizonSpotCorner(ActiveSpotCorner) and SupportedHorizonSpotCorner(AnotherSpot.SpotCorner) then
    begin
      AnotherSpot.SpotCorner := VerticalSpotCornerExchange(ActiveSpotCorner);
      Continue;
    end;

    if not SupportedVerticalSpotCorner(ActiveSpotCorner) and SupportedVerticalSpotCorner(AnotherSpot.SpotCorner) then
    begin
      AnotherSpot.SpotCorner := HorizonSpotCornerExchange(ActiveSpotCorner);
      Continue;
    end;

    if SupportedHorizonSpotCorner(ActiveSpotCorner) and not SupportedHorizonSpotCorner(AnotherSpot.SpotCorner) or
        SupportedVerticalSpotCorner(ActiveSpotCorner) and not SupportedVerticalSpotCorner(AnotherSpot.SpotCorner) then
    begin
      AnotherSpot.SpotCorner := SpotCornerExchange(ActiveSpotCorner);
      Continue;
    end;

    // Switch horizon spot
    if ChangeHorizonSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := HorizonSpotCornerExchange(AnotherSpot.SpotCorner);

    // Switch vertical spot
    if ChangeVerticalSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := VerticalSpotCornerExchange(AnotherSpot.SpotCorner);
  end;

  ActiveSpot.SpotCorner := ActiveSpotCorner;
end;

procedure TThLineSelection.RealignSpot;
var
  I: Integer;
  SpotP: TPointF;
  ItemR: TRectF;
  Spot: TItemResizeSpot;
begin
  ItemR := FParent.GetItemRect;

  for I := 0 to Count - 1 do
  begin
    Spot := TItemResizeSpot(Spots[I]);

    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ItemR.Left, ItemR.Top);
      scTop:          SpotP := PointF(0, ItemR.Top);
      scTopRight:     SpotP := PointF(ItemR.Right, ItemR.Top);
      scLeft:         SpotP := PointF(ItemR.Left, 0);
      scRight:        SpotP := PointF(ItemR.Right, 0);
      scBottomLeft:   SpotP := PointF(ItemR.Left, ItemR.Bottom);
      scBottom:       SpotP := PointF(0, ItemR.Bottom);
      scBottomRight:  SpotP := PointF(ItemR.Right, ItemR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  FParentControl.Repaint;
end;

{ TThCircleSelection }

procedure TThCircleSelection.NormalizeSpotCorner(ASpot: IItemResizeSpot);
var
  I: Integer;
  ActivateSpotRect: TRectF;
  SpotPos: TPointF;
  AnotherSpot,
  ActiveSpot: TItemResizeSpot;
  ActiveSpotCorner: TSpotCorner;
begin
  ActivateSpotRect := GetActiveSpotsItemRect(ASpot);

  ActiveSpot := TItemResizeSpot(ASpot);
  SpotPos := ActiveSpot.Position.Point;

  ActiveSpotCorner := scUnknown;
  // 가로축, 세로축으로만 변경
  if SupportedHorizonSpotCorner(ActiveSpot.SpotCorner) then
  begin
    if ActivateSpotRect.Width = 0 then    // Rect - ActiveSpot의 HorizonSpotCorner를 전환
      ActiveSpotCorner := HorizonSpotCornerExchange(AndSpotCorner(ActiveSpot.SpotCorner, HORIZON_CORNERS))
    else if ActivateSpotRect.Left = SpotPos.X then
      ActiveSpotCorner := scLeft
    else if ActivateSpotRect.Right = SpotPos.X then
      ActiveSpotCorner := scRight;
  end
  else if SupportedVerticalSpotCorner(ActiveSpot.SpotCorner) then
  begin
    if ActivateSpotRect.Height = 0 then   // Rect - ActiveSpot의 VerticalSpotCorner를 전환
      ActiveSpotCorner := VerticalSpotCornerExchange(AndSpotCorner(ActiveSpot.SpotCorner, VERTICAL_CORNERS))
    else if ActivateSpotRect.Top = SpotPos.Y then
      ActiveSpotCorner := scTop
    else if ActivateSpotRect.Bottom = SpotPos.Y then
      ActiveSpotCorner := scBottom;
  end;

  for I := 0 to Count - 1 do
  begin
    AnotherSpot := TItemResizeSpot(Spots[I]);

    if AnotherSpot = ActiveSpot then
      Continue;

    // Switch horizon spot
    if ChangeHorizonSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := HorizonSpotCornerExchange(AnotherSpot.SpotCorner);

    // Switch vertical spot
    if ChangeVerticalSpotCorner(ActiveSpot.SpotCorner, ActiveSpotCorner) then
      AnotherSpot.SpotCorner := VerticalSpotCornerExchange(AnotherSpot.SpotCorner);
  end;

  ActiveSpot.SpotCorner := ActiveSpotCorner;
end;

function TThCircleSelection.GetActiveSpotsItemRect(ASpot: IItemResizeSpot): TRectF;
var
  ActiveSpot,
  OppositeSpot: TItemResizeSpot;
begin
  if not Assigned(ASpot) then
    ASpot := Spots[0];
  ActiveSpot := TItemResizeSpot(ASpot);
  OppositeSpot := GetSpot(SpotCornerExchange(ActiveSpot.SpotCorner));
  if not Assigned(OppositeSpot) then
    Exit;

  Result := FParent.GetItemRect;
  case ActiveSpot.SpotCorner of
    scTop, scBottom:
      begin
        Result.Top := ActiveSpot.Position.Y;
        Result.Bottom := OppositeSpot.Position.Y;
      end;
    scLeft,
    scRight:
      begin
        Result.Left := ActiveSpot.Position.X;
        Result.Right := OppositeSpot.Position.X;
      end;
  end;
  Result.NormalizeRect;
end;

end.
