unit ThItemResizer;

interface

uses
  System.Classes, System.Types, FMX.Types, System.UITypes,
  System.Generics.Collections, ThConsts, ThTypes;

type
  TItemResizeSpot = class(TControl, IItemResizeSpot)
  private
    FMouseDownPos: TPointF;
    FDownItemRect: TRectF;
    FSpotCorner: TSpotCorner;
    FOnTracking: TTrackingEvent;
    procedure SetSpotCorner(const Value: TSpotCorner);
  protected
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

    class function InflateSize: Integer;
  end;

  TThItemCircleResizeSpot = class(TItemResizeSpot)
  protected
    procedure Paint; override;
    function GetUpdateRect: TRectF; override;
  public
    constructor Create(AOwner: TComponent; ADirection: TSpotCorner); override;
  end;

  TResizeSpotClass = class of TItemResizeSpot;
  TThItemResizer = class(TInterfacedObject, IItemResizer)
  private
    FParent: IItemResizerObject;
    FSpotClass: TResizeSpotClass;
    FParentControl: TControl;
    FList: TList;
    FOnTracking: TTrackingEvent;
    function GetSpots(Index: Integer): IItemResizeSpot;
    function GetCount: Integer;
    function GetSpot(SpotCorner: TSpotCorner): TItemResizeSpot;

    procedure DoResizeSpotTrack(Sender: TObject; X, Y: Single);
  protected
    function GetResizerRect: TRectF; virtual;

    procedure ResizeShapeBySpot(ASpot: IItemResizeSpot); virtual;    // Spot 이동 시 Shape 크기 조정
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); virtual;  // Spot의 SpotCorner 조정

    procedure RealignSpot; virtual;
  public
    constructor Create(AParent: IItemResizerObject);
    destructor Destroy; override;

    procedure SetSpotClass(ASpotClass: TResizeSpotClass);
    procedure SetResizeSpots(Spots: array of TSpotCorner);

    function GetActiveSpotsItemRect(ASpot: IItemResizeSpot = nil): TRectF; virtual;

    procedure ShowSpots;
    procedure HideSpots;

    property Spots[Index: Integer] : IItemResizeSpot read GetSpots; default;
    property Count: Integer read GetCount;
    property OnTracking: TTrackingEvent read FOnTracking write FOnTracking;
  end;

  // 수직/수평선(Width, Height = 0) 예외처리
  TThLineResizer = class(TThItemResizer)
  protected
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;

    procedure RealignSpot; override;    // Width, Height가 1인 경우 발생
  end;

  // 상하/좌우 크기만 변경되도록 예외
  TThCircleResizer = class(TThItemResizer)
  protected
    procedure NormalizeSpotCorner(ASpot: IItemResizeSpot); override;
  public
    function GetActiveSpotsItemRect(ASpot: IItemResizeSpot = nil): TRectF; override;
  end;

implementation

uses
  System.UIConsts, CommonUtils, ResizeUtils, System.Math, ThItem;

{ TItemResizeSpot }

constructor TItemResizeSpot.Create(AOwner: TComponent;
  ASpotCorner: TSpotCorner);
begin
  inherited Create(AOwner);

  Opacity := 1;
  SpotCorner := ASpotCorner;
end;

procedure TItemResizeSpot.DoMouseEnter;
begin
  inherited;

  Repaint;
end;

procedure TItemResizeSpot.DoMouseLeave;
begin
  inherited;

  Repaint;
end;

class function TItemResizeSpot.InflateSize: Integer;
begin
  Result := ItemResizeSpotRadius;
end;

procedure TItemResizeSpot.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed then
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
  inherited;

  if FPressed then
  begin
    Gap := PointF(X, Y).Subtract(FMouseDownPos);  // Down and Move Gap
    Position.Point := Position.Point.Add(Gap);

    if Assigned(FOnTracking) then
      FOnTracking(Self, Gap.X, Gap.Y);
  end;
end;

procedure TItemResizeSpot.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (FDownItemRect <> TControl(Parent).BoundsRect) then
  begin
    Debug('Resize item');
    TThItem(FParent).DoItemResize(Self, FDownItemRect);
  end;
end;

function TItemResizeSpot.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if (Abs(P.X) < ItemResizeSpotRadius) and (Abs(P.Y) < ItemResizeSpotRadius) then
    Result := True;
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

{ TItemResizer }

constructor TThItemResizer.Create(AParent: IItemResizerObject);
begin
  FParent := AParent;
  FParentControl := TControl(AParent);
  FList := TList.Create;
end;

destructor TThItemResizer.Destroy;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TControl(FList[I]).Free;

  inherited;
end;

procedure TThItemResizer.SetResizeSpots(Spots: array of TSpotCorner);
var
  I: Integer;
  Spot: TItemResizeSpot;
begin
  for I := FList.Count - 1 downto 0 do
    TControl(FList[I]).Free;
  FList.Clear;
  Assert(Assigned(FSpotClass), 'Not assigned items Spot class');

  for I := 0 to Length(Spots) - 1 do
  begin
    Spot := FSpotClass.Create(TControl(FParent), Spots[I]);
    Spot.Parent := TControl(FParent);
    Spot.OnTracking := DoResizeSpotTrack;
    Spot.Visible := False;
    FList.Add(Spot);
  end;
end;

procedure TThItemResizer.SetSpotClass(ASpotClass: TResizeSpotClass);
begin
  FSpotClass := ASpotClass;
end;

function TThItemResizer.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TThItemResizer.GetResizerRect: TRectF;
begin
  Result := TControl(FParent).ClipRect;
  InflateRect(Result, FSpotClass.InflateSize + 1, FSpotClass.InflateSize + 1);
end;

function TThItemResizer.GetSpot(
  SpotCorner: TSpotCorner): TItemResizeSpot;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList[I];
    if Result.SpotCorner = SpotCorner then
      Exit;
  end;

  Result := nil;
end;

function TThItemResizer.GetSpots(Index: Integer): IItemResizeSpot;
begin
  if FList.Count > Index then
    Result := IItemResizeSpot(TItemResizeSpot(FList[Index]));
end;

procedure TThItemResizer.ShowSpots;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TControl(FList[I]).Visible := True;
  RealignSpot;
end;

procedure TThItemResizer.HideSpots;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TControl(FList[I]).Visible := False;
end;

procedure TThItemResizer.DoResizeSpotTrack(Sender: TObject; X, Y: Single);
var
  ActiveSpot: TThItemCircleResizeSpot absolute Sender;
begin
  ResizeShapeBySpot(ActiveSpot);
  NormalizeSpotCorner(ActiveSpot);
  RealignSpot;

  if Assigned(FOnTracking) then
    FOnTracking(Sender, X, Y);
end;

procedure TThItemResizer.ResizeShapeBySpot(ASpot: IItemResizeSpot);
var
  MinSize: TPointF;
  ShapeR: TRectF;
  ActiveSpot: TThItemCircleResizeSpot;
begin
  ActiveSpot := TThItemCircleResizeSpot(ASpot);

  ShapeR := GetActiveSpotsItemRect(ASpot);

  MinSize := FParent.MinimumSize;
  if ShapeR.Width < MinSize.X then
  begin
    if (ShapeR.Width = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scRight) then
      // 우측에서 좌로 올라올때 아래로 최소크기 적요되는 버그 개선
      ShapeR.Left := ShapeR.Right - MinSize.X
    else if ShapeR.Right = ActiveSpot.Position.X then
    {(ShapeR.Width = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scLeft) 포함}
      ShapeR.Right := ShapeR.Left + MinSize.X
    else
      ShapeR.Left := ShapeR.Right - MinSize.X;
  end;

  if ShapeR.Height < MinSize.Y then
  begin
    if (ShapeR.Height = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) then
      // 아래에서 위로 올라올때 아래로 최소크기 적요되는 버그 개선
      ShapeR.Top := ShapeR.Bottom - MinSize.Y
    else if ShapeR.Bottom = ActiveSpot.Position.Y then
    {(ShapeR.Height = 0) and ContainSpotCorner(ActiveSpot.SpotCorner, scBottom) 포함}
      ShapeR.Bottom := ShapeR.Top + MinSize.Y
    else
      ShapeR.Top := ShapeR.Bottom - MinSize.Y;
  end;

  if ShapeR.Width = 0 then  ShapeR.Width := 1;
  if ShapeR.Height = 0 then  ShapeR.Height := 1;

  ShapeR.Offset(FParentControl.Position.Point);
  FParentControl.BoundsRect := ShapeR;
end;

procedure TThItemResizer.NormalizeSpotCorner(ASpot: IItemResizeSpot);
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

procedure TThItemResizer.RealignSpot;
var
  I: Integer;
  SpotP: TPointF;
  ShapeR: TRectF;
  Spot: TItemResizeSpot;
begin
  ShapeR := FParent.GetItemRect;

  for I := 0 to Count - 1 do
  begin
    Spot := TItemResizeSpot(Spots[I]);

    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ShapeR.Left, ShapeR.Top);
      scTop:          SpotP := PointF(RectWidth(ShapeR)/2, ShapeR.Top);
      scTopRight:     SpotP := PointF(ShapeR.Right, ShapeR.Top);
      scLeft:         SpotP := PointF(ShapeR.Left, RectHeight(ShapeR)/2);
      scRight:        SpotP := PointF(ShapeR.Right, RectHeight(ShapeR)/2);
      scBottomLeft:   SpotP := PointF(ShapeR.Left, ShapeR.Bottom);
      scBottom:       SpotP := PointF(RectWidth(ShapeR)/2, ShapeR.Bottom);
      scBottomRight:  SpotP := PointF(ShapeR.Right, ShapeR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  FParentControl.Repaint;
end;

function TThItemResizer.GetActiveSpotsItemRect(ASpot: IItemResizeSpot): TRectF;
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

  AutoCapture := True;

  Width := ItemResizeSpotRadius * 2;
  Height := ItemResizeSpotRadius * 2;
end;

function TThItemCircleResizeSpot.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result,
    ItemResizeSpotRadius + Canvas.StrokeThickness,
    ItemResizeSpotRadius + Canvas.StrokeThickness);
end;

procedure TThItemCircleResizeSpot.Paint;
var
  R: TRectF;
begin
  inherited;

  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := $FF222222;
  if IsMouseOver then
    Canvas.Fill.Color := ItemResizeSpotOverColor
  else
    Canvas.Fill.Color := ItemResizeSpotOutColor;

  R := R.Empty;
  InflateRect(R, ItemResizeSpotRadius, ItemResizeSpotRadius);
  Canvas.FillEllipse(R, 1);
  Canvas.DrawEllipse(R, 1);
end;

{ TThLineResizer }

procedure TThLineResizer.NormalizeSpotCorner(ASpot: IItemResizeSpot);
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

procedure TThLineResizer.RealignSpot;
var
  I: Integer;
  SpotP: TPointF;
  ShapeR: TRectF;
  Spot: TItemResizeSpot;
begin
  ShapeR := FParent.GetItemRect;

  for I := 0 to Count - 1 do
  begin
    Spot := TItemResizeSpot(Spots[I]);

    case Spot.SpotCorner of
      scTopLeft:      SpotP := PointF(ShapeR.Left, ShapeR.Top);
      scTop:          SpotP := PointF(0, ShapeR.Top);
      scTopRight:     SpotP := PointF(ShapeR.Right, ShapeR.Top);
      scLeft:         SpotP := PointF(ShapeR.Left, 0);
      scRight:        SpotP := PointF(ShapeR.Right, 0);
      scBottomLeft:   SpotP := PointF(ShapeR.Left, ShapeR.Bottom);
      scBottom:       SpotP := PointF(0, ShapeR.Bottom);
      scBottomRight:  SpotP := PointF(ShapeR.Right, ShapeR.Bottom);
    end;

    Spot.Position.Point := SpotP;
  end;

  FParentControl.Repaint;
end;

{ TThCircleResizer }

procedure TThCircleResizer.NormalizeSpotCorner(ASpot: IItemResizeSpot);
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

function TThCircleResizer.GetActiveSpotsItemRect(ASpot: IItemResizeSpot): TRectF;
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
