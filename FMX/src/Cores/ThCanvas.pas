unit ThCanvas;

interface

uses
  DebugUtils,
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Ani,
  ThTypes, ThItem, ThZoomAnimation, ThAlertAnimation;

type
  // {TODO} IThItemContainer�� ��ӹ޴°� ���� �� ���� �ʿ�
  //  Viewer������ ����ұ�?
  TThContents = class(TControl, IThItemContainer)
  private
    FTrackingPos: TPointF;
    FZoomScale: Single;

    procedure SetZoomScale(const Value: Single);
    function GetScaledPoint: TPointF;
    function GetItem(Index: Integer): TThItem;
    function GetItemCount: Integer;
  protected
    procedure Paint; override;
    function GetClipRect: TRectF; override;
    function DoGetUpdateRect: TRectF; override;

    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);

    function FindParent(AChild: TThItem): TFMXObject;
    procedure ContainChildren(AContainer: TThItem);

    property ZoomScale: Single read FZoomScale write SetZoomScale;
    property ScaledPoint: TPointF read GetScaledPoint;

    property Items[Index: Integer]: TThItem read GetItem;
    property ItemCount: Integer read GetItemCount;
  end;

  TThCanvas = class(TControl, IThCanvas, IThZoomObject)
  private
    FUseMouseTracking: Boolean;
    FBgColor: TAlphaColor;

    // ���콺�� ���������� �̵��� �Ÿ�
    FLastDelta: TPointF;

    FVertTrackAni,
    FHorzTrackAni: TFloatAnimation;
    FZoomAni: TZoomAni;
    FTrackAnimated: Boolean;
    FZoomAnimated: Boolean;

    FAlert: TAlertAnimation;
    FOnZoom: TNotifyEvent;

    function GetViewPortPosition: TPosition;
    function GetItemCount: Integer;
    procedure SetBgColor(const Value: TAlphaColor);
    function GetZoomScale: Single;
    function GetViewPortSize: TSizeF;

    procedure AlertMessage(msg: string);
    function GetCenterPoint: TPointF;
  protected
    FContents: TThContents;
    FMouseDownPos,          // MouseDown �� ��ǥ
    FMouseCurrPos: TPointF; // MouseMove �� ��ǥ

    procedure Show; override;

    procedure Paint; override;
    procedure DoAddObject(const AObject: TFmxObject); override;

    procedure DoZoom(AScale: Single; ATargetPos: TPointF);
    procedure DoZoomHome;
    procedure DoZoomIn(ATargetPos: TPointF); virtual;
    procedure DoZoomOut(ATargetPos: TPointF); virtual;

    procedure ClickCanvas; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;

    // Interface method
    function IsDrawingItem: Boolean; virtual;
    function IsMultiSelected: Boolean; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomHome;

    procedure ZoomInAtPoint(X, Y: Single);
    procedure ZoomOutAtPoint(X, Y: Single);

    property ZoomScale: Single read GetZoomScale;

    property ViewPortPosition: TPosition read GetViewPortPosition;
    property ViewPortSize: TSizeF read GetViewPortSize;
    property ItemCount: Integer read GetItemCount;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
    property TrackAnimated: Boolean read FTrackAnimated write FTrackAnimated;
    property ZoomAnimated: Boolean read FZoomAnimated write FZoomAnimated;
    property CenterPoint: TPointF read GetCenterPoint;

    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
   end;

implementation

uses
  ThConsts, FMX.Forms, FMX.Graphics, ThResourceString;

{ TThContent }

procedure TThContents.AddTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
//  Debug('AddTrackingPos: %f, %f', [Position.X, Position.Y]);
end;

procedure TThContents.ContainChildren(AContainer: TThItem);
var
  I: Integer;
  CurrItem: TThItem;
begin
  AContainer.LastContainItems.Clear;

  for I := 0 to ItemCount - 1 do
  begin
    CurrItem := Items[I];
    if not Assigned(CurrItem) or (AContainer = CurrItem) then
      Continue;

    if AContainer.IsContain(CurrItem) then
      // ���⼭ Parent�� �ٲٸ� Items / ItemCount�� �پ��
//      CurrItem.Parent := AParent;
      AContainer.LastContainItems.Add(CurrItem);
  end;
  for CurrItem in AContainer.LastContainItems do
  begin
    CurrItem.Parent := AContainer;
    AContainer.ContainChildren(CurrItem);
  end;
end;

constructor TThContents.Create(AOwner: TComponent);
begin
  inherited;

  FZoomScale := 1;
end;

procedure TThContents.DoAddObject(const AObject: TFmxObject);
var
  Item: TThItem;
begin
  if AObject is TThItem then
  begin
    Item := TThItem(AObject);
    if Assigned(Item.BeforeParent) and (Item.BeforeParent is TThItem) then
      Item.Position.Point := TThItem(Item.BeforeParent).AbsolutePoint + Item.Position.Point;
  end;

  inherited;

  // ������ �߰� �� ȭ�鿡 ������������ ���� ��� �ٽ� �׸��� �ʾ�
  //  �������� ǥ�õ��� ����. �׷��� �ٽ� �׸����� ó��
//  RecalcUpdateRect;
  FRecalcUpdateRect := True;
end;

procedure TThContents.DoRemoveObject(const AObject: TFmxObject);
var
  Item: TThItem;
begin
  if not (csDestroying in ComponentState) and (AObject is TThItem) then
  begin
    Item := TThItem(AObject);
    Item.BeforeParent := Self;
    Item.BeforeIndex := AObject.Index;
  end;

  // �ʱ�ȭ ���� ����
  inherited;
end;

function TThContents.FindParent(AChild: TThItem): TFMXObject;
var
  I: Integer;
  CurrItem: TThItem;
begin
  Result := nil;
  for I := ItemCount - 1 downto 0 do
  begin
    CurrItem := Items[I];
    if not Assigned(CurrItem) or (CurrItem = AChild) then
      Continue;

    if CurrItem.IsContain(AChild) then
    begin
      Result := CurrItem.FindParent(AChild);
      if not Assigned(Result) then
      begin
        Result := CurrItem;
        Exit;
      end;
    end;
  end;
  if not Assigned(Result) then
    Result := Self;
end;

function TThContents.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
end;

function TThContents.GetItem(Index: Integer): TThItem;
begin
  Result := nil;
  if (Children.Count > Index) and not (Children[Index].ClassType = TThItem) then
    Result := TThItem(Children[Index]);
end;

function TThContents.GetItemCount: Integer;
begin
  Result := Children.Count;
end;

function TThContents.GetScaledPoint: TPointF;
begin
  Result.X := Position.X / ZoomScale;
  Result.Y := Position.Y / ZoomScale;
end;

function TThContents.DoGetUpdateRect: TRectF;
begin
  if not Assigned(Parent) then
    Exit;

{   ClipClildren := True ���� �� Canvas ������ ���������� Contents ǥ�� ����
      TControl.GetUpdateRect 11 line
          if TControl(P).ClipChildren or TControl(P).SmallSizeControl then
            IntersectRect(FUpdateRect, FUpdateRect, TControl(P).UpdateRect);}

  TControl(Parent).ClipChildren := False;
  try
    Result := inherited DoGetUpdateRect;
  finally
    TControl(Parent).ClipChildren := True;
  end;
end;

procedure TThContents.Paint;
{$IFDEF DEBUG}
var
  R: TRectF;
{$ENDIF}
begin
  inherited;
{$IFDEF DEBUG}
  R := R.Empty;
  InflateRect(R, 100, 100);

  Canvas.Stroke.Color := $FFFF0000;
  Canvas.DrawEllipse(R, 1);
{$ENDIF}
end;

procedure TThContents.SetZoomScale(const Value: Single);
begin
  if FZoomScale = Value then
    Exit;

  FZoomScale := Value;
  Scale.X := Value;
  Scale.Y := value;
end;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
  function _CreateTrackAni(APropertyName: string): TFloatAnimation;
  begin
    Result := TFloatAnimation.Create(Self);
    Result.Parent := Self;
    Result.AnimationType := TAnimationType.Out;
    Result.Interpolation := TInterpolationType.Quadratic;
    Result.PropertyName := APropertyName;
    Result.StartFromCurrent := True;
    Result.Delay := 0;
    Result.Duration := CanvasTrackDuration;
  end;
begin
  inherited;

  ClipChildren := True; // ��Ʈ���� �����ۿ� ǥ�õ��� �ʵ��� ó��
  AutoCapture := True;  // ���������� ������ ��Ʈ�� �ǵ��� ó��

  FUseMouseTracking := True;
  FTrackAnimated    := True;
  FZoomAnimated     := True;

  FContents := TThContents.Create(Self);
  FContents.Parent := Self;
  FContents.HitTest := False;
  FContents.Stored := False;
  FContents.Locked := True;
{$IFDEF DEBUG}
  FContents.Name := FContents.ClassName;
{$ENDIF}

//  DoZoomHome;

  FVertTrackAni := _CreateTrackAni('ViewPortPosition.Y');
  FHorzTrackAni := _CreateTrackAni('ViewPortPosition.X');
  FZoomAni := TZoomCircleAni.Create(Self);
  FZoomAni.Parent := Self;

  FAlert := TAlertAnimation.Create(Self);
  FAlert.Parent := Self;

{$IFDEF DEBUG}
  FBgColor := $FFDDFFDD;
{$ELSE}
  FBgColor := $FFFFFFFF;
{$ENDIF}
end;

destructor TThCanvas.Destroy;
begin
  FAlert.Free;
  FZoomAni.Free;
  FContents.Free;

  inherited;
end;

procedure TThCanvas.DoAddObject(const AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents)
      and (not (AObject is TAnimation)) and (not (AObject is TZoomAni)) and (not (AObject is TAlertAnimation)) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThCanvas.DoZoom(AScale: Single; ATargetPos: TPointF);
var
  P: TPointF;
begin
  P := FContents.Position.Point;
  FContents.Position.X := P.X * AScale + (Width * (1 - AScale)) * (ATargetPos.X / Width);
  FContents.Position.Y := P.Y * AScale + (Height * (1 - AScale)) * (ATargetPos.Y / Height);

  FContents.ZoomScale := FContents.ZoomScale * AScale;

  if Assigned(FOnZoom) then
    FOnZoom(Self);
end;

procedure TThCanvas.DoZoomHome;
var
  P: TPointF;
begin
//  P := BoundsRect.CenterPoint;
  P.X := (BoundsRect.Right - BoundsRect.Left)/2;
  P.Y := (BoundsRect.Bottom - BoundsRect.Top)/2;

  FContents.Position.Point := P;//PointF(0, 0);

  FContents.ZoomScale := CanvasZoomScaleDefault;

  if Assigned(FOnZoom) then
    FOnZoom(Self);
end;

procedure TThCanvas.DoZoomIn(ATargetPos: TPointF);
begin
  if FContents.ZoomScale * CanvasZoomOutRate > CanvasZoomScaleMax then
  begin
    AlertMessage(MsgCanvasZoomInMaxAlert);
    Exit;
  end;

  if FZoomAnimated then
    FZoomAni.ZoomIn(ATargetPos);

  DoZoom(CanvasZoomInRate, ATargetPos);
end;

procedure TThCanvas.DoZoomOut(ATargetPos: TPointF);
begin
  if FContents.ZoomScale * CanvasZoomInRate < CanvasZoomScaleMin then
  begin
    AlertMessage(MsgCanvasZoomOutMinAlert);
    Exit;
  end;

  if FZoomAnimated then
    FZoomAni.ZoomOut(ATargetPos);
  DoZoom(CanvasZoomOutRate, ATargetPos);
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Pressed and FUseMouseTracking then
  begin
    FMouseDownPos := PointF(X / ZoomScale, Y / ZoomScale);
    FMouseCurrPos := PointF(X, Y);

//    Debug('MouseDown: %f, %f', [FMouseDownPos.X, FMouseDownPos.Y]);

    if FVertTrackAni.Running then
      FVertTrackAni.StopAtCurrent;
    if FHorzTrackAni.Running then
      FHorzTrackAni.StopAtCurrent;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
//  Debug('Not pressed');
  if Pressed and FUseMouseTracking then
  begin
    FLastDelta := PointF(X - FMouseCurrPos.X, Y - FMouseCurrPos.Y);
    FMouseCurrPos := PointF(X, Y);

//    Debug('MouseMove: %f, %f', [FMouseCurrPos.X, FMouseCurrPos.Y]);
    FContents.AddTrackingPos(FLastDelta);
  end;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if not (ssShift in Shift) and not (ssCtrl in Shift) and (FMouseDownPos = PointF(X / ZoomScale, Y / ZoomScale)) then
    ClickCanvas
  else if FTrackAnimated and (not IsDrawingItem) then
  begin
    if FLastDelta.Y <> 0 then
    begin
      if FVertTrackAni.Running then
        FVertTrackAni.StopAtCurrent;
      FVertTrackAni.StartValue  := ViewPortPosition.Y;
      FVertTrackAni.StopValue   := ViewPortPosition.Y + FLastDelta.Y * CanvasTrackAniCount;
      FVertTrackAni.Start;
    end;

    if FLastDelta.X <> 0 then
    begin
      if FHorzTrackAni.Running then
        FHorzTrackAni.StopAtCurrent;
      FHorzTrackAni.StartValue  := ViewPortPosition.X;
      FHorzTrackAni.StopValue   := ViewPortPosition.X + FLastDelta.X * CanvasTrackAniCount;
      FHorzTrackAni.Start;
    end;
  end;

end;

procedure TThCanvas.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  P: TPointF;
begin
  inherited;

  P := Screen.MousePos;
  P := ScreenToLocal(P);

  if WheelDelta < 0 then    DoZoomOut(P)
  else                      DoZoomIn(P);
end;

procedure TThCanvas.Paint;
begin
  inherited;

  Canvas.Fill.Color := FBgColor;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);

{$IFDEF DEBUG}
  Canvas.Stroke.Thickness := 1;
  Canvas.Stroke.Color := claBlack;
  Canvas.DrawLine(PointF(Width/2, Top), PointF(Width/2, Height), 1);
  Canvas.DrawLine(PointF(Left, Height/2), PointF(Width, Height/2), 1);
{$ENDIF}
end;

procedure TThCanvas.AlertMessage(msg: string);
begin
  FAlert.Message(msg);
end;

procedure TThCanvas.ClickCanvas;
begin
end;

procedure TThCanvas.SetBgColor(const Value: TAlphaColor);
begin
  if FBgColor = Value then
    Exit;

  FBgColor := Value;
  Repaint;
end;

procedure TThCanvas.Show;
begin
  if Width > 0 then
  begin

  end;

  DoZoomHome;
end;

procedure TThCanvas.ZoomHome;
begin
  DoZoomHome;
end;

procedure TThCanvas.ZoomIn;
begin
  DoZoomIn(ClipRect.CenterPoint)
end;

procedure TThCanvas.ZoomInAtPoint(X, Y: Single);
begin
  DoZoomIn(PointF(X, Y));
end;

procedure TThCanvas.ZoomOut;
begin
  DoZoomOut(ClipRect.CenterPoint);
end;

procedure TThCanvas.ZoomOutAtPoint(X, Y: Single);
begin
  DoZoomOut(PointF(X, Y));
end;

function TThCanvas.GetZoomScale: Single;
begin
  Result := FContents.ZoomScale;
end;

procedure TThCanvas.Initialize;
begin
  DoZoomHome;
end;

function TThCanvas.IsDrawingItem: Boolean;
begin
  Result := False;
end;

function TThCanvas.IsMultiSelected: Boolean;
begin
  Result := False;
end;

function TThCanvas.GetViewPortPosition: TPosition;
begin
  Result := FContents.Position;
end;

function TThCanvas.GetViewPortSize: TSizeF;
begin
  Result.Width  := Width / ZoomScale;
  Result.Height := Height / ZoomScale;
end;

function TThCanvas.GetCenterPoint: TPointF;
begin
  Result := PointF(Width / ZoomScale / 2, Height / ZoomScale / 2)
end;

function TThCanvas.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
