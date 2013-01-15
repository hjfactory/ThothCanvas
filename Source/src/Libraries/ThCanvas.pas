unit ThCanvas;

interface

uses
  System.Classes, System.SysUtils,
  System.Types, System.UITypes, System.UIConsts, FMX.Types, FMX.Ani,
  ThTypes, ThItem, ThZoomAnimation, ThAlertAnimation;

type
  TThContents = class(TControl)
  private
    FTrackingPos: TPointF;
    FZoomScale: Single;

    procedure SetZoomScale(const Value: Single);
    function GetScaledPoint: TPointF;
  protected
    procedure Paint; override;
    function GetClipRect: TRectF; override;
    function GetUpdateRect: TRectF; override;

    procedure DoAddObject(AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);
    property ZoomScale: Single read FZoomScale write SetZoomScale;
    property ScaledPoint: TPointF read GetScaledPoint;
  end;

  TThCanvas = class(TControl, IThCanvas, IThZoomObject, IThItemContainer)
  private
    FUseMouseTracking: Boolean;
    FBgColor: TAlphaColor;

    // 마우스가 마지막으로 이동한 거리
    FLastDelta: TPointF;

    FVertTrackAni,
    FHorzTrackAni: TFloatAnimation;
    FZoomAni: TZoomAni;
    FTrackAnimated: Boolean;
    FZoomAnimated: Boolean;

    FAlert: TAlertAnimation;
    FOnZoom: TNotifyEvent;

    function GetViewPortPosition: TPosition;
    procedure SetBgColor(const Value: TAlphaColor);
    function GetZoomScale: Single;
    function GetViewPortSize: TSizeF;

    procedure AlertMessage(msg: string);
    function GetCenterPoint: TPointF;

    function GetItem(Index: Integer): IThItem;
    function GetItemCount: Integer;
  protected
    FContents: TThContents;
    FMouseDownPos,          // MouseDown 시 좌표
    FMouseCurrPos: TPointF; // MouseMove 시 좌표

    procedure Show; override;

    procedure Paint; override;
    procedure DoAddObject(AObject: TFmxObject); override;

    procedure DoZoom(AScale: Single; ATargetPos: TPointF);
    procedure DoZoomHome;
    procedure DoZoomIn(ATargetPos: TPointF); virtual;
    procedure DoZoomOut(ATargetPos: TPointF); virtual;

    procedure DoGrouping(AItem: IThItem); virtual;
//    procedure DoDegrouping(AItem: TThItem); virtual;

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

    property Items[Index: Integer] : IThItem read GetItem;
    property ItemCount: Integer read GetItemCount;
    function GetContainChildrenItems(AItem: IThItem; AChildren: IThItems): Boolean;
    function GetContainParentItem(AItem: IThItem): IThItem;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
    property TrackAnimated: Boolean read FTrackAnimated write FTrackAnimated;
    property ZoomAnimated: Boolean read FZoomAnimated write FZoomAnimated;
    property CenterPoint: TPointF read GetCenterPoint;

    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
   end;

implementation

uses
  System.Generics.Collections,
  ThConsts, FMX.Forms, ThResourceString;

{ TThContent }

procedure TThContents.AddTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
end;

constructor TThContents.Create(AOwner: TComponent);
begin
  inherited;

  FZoomScale := 1;
end;

procedure TThContents.DoAddObject(AObject: TFmxObject);
begin
  inherited;

  // 아이템 추가 시 화면에 기존아이템이 없을 경우 다시 그리지 않아
  //  아이템이 표시되지 않음. 그래서 다시 그리도록 처리
//  RecalcUpdateRect;
  FRecalcUpdateRect := True;
end;

function TThContents.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
end;

function TThContents.GetScaledPoint: TPointF;
begin
  Result.X := Position.X / ZoomScale;
  Result.Y := Position.Y / ZoomScale;
end;

function TThContents.GetUpdateRect: TRectF;
begin
  if not Assigned(Parent) then
    Exit;

{   ClipClildren := True 설정 시 Canvas 영역을 빠져나가면 Contents 표시 멈춤
      TControl.GetUpdateRect 11 line
          if TControl(P).ClipChildren or TControl(P).SmallSizeControl then
            IntersectRect(FUpdateRect, FUpdateRect, TControl(P).UpdateRect);}

  TControl(Parent).ClipChildren := False;
  try
    Result := inherited GetUpdateRect;
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
    Result.AnimationType := TAnimationType.atOut;
    Result.Interpolation := TInterpolationType.itQuadratic;
    Result.PropertyName := APropertyName;
    Result.StartFromCurrent := True;
    Result.Delay := 0;
    Result.Duration := CanvasTrackDuration;
  end;
begin
  inherited;

  ClipChildren := True; // 컨트롤이 영역밖에 표시되지 않도록 처리
  AutoCapture := True;  // 영역밖으로 나가도 컨트롤 되도록 처리

  FUseMouseTracking := True;
  FTrackAnimated    := True;
  FZoomAnimated     := True;

  FContents := TThContents.Create(Self);
  FContents.Parent := Self;
  FContents.HitTest := False;
  FContents.Stored := False;
  FContents.Locked := True;

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

procedure TThCanvas.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents)
      and (not (AObject is TAnimation)) and (not (AObject is TZoomAni)) and (not (AObject is TAlertAnimation)) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThCanvas.DoGrouping(AItem: IThItem);
var
  I: Integer;
  Item: TThItem;
  _Item, NewParent: TThItem;
  Child: IThItem;
  Children: IThItems;
begin
  Item := TThItem(AItem);

  // (Resize 시)아이템 풀어주기
  for I := 0 to Item.ItemCount -1 do
  begin
    _Item := TThItem(Item.Items[I]);
    if not Item.IsContain(_Item) then
      _Item.ReleaseContain;
  end;

  // AItem에 다른 아이템 넣어주기(Search children)
  Children := TList<IThItem>.Create;
  try
    if GetContainChildrenItems(AItem, Children) then
    begin
      for Child in Children do
        Item.Contain(TThItem(Child));
    end;
  finally
    Children.Free;
  end;

  // AItem의 부모찾기(Change parent)
  NewParent := TThItem(GetContainParentItem(Item));

  if Item.Parent is TThItem then
    Item.ReleaseContain;

  if Assigned(NewParent) then
  begin
    if NewParent = Item.Parent then
      Exit;

    // 부모의 자식 중에서 자식 찾기

    NewParent.Contain(Item)
  end;
//  else
end;
//
//procedure TThCanvas.DoDegrouping(AItem: TThItem);
//begin
//
//end;

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
  P := BoundsRect.CenterPoint;
  P.X := P.X;
  P.Y := P.Y;

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

  if FPressed and FUseMouseTracking then
  begin
    FMouseDownPos := PointF(X / ZoomScale, Y / ZoomScale);
    FMouseCurrPos := PointF(X, Y);

    if FVertTrackAni.Running then
      FVertTrackAni.StopAtCurrent;
    if FHorzTrackAni.Running then
      FHorzTrackAni.StopAtCurrent;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if FPressed and FUseMouseTracking then
  begin
    FLastDelta := PointF(X - FMouseCurrPos.X, Y - FMouseCurrPos.Y);
    FMouseCurrPos := PointF(X, Y);

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
  Canvas.StrokeThickness := 1;
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

function TThCanvas.GetContainChildrenItems(AItem: IThItem;
  AChildren: IThItems): Boolean;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    if AItem = Items[I] then
      Continue;

    if TThItem(AItem).IsContain(Items[I]) then
      AChildren.Add(Items[I])
    else
      TThItem(Items[I]).GetContainChildrenItems(AItem, AChildren);
    ;
  end;

  Result := AChildren.Count > 0;
end;

function TThCanvas.GetContainParentItem(AItem: IThItem): IThItem;
var
  I: Integer;
  Item: TThItem;
begin
  for I := ItemCount - 1 downto 0 do
  begin
    Item := TThItem(Items[I]);
    if Item = TThItem(AItem) then
      Continue;

    Result := Item.GetContainParentItem(AItem);
    if Assigned(Result) then
      Exit;
//    if Item.IsContain(AItem) then
//    begin
//      NewParent := Item;
//      Break;
//    end;
  end;
end;

function TThCanvas.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

function TThCanvas.GetItem(Index: Integer): IThItem;
begin
  Result := nil;
  if FContents.ChildrenCount > Index then
    Result := TThItem(FContents.Children[Index]);
end;

end.
