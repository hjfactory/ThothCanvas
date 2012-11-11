unit ThCanvas;

interface

uses
  System.Classes, System.SysUtils,
  System.Types, System.UITypes, System.UIConsts, FMX.Types, FMX.Ani,
  ThTypes, ThItem, ThZoomAnimation;

type
  TThContents = class(TControl)
  private
    FTrackingPos: TPointF;
    FZoomScale: Single;

    procedure SetZoomScale(const Value: Single);
  protected
    function GetClipRect: TRectF; override;
    function GetUpdateRect: TRectF; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);
    property ZoomScale: Single read FZoomScale write SetZoomScale;
  end;

  TThCanvas = class(TControl, IThCanvas, IThZoomObject)
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

    function GetViewPortPosition: TPosition;
    function GetItemCount: Integer;
    procedure SetBgColor(const Value: TAlphaColor);
    function GetZoomScale: Single;
    function GetViewPortSize: TSizeF;
  protected
    FContents: TThContents;
    FMouseDownPos,          // MouseDown 시 좌표
    FMouseCurrPos: TPointF; // MouseMove 시 좌표

    procedure Paint; override;
    procedure DoAddObject(AObject: TFmxObject); override;

    procedure DoZoom(AScale: Single; ATargetPos: TPointF);
    procedure DoZoomIn(ATargetPos: TPointF); virtual;
    procedure DoZoomOut(ATargetPos: TPointF); virtual;

    procedure ClickCanvas; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Interface method
    function IsDrawingItem: Boolean; virtual;
    function IsMultiSelected: Boolean; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    procedure ZoomIn;
    procedure ZoomOut;

    procedure ZoomInAtPoint(APos: TPointF);
    procedure ZoomOutAtPoint(APos: TPointF);

    property ZoomScale: Single read GetZoomScale;

    property ViewPortPosition: TPosition read GetViewPortPosition;
    property ViewPortSize: TSizeF read GetViewPortSize;
    property ItemCount: Integer read GetItemCount;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
    property TrackAnimated: Boolean read FTrackAnimated write FTrackAnimated;
    property ZoomAnimated: Boolean read FZoomAnimated write FZoomAnimated;
   end;

implementation

uses
  ThConsts, FMX.Forms, DebugUtils;

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

function TThContents.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
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
begin
  inherited;

{$IFDEF DEBUG}
  Canvas.Fill.Color := claNull;
  Canvas.Stroke.Color := claBlack;

  Canvas.DrawRect(TControl(Parent).ClipRect, 0, 0, AllCorners, 1);
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

  FVertTrackAni := _CreateTrackAni('ViewPortPosition.Y');
  FHorzTrackAni := _CreateTrackAni('ViewPortPosition.X');
  FZoomAni := TZoomCircleAni.Create(Self);
  FZoomAni.Parent := Self;

{$IFDEF DEBUG}
  FBgColor := $FFDDFFDD;
{$ELSE}
  FBgColor := $FFFFFFFF;
{$ENDIF}
end;

destructor TThCanvas.Destroy;
begin
  FZoomAni.Free;
  FContents.Free;
//  FVertTrackAni.Free;
//  FHorzTrackAni.Free;


  inherited;
end;

procedure TThCanvas.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents)
      and (not (AObject is TAnimation)) and (not (AObject is TZoomAni)) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThCanvas.DoZoom(AScale: Single; ATargetPos: TPointF);
var
  ScaledSize: TSizeF;
  DifferenceSize: TSizeF;  // different of scaled size to not scaled size
  MoveDistancePoint: TPointF;
  ScaledPoint: TPointF;
var
  P: TPointF;
begin
//
//  // 증가된 크기
  ScaledSize.Width := Width / AScale;
  ScaledSize.Height := Height / AScale;

  // 증가된 크기와 원래크기 차이
  DifferenceSize := ScaledSize.Subtract(PointF(Width, Height));

  // 증가된 크기 중 마우스의 비율(=이동할 거리)
  MoveDistancePoint.X := DifferenceSize.Width * (ATargetPos.X / Width) * AScale;
  MoveDistancePoint.Y := DifferenceSize.Height * (ATargetPos.Y / Height) * AScale;

//  FContents.Position.Point.Offset(MoveDistancePoint);
//
////  ScaledPoint.X := FContents.Position.X * FContents.ZoomScale;
////  ScaledPoint.Y := FContents.Position.Y * FContents.ZoomScale;
////  FContents.Position.Point := ScaledPoint.Add(MoveDistancePoint);
//  FContents.Position.Point := FContents.Position.Point.Add(MoveDistancePoint);
//  FContents.Position.Point := MoveDistancePoint;

  MoveDistancePoint.X := (ScaledSize.Width - ViewPortSize.Width) * (ATargetPos.X / Width) * AScale * ZoomScale;
  MoveDistancePoint.Y := (ScaledSize.Height - ViewPortSize.Height) * (ATargetPos.Y / Height) * AScale * ZoomScale;

//  MoveDistancePoint.X := (ScaledSize.Width * AScale - ViewPortSize.Width * ZoomScale) * (ATargetPos.X / Width);
//  MoveDistancePoint.Y := (ScaledSize.Height - ViewPortSize.Height) * (ATargetPos.Y / Height) * AScale;

  Debug('B: %f, A: %f(%f, %f, %f)',
    [
      FContents.Position.Point.X,
      FContents.Position.Point.Add(MoveDistancePoint).X,
      AScale,
      ScaledSize.Width,
      ViewPortSize.Width
    ]);


  FContents.Position.Point := FContents.Position.Point.Add(MoveDistancePoint);

  FContents.ZoomScale := AScale;
end;

procedure TThCanvas.DoZoomIn(ATargetPos: TPointF);
begin
  if FZoomAnimated then
    FZoomAni.ZoomIn(ATargetPos);
  DoZoom(FContents.ZoomScale * 1.1, ATargetPos);
//  FContents.ZoomScale := FContents.ZoomScale * 1.1;
end;

procedure TThCanvas.DoZoomOut(ATargetPos: TPointF);
var
  ZoomScale: Single;
begin
  if FZoomAnimated then
    FZoomAni.ZoomOut(ATargetPos);
  ZoomScale := FContents.ZoomScale * 0.9;
  DoZoom(ZoomScale, ATargetPos);
//  FContents.ZoomScale := FContents.ZoomScale * 0.9;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed and FUseMouseTracking then
  begin
//    FLastDelta := PointF(0, 0);
    FMouseDownPos := PointF(X, Y);
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

  if FTrackAnimated then
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

  if FMouseDownPos = PointF(X, Y) then
    ClickCanvas
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

procedure TThCanvas.ZoomIn;
begin
  DoZoomIn(ClipRect.CenterPoint)
end;

procedure TThCanvas.ZoomInAtPoint(APos: TPointF);
begin
  DoZoomIn(APos);
end;

procedure TThCanvas.ZoomOut;
begin
  DoZoomOut(ClipRect.CenterPoint);
end;

procedure TThCanvas.ZoomOutAtPoint(APos: TPointF);
begin
  DoZoomOut(APos);
end;

function TThCanvas.GetZoomScale: Single;
begin
  Result := FContents.ZoomScale;
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

function TThCanvas.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
