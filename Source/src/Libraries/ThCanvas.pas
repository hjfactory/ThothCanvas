unit ThCanvas;

interface

uses
  System.Classes, System.SysUtils, ThTypes, ThItem, FMX.Layouts,
  System.Types, System.UITypes, System.UIConsts, FMX.Types, FMX.Ani;

type
  TThContents = class(TControl)
  private
    FTrackingPos: TPointF;
    FZoomScale: Single;

    procedure SetZoomScale(const Value: Single);
    function GetZoomScale: Single;
  protected
    function GetClipRect: TRectF; override;
    function GetUpdateRect: TRectF; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);
    property ZoomScale: Single read FZoomScale write SetZoomScale;
  end;

  TThCanvas = class(TControl, IThCanvas)
  private
    FUseMouseTracking: Boolean;
    FBgColor: TAlphaColor;

    FLastDelta: TPointF;
    FVertTrackAni,
    FHorzTrackAni: TFloatAnimation;

    function GetViewPortPosition: TPosition;
    function GetItemCount: Integer;
    procedure SetBgColor(const Value: TAlphaColor);
    function GetZoomScale: Single;
  protected
    FContents: TThContents;
    FMouseDownPos,          // MouseDown 시 좌표
    FMouseCurrPos: TPointF; // MouseMove 시 좌표

    procedure Paint; override;
    procedure DoAddObject(AObject: TFmxObject); override;

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

    procedure ZoomIn;
    procedure ZoomOut;
    property ZoomScale: Single read GetZoomScale;

    property ViewPortPosition: TPosition read GetViewPortPosition;
    property ItemCount: Integer read GetItemCount;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
   end;

implementation

uses
  ThConsts;

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

function TThContents.GetZoomScale: Single;
begin

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
begin
  inherited;

  ClipChildren := True; // 컨트롤이 영역밖에 표시되지 않도록 처리
  AutoCapture := True;  // 영역밖으로 나가도 컨트롤 되도록 처리

  FUseMouseTracking := True;

  FContents := TThContents.Create(Self);
  FContents.Parent := Self;
  FContents.HitTest := False;
  FContents.Stored := False;
  FContents.Locked := True;

  FVertTrackAni := TFloatAnimation.Create(Self);
  FVertTrackAni.Parent := Self;
  FVertTrackAni.AnimationType := TAnimationType.atOut;
  FVertTrackAni.Interpolation := TInterpolationType.itQuadratic;
  FVertTrackAni.PropertyName := 'ViewPortPosition.Y';
  FVertTrackAni.StartFromCurrent := True;
  FVertTrackAni.Delay := 0;
  FVertTrackAni.Duration := CanvasTrackDuration;

  FHorzTrackAni := TFloatAnimation.Create(Self);
  FHorzTrackAni.Parent := Self;
  FHorzTrackAni.AnimationType := TAnimationType.atOut;
  FHorzTrackAni.Interpolation := TInterpolationType.itQuadratic;
  FHorzTrackAni.PropertyName := 'ViewPortPosition.X';
  FHorzTrackAni.StartFromCurrent := True;
  FHorzTrackAni.Delay := 0;
  FHorzTrackAni.Duration := CanvasTrackDuration;

{$IFDEF DEBUG}
  FBgColor := $FFDDFFDD;
{$ELSE}
  FBgColor := $FFFFFFFF;
{$ENDIF}
end;

destructor TThCanvas.Destroy;
begin
  FContents.Free;
//  FVertTrackAni.Free;
//  FHorzTrackAni.Free;

  inherited;
end;

procedure TThCanvas.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents) and (not (AObject is TAnimation)) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed and FUseMouseTracking then
  begin
    FLastDelta := PointF(0, 0);
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

  if FMouseDownPos = PointF(X, Y) then
    ClickCanvas
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
  FContents.ZoomScale := FContents.ZoomScale * 1.1;
end;

procedure TThCanvas.ZoomOut;
begin
  FContents.ZoomScale := FContents.ZoomScale * 0.9;
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

function TThCanvas.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
