unit ThCanvas;

interface

uses
  System.Classes, System.SysUtils, ThTypes, ThItem,
  System.Types, System.UITypes, System.UIConsts, FMX.Types;

type
  TThContents = class(TControl)
  private
    FTrackingPos: TPointF;
  protected
    function GetClipRect: TRectF; override;
    function GetUpdateRect: TRectF; override;
    procedure Paint; override;
  public
    procedure AddTrackingPos(const Value: TPointF);
  end;

  TThCanvas = class(TControl, IThCanvas)
  private
    FUseMouseTracking: Boolean;
    FBgColor: TAlphaColor;

    function GetContentPos: TPosition;
    function GetItemCount: Integer;
    procedure SetBgColor(const Value: TAlphaColor);
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

    property ContentPos: TPosition read GetContentPos;
    property ItemCount: Integer read GetItemCount;

    property BgColor: TAlphaColor read FBgColor write SetBgColor;
   end;

implementation

uses
  CommonUtils;

{ TThContent }

procedure TThContents.AddTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
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

{$IFDEF DEBUG}
  FBgColor := $FFDDFFDD;
{$ELSE}
  FBgColor := $FFFFFFFF;
{$ENDIF}
end;

destructor TThCanvas.Destroy;
begin
  FContents.Free;

  inherited;
end;

procedure TThCanvas.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents) then
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
    FMouseDownPos := PointF(X, Y);
    FMouseCurrPos := PointF(X, Y);
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
var
  TrackingPos: TPointF;
begin
  if FPressed and FUseMouseTracking then
  begin
    TrackingPos := PointF(X - FMouseCurrPos.X, Y - FMouseCurrPos.Y);
    FMouseCurrPos := PointF(X, Y);

    FContents.AddTrackingPos(TrackingPos);
  end;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

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

function TThCanvas.IsDrawingItem: Boolean;
begin
  Result := False;
end;

function TThCanvas.IsMultiSelected: Boolean;
begin
  Result := False;
end;

function TThCanvas.GetContentPos: TPosition;
begin
  Result := FContents.Position;
end;

function TThCanvas.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
