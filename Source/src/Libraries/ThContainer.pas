unit ThContainer;

interface

uses
  System.Classes, System.SysUtils,
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

  TThContainer = class(TControl)
  private
    FContentPos: TPosition;
    FUseMouseTracking: Boolean;

    function GetContentPos: TPosition;
    function GetItemCount: Integer;
  protected
    FDownPos,
    FCurrentPos: TPointF;
    FContents: TThContents;

    procedure Paint; override;
    procedure PaintChildren; override;

    procedure DoAddObject(AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ContentPos: TPosition read GetContentPos;
    property ItemCount: Integer read GetItemCount;
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

//Result := inherited GetUpdateRect;
//Exit;
{
   ClipClildren := True 설정 시 Canvas 영역을 빠져나가면 Contents 표시 멈춤
      TControl.GetUpdateRect 11 line
          if TControl(P).ClipChildren or TControl(P).SmallSizeControl then
            IntersectRect(FUpdateRect, FUpdateRect, TControl(P).UpdateRect);
}

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

{ TThContainer }

constructor TThContainer.Create(AOwner: TComponent);
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
end;

destructor TThContainer.Destroy;
begin
  FContents.Free;

  inherited;
end;

procedure TThContainer.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (FPressed) and FUseMouseTracking then
  begin
    FDownPos := PointF(X, Y);
    FCurrentPos := PointF(X, Y);
  end;
end;

procedure TThContainer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  TrackingPos: TPointF;
begin
  if FPressed and FUseMouseTracking then
  begin
    TrackingPos := PointF(X - FCurrentPos.X, Y - FCurrentPos.Y);
    FCurrentPos := PointF(X, Y);

    FContents.AddTrackingPos(TrackingPos);
  end;
end;

procedure TThContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThContainer.Paint;
begin
  inherited;

{$IFDEF DEBUG}
  Canvas.Fill.Color := $FFDDDDDD;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
{$ELSE}
  Canvas.Fill.Color := $FFDDDDDD;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
{$ENDIF}
end;

procedure TThContainer.PaintChildren;
begin
  inherited;

end;
{
procedure TThContainer.DoRealign;
var
  R: TRectF;
begin
  inherited;
Exit;
  R := GetContentBounds;
  OffsetRect(R, FContents.Position.X, FContents.Position.Y);
  OffsetRect(R, FContents.Position.X, FContents.Position.Y);

  RealignContent(R);
end;

procedure TThContainer.RealignContent(R: TRectF);
begin
  if (FContents <> nil) then
  begin
    FContents.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContents.FRecalcUpdateRect := True; // need to recalc
    FContents.Realign;
  end;
end;

function TThContainer.GetContentBounds: TRectF;
var
  I: Integer;
  R, LocalR: TRectF;
  C: TControl;
begin
  Result := RectF(0, 0, Width, Height);
  if (FContents <> nil) then
  begin
    R := ClipRect;
    for I := 0 to FContents.ChildrenCount - 1 do
      if (TControl(FContents.Children[I]).Visible) then
      begin
        if (csDesigning in ComponentState) and not (csDesigning in FContents.Children[I].ComponentState) then
          Continue;

        C := TControl(FContents.Children[I]);
        LocalR := RectF(0, 0, C.Width * C.Scale.X, C.Height * C.Scale.Y);
        R := UnionRect(R, LocalR);
      end;
    Result := R;
  end;
end;
}
function TThContainer.GetContentPos: TPosition;
begin
  Result := FContents.Position;
end;

function TThContainer.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
