unit ThContainer;

interface

uses
  System.Classes,
  System.Types, System.UITypes, System.UIConsts, FMX.Types;

type
  TThContent = class(TControl)
  private
    FTrackingPos: TPointF;
  protected
    function GetClipRect: TRectF; override;
//    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);
  end;

  TThContainer = class(TControl)
  private
    FContentPos: TPosition;
    FUseMouseTracking: Boolean;

    function GetContentPos: TPosition;
    function GetContentBounds: TRectF; virtual;
    procedure RealignContent(R: TRectF); virtual;
  protected
    FDownPos,
    FCurrentPos: TPointF;
    FContent: TThContent;

    procedure Paint; override;

    procedure DoAddObject(AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ContentPos: TPosition read GetContentPos;
    procedure DoRealign; override;
  end;

implementation

{ TThContent }

procedure TThContent.AddTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
end;

constructor TThContent.Create(AOwner: TComponent);
begin
  inherited;

//  ClipChildren := False;
end;

function TThContent.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);

//  Result.Right := Result.Right / Scale.X;
//  Result.Bottom := Result.Bottom / Scale.Y;
end;

//procedure TThContent.PaintChildren;
//var
//  I, j: Integer;
//  R: TRectF;
//  AllowPaint: Boolean;
//  Control: TControl;
//begin
//  ClipChildren := True;
//  inherited PaintChildren;
//  ClipChildren := False;
//end;

{ TThContainer }

constructor TThContainer.Create(AOwner: TComponent);
begin
  inherited;

  ClipChildren := True; // 컨트롤이 영역밖에 표시되지 않도록 처리
  AutoCapture := True;  // 영역밖으로 나가도 컨트롤 되도록 처리

  FUseMouseTracking := True;

  FContent := TThContent.Create(Self);
  FContent.Parent := Self;
  FContent.HitTest := False;
  FContent.Stored := False;
  FContent.Locked := True;
end;

destructor TThContainer.Destroy;
begin
  FContent.Free;

  inherited;
end;

procedure TThContainer.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContent) and (AObject <> FContent) then
    FContent.AddObject(AObject)
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

    FContent.AddTrackingPos(TrackingPos);

    FCurrentPos := PointF(X, Y);
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

  Canvas.Fill.Color := claGray;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
end;

procedure TThContainer.DoRealign;
  procedure IntAlign;
  var
    R: TRectF;
  begin
    R := GetContentBounds;
    OffsetRect(R, FContent.Position.X, FContent.Position.Y);

    RealignContent(R);
  end;
begin
//Exit;

  if csDestroying in ComponentState then
    Exit;

  if csLoading in ComponentState then
    Exit;

  if FDisableAlign then
    Exit;
  if FUpdating > 0 then
    Exit;
  IntAlign;
end;

procedure TThContainer.RealignContent(R: TRectF);
begin
  if (FContent <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContent.FRecalcUpdateRect := True; // need to recalc
  end;
end;

function TThContainer.GetContentBounds: TRectF;
var
  I: Integer;
  R, LocalR: TRectF;
  C: TControl;
begin
  Result := RectF(0, 0, Width, Height);
  if (FContent <> nil) then
  begin
    R := ClipRect;
    for I := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[I] is TControl then
        if (TControl(FContent.Children[I]).Visible) then
        begin
          if (csDesigning in ComponentState) and not (csDesigning in FContent.Children[I].ComponentState) then Continue;
          // Scale등의 크기와 관련된 항목 처리
//          LocalR := TControl(FContent.Children[I]).ParentedRect;
          C := TControl(FContent.Children[I]);
          LocalR := RectF(0, 0, C.Width * C.Scale.X, C.Height * C.Scale.Y);
          R := UnionRect(R, LocalR);
        end;
    Result := R;

//    Debug('ContentBound L T :%f, %f', [R.Left, R.Top]);
  end;
end;

function TThContainer.GetContentPos: TPosition;
begin
  Result := FContent.Position;
end;

end.
