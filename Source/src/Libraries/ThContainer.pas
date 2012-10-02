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
    procedure Paint; override;

    function GetClipRect: TRectF; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTrackingPos(const Value: TPointF);
  end;

  TThContainer = class(TControl)
  private
    FContentPos: TPosition;
    FDownPos,
    FCurrentPos: TPointF;
    FUseMouseTracking: Boolean;

    procedure SetContentPos(const Value: TPosition);
    function GetContentPos: TPosition;
  protected
    FContent: TThContent;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ContentPos: TPosition read GetContentPos write SetContentPos;
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

end;

function TThContent.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);

//  Result.Right := Result.Right / Scale.X;
//  Result.Bottom := Result.Bottom / Scale.Y;
end;

procedure TThContent.Paint;
begin
  inherited;

  Canvas.Fill.Color := claWhite;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
end;

{ TThContainer }

constructor TThContainer.Create(AOwner: TComponent);
begin
  inherited;

//  ClipChildren := True;

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

procedure TThContainer.Paint;
begin
  inherited;

  Canvas.Fill.Color := claGreen;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
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

function TThContainer.GetContentPos: TPosition;
begin
  Result := FContent.Position;
end;

procedure TThContainer.SetContentPos(const Value: TPosition);
begin
  FContentPos.Assign(Value);
end;

end.
