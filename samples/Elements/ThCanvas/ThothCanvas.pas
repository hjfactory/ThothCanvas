unit ThothCanvas;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, FMX.Types;

type
  TThContent = class(TControl)
    // Layout - ClipRect
    // OffSet - Point(Position)
  private
    FLayout: TControl;
    FOffset: TPointF;
    FTrackingPos: TPointF;
    FSize: TPointF;

    procedure SetTrackingPos(const Value: TPointF);
    procedure SetSize(const Value: TPointF);
  protected
    constructor Create(AOwner: TComponent); override;

    function GetClipRect: TRectF; override;
//    function GetUpdateRect: TRectF; override;

    property TrackingPos: TPointF read FTrackingPos write SetTrackingPos;
    property Size: TPointF read FSize write SetSize;

    procedure test;
  end;

  TThCanvas = class(TStyledControl)
  private
    FDown: Boolean;
    FCurrentPos: TPointF;

    FContent: TThContent;
    FMouseTracking: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property MouseTracking: Boolean read FMouseTracking write FMouseTracking default False;

    function test: string;

    procedure Realign; override;
  end;

implementation

{ TThContent }

constructor TThContent.Create(AOwner: TComponent);
begin
  inherited;
end;

function TThContent.GetClipRect: TRectF;
begin

  Result := RectF(0, 0, Size.X, Size.Y);
  OffsetRect(Result, Position.X, Position.Y);

//  if (Parent <> nil) and (Parent is TThContent) and  then

// Update 할 영역만 반환
end;

procedure TThContent.SetSize(const Value: TPointF);
begin
  FSize := Value;
end;

procedure TThContent.SetTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;

//  Self.SetBoundsRect(ClipRect);

//  Self.SetBoundsRect();
end;

procedure TThContent.test;
var
  R: TRectF;
begin
  SetBoundsRect(ClipRect);
end;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FMouseTracking := True;

  FContent := TThContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
  FContent.Size := PointF(Width, Height);
end;

destructor TThCanvas.Destroy;
begin
  FContent.Free;
  FContent := nil;

  inherited;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and FMouseTracking then
  begin
    FCurrentPos := PointF(X, Y);
    FDown := True;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FDown and FMouseTracking then
  begin
    FContent.TrackingPos := PointF(X - FCurrentPos.X, Y - FCurrentPos.Y);

    FCurrentPos := PointF(X, Y);
  end;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FDown and FMouseTracking then
  begin
    FDown := False;
  end;
end;

procedure TThCanvas.Realign;

  procedure IntAlign;
  begin
    FContent.test;
  end;
begin
  if csDestroying in ComponentState then
    Exit;
  inherited;
  if csLoading in ComponentState then
    Exit;

  if FDisableAlign then
    Exit;
  if FUpdating > 0 then
    Exit;
  FDisableAlign := True;
  try
    IntAlign;
  finally
    FDisableAlign := False;
  end;
end;

function TThCanvas.test: string;
begin
  Result := 'X: ' + IntToStr(Round(FContent.Width)) + ', Y: ' + IntToStr(Round(FContent.Height));
  Result := 'X: ' + IntToStr(Round(FContent.Position.X)) + ', Y: ' + IntToStr(Round(FContent.Position.Y));
end;

procedure TThCanvas.AddObject(AObject: TFmxObject);
begin
  if Assigned(FContent) and (AObject <> FContent) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.AddObject(AObject)
  end
  else
    inherited;
end;

end.
