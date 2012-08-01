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

    procedure SetTrackingPos(const Value: TPointF);
  protected
    constructor Create(AOwner: TComponent); override;

    function GetClipRect: TRectF; override;

    property TrackingPos: TPointF read FTrackingPos write SetTrackingPos;
  end;

  TThCanvas = class(TStyledControl)
  private
    FDown: Boolean;
    FCurrentPos: TPointF;

    FContent: TThContent;
    FMouseTracking: Boolean;

    function GetContentBounds: TRectF; virtual;
    procedure RealignContent(R: TRectF); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property MouseTracking: Boolean read FMouseTracking write FMouseTracking default False;

    procedure Realign; override;
  end;

implementation

uses
  Unit1;

{ TThContent }

constructor TThContent.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := True;
end;

function TThContent.GetClipRect: TRectF;
begin
//  Result := RectF(0, 0, TThCanvas(Parent).Width, TThCanvas(Parent).Height);
  Result :=  TThCanvas(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
//  Debug(Format('L: %f, T: %f, R: %f, B: %f', [Result.Left, result.Top, Result.Right, Result.Bottom]));
end;

procedure TThContent.SetTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
end;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FMouseTracking := True;

  AutoCapture := True;

  FContent := TThContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
end;

destructor TThCanvas.Destroy;
begin
  FContent.Free;
  FContent := nil;

  inherited;
end;

function TThCanvas.GetContentBounds: TRectF;
var
  i: Integer;
  R, LocalR: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
  if (FContent <> nil) then
  begin
    R := ClipRect;
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TControl then
        if (TControl(FContent.Children[i]).Visible) then
        begin
          if (csDesigning in ComponentState) and not (csDesigning in FContent.Children[i].ComponentState) then Continue;
          LocalR := TControl(FContent.Children[i]).ParentedRect;
          R := UnionRect(R, LocalR);
        end;
    Result := R;
  end;
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
  var
    R: TRectF;
  begin
    R := GetContentBounds;
    RealignContent(R);
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

procedure TThCanvas.RealignContent(R: TRectF);
begin
  if (FContent <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContent.FRecalcUpdateRect := True; // need to recalc
  end;
end;

procedure TThCanvas.AddObject(AObject: TFmxObject);
begin
  if Assigned(FContent) and (AObject <> FContent) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

end.
