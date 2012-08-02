unit ThothCanvas;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, FMX.Types;

type
  TThContent = class(TControl)
    // Layout - ClipRect
    // OffSet - Point(Position)
  private
    FTrackingPos: TPointF;

    procedure SetTrackingPos(const Value: TPointF);
  protected
    function GetClipRect: TRectF; override;
    procedure PaintChildren; override;

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
    procedure Center;
  end;

implementation

uses
  Unit1;

{ TThContent }

function TThContent.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
end;

procedure TThContent.PaintChildren;
var
  I, j: Integer;
  R: TRectF;
  State: TCanvasSaveState;
  AllowPaint: Boolean;
begin
  if FScene = nil then
    Exit;
  if FChildren <> nil then
  begin
    for I := 0 to FChildren.Count - 1 do
      if (Children[I] is TControl) and
        ((TControl(FChildren[I]).Visible) or (not TControl(FChildren[I]).Visible and (csDesigning in ComponentState) and not TControl(FChildren[I]).Locked)) then
        with TControl(FChildren[I]) do
        begin
          if (csDesigning in ComponentState) then
            Continue;
          if not FInPaintTo and ((RectWidth(UpdateRect) = 0) or (RectHeight(UpdateRect) = 0)) then
            Continue;
          if Self.ClipChildren and not IntersectRect(Self.UpdateRect, UpdateRect) then
            Continue;
          // Check visibility
          AllowPaint := False;
          if (csDesigning in ComponentState) or FInPaintTo then
            AllowPaint := True;
          if not AllowPaint then
          begin
            R := UnionRect(GetChildrenRect, UpdateRect);
            for j := 0 to FScene.GetUpdateRectsCount - 1 do
              if IntersectRect(FScene.GetUpdateRect(j), R) then
              begin
                AllowPaint := True;
                Break;
              end;
          end;
          // Paint
          if AllowPaint then
          begin
            State := nil;
            try
              // HJF - 캔버스 영역 안으로만 그리기 위한 조취
              if {Self.FClipChildren and }CanClip then
              begin
                State := Canvas.SaveState;
                Canvas.SetMatrix(Self.AbsoluteMatrix);
                Canvas.IntersectClipRect(Self.ClipRect);
              end;
              if HasEffect and not HasAfterPaintEffect then
                ApplyEffect;
              Painting;
              DoPaint;
              AfterPaint;
            finally
              if State <> nil then
                Canvas.RestoreState(State);
            end;
            if HasAfterPaintEffect then
              ApplyEffect;
          end;
        end;
  end;
end;

procedure TThContent.SetTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
end;

{ TThCanvas }

procedure TThCanvas.Center;
begin
  FContent.Position.Point := PointF(0, 0);
end;

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
  C: TControl;
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
          // HJF - Scale등의 크기와 관련된 항목 처리
//          LocalR := TControl(FContent.Children[i]).ParentedRect;
          C := TControl(FContent.Children[i]);
          LocalR := RectF(0, 0, C.Width * C.Scale.X, C.Height * C.Scale.Y);
          OffsetRect(LocalR, C.Position.X, C.Position.Y);
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
    OffsetRect(R, FContent.Position.X, FContent.Position.Y);
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
