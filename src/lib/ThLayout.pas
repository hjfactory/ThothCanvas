unit ThLayout;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, FMX.Types, ThTypes;

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

  TThContainer = class(TStyledControl)
  private
    FContentScale: Single;
    FMouseTracking: Boolean;

    function GetContentBounds: TRectF; virtual;
    procedure RealignContent(R: TRectF); virtual;
    function GetOffsetPos: TPointF;
  protected
    FContent: TThContent;
    FCurrentPos: TPointF;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property MouseTracking: Boolean read FMouseTracking write FMouseTracking default False;
    property OffsetPos: TPointF read GetOffsetPos;

    procedure Realign; override;
    procedure Center;

    function ZoomIn: Single;
    function ZoomOut: Single;
  end;

implementation

{ TThContent }

function TThContent.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);

//  Result.Left := Result.Left / Scale.X;
  Result.Right := Result.Right / Scale.X;
  Result.Bottom := Result.Bottom / Scale.Y;

  Debug('%f %f %f %f', [Result.Left, Result.Top, Result.Right, Result.Bottom]);
//  Debug('UpdateRect %f %f %f %f', [UpdateRect.Left, UpdateRect.Top, UpdateRect.Right, UpdateRect.Bottom]);
end;

procedure TThContent.PaintChildren;
var
  I, j, K: Integer;
  R, ChildR: TRectF;
  State: TCanvasSaveState;
  AllowPaint: Boolean;

  M: TMatrix;
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

                M := AbsoluteMatrix;
                if M.m31 > 0 then
                begin

                end;

//                R := ChildrenRect;
//                if not ClipChildren and (FChildren <> nil) then
//                begin
//                  for K := 0 to FChildren.Count - 1 do
//                  begin
//                    if not(Children[K] is TControl) then
//                      Continue;
//                    if not TControl(FChildren[K]).Visible then
//                      Continue;
//                    R := TControl(FChildren[K]).UpdateRect;
//                    ChildR := UnionRect(ChildR, R);
//                  end;
//                end;
                M := Self.AbsoluteMatrix;
                R := Self.ClipRect;

//                if ChildR.Left < 0 then
//                  M.m31 := TControl(Parent).AbsoluteMatrix.m31;// M.m31 + ChildR.Left;
                  M.m31 := 120;//TControl(Parent).AbsoluteMatrix.m31;// M.m31 + ChildR.Left;
                R.Left := R.Left + 120;
                R.Right := R.Right + 120;
//                M.m31 := M.m31 + (R.Left - Position.X);
//                M.m32 := M.m32 + (R.Top - Position.Y);
//                Canvas.SetMatrix(Self.AbsoluteMatrix);
                Debug('000000 M %f R %f P %f, R.L %f R.R %f', [M.m31, ChildR.Left, Position.X, R.Left, R.Right]);
                Canvas.SetMatrix(M);
                Canvas.IntersectClipRect(Self.ClipRect);

//                R := Self.ClipRect;
//                OffsetRect(R, Position.X, Position.Y);

//                Canvas.IntersectClipRect(Self.ClipRect);
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

{ TThContainer }

procedure TThContainer.Center;
begin
  FContent.Position.Point := PointF(0, 0);
end;

constructor TThContainer.Create(AOwner: TComponent);
begin
  inherited;

  FContentScale := 1;
  FMouseTracking := True;

  AutoCapture := True;

  FContent := TThContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
end;

destructor TThContainer.Destroy;
begin
  FContent.Free;
  FContent := nil;

  inherited;
end;

function TThContainer.GetContentBounds: TRectF;
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
//          LocalR := RectF(0, 0, C.Width, C.Height);
//          OffsetRect(LocalR, C.Position.X, C.Position.Y);
          R := UnionRect(R, LocalR);
        end;
    Result := R;

    Debug('ContentBound L T :%f, %f', [R.Left, R.Top]);
  end;
end;

function TThContainer.GetOffsetPos: TPointF;
begin
  Result := FContent.Position.Point;
end;

procedure TThContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (FPressed) and FMouseTracking then
  begin
    FCurrentPos := PointF(X, Y);
//    FCurrentPos := ScalePoint(PointF(X, Y), FContentScale, FContentScale);
  end;
end;

procedure TThContainer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  TP: TPointF;
begin
  if FPressed and FMouseTracking then
  begin
    TP := PointF(X - FCurrentPos.X, Y - FCurrentPos.Y);
//    TP := ScalePoint(TP, 1/FContentScale, 1/FContentScale);

    FContent.TrackingPos := TP;

    FCurrentPos := PointF(X, Y);
  end;
end;

procedure TThContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
//  if FPressed and FMouseTracking then
//    RealignContent(GetContentBounds);
  inherited;

  if FMouseTracking then
  begin
    //
    FLocalMatrix.m31 := -FContent.Position.X;
    FContent.Repaint;
  end;
end;

procedure TThContainer.Paint;
begin
  Canvas.Fill.Color := claGreen;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);

  inherited;
end;

procedure TThContainer.Realign;

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

procedure TThContainer.RealignContent(R: TRectF);
var
  W, H: Single;
begin
  if (FContent <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContent.FRecalcUpdateRect := True; // need to recalc
  end;
end;

function TThContainer.ZoomIn: Single;
begin
  FContentScale := FContentScale * 1.1;

  FContent.Scale.X := FContentScale;
  FContent.Scale.Y := FContentScale;

  Result := FContent.Scale.X;
end;

function TThContainer.ZoomOut: Single;
begin
  FContentScale := FContentScale * 0.9;

  FContent.Scale.X := FContentScale;
  FContent.Scale.Y := FContentScale;

  Result := FContent.Scale.X;
end;

procedure TThContainer.AddObject(AObject: TFmxObject);
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
