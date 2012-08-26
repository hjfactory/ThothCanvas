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
    FContentScale: Single;
    procedure SetContentScale(const Value: Single);

  protected
    procedure Paint; override;

    function GetClipRect: TRectF; override;
    procedure PaintChildren; override;

    procedure AddTrackingPos(const Value: TPointF);
  public

    test: Single;
    test2: Single;

    constructor Create(AOwner: TComponent); override;
    property ContentScale: Single read FContentScale write SetContentScale;
  end;

  TThContainer = class(TStyledControl)
  private
    FMouseTracking: Boolean;

    function GetContentBounds: TRectF; virtual;
    procedure RealignContent(R: TRectF); virtual;
    function GetOffsetPos: TPointF;
  protected
    FContent: TThContent;
    FContentScale: Single;
    FDownPos,
    FCurrentPos: TPointF;

    procedure Paint; override;
    procedure BlankClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    property MouseTracking: Boolean read FMouseTracking write FMouseTracking default False;
    property OffsetPos: TPointF read GetOffsetPos;

    procedure Realign; override;
    procedure Center;

    procedure Zoom(AScale: Single); overload;
    procedure Zoom(AScale: Single; AZoomPos: TPointF); overload;
    procedure ZoomIn; overload;
    procedure ZoomIn(AZoomPos: TPointF); overload;
    procedure ZoomOut; overload;
    procedure ZoomOut(AZoomPos: TPointF); overload;

    procedure Test(A, B: Single);

    property ContentScale: Single read FContentScale;
  end;

var
  MV: Single = 0;

implementation

uses
  FMX.Platform;

{ TThContent }

constructor TThContent.Create(AOwner: TComponent);
begin
  inherited;

  FContentScale := 1;
end;

function TThContent.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);

//  Result.Left := Result.Left / Scale.X;
  Result.Right := Result.Right / Scale.X;
  Result.Bottom := Result.Bottom / Scale.Y;

//  Debug('%f %f %f %f', [Result.Left, Result.Top, Result.Right, Result.Bottom]);
//  Debug('UpdateRect %f %f %f %f', [UpdateRect.Left, UpdateRect.Top, UpdateRect.Right, UpdateRect.Bottom]);
end;

procedure TThContent.Paint;
begin
  inherited;

  Canvas.Fill.Color := claGreen;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
end;

procedure TThContent.PaintChildren;
var
  I, j: Integer;
  R: TRectF;
  State: TCanvasSaveState;
  AllowPaint: Boolean;

  M: TMatrix;

  ParentAbsP: TPointF;          // Form내 Canvas 절대 좌표

  P, RotateTopP, RotateLeftP: TPointF;
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
              // Frame 안에만 그리기 위해 FClipChildren 무시
              if {Self.FClipChildren and }CanClip then
              begin
                State := Canvas.SaveState;

                M := Self.AbsoluteMatrix;
                R := Self.ClipRect;

//                Debug('@ m31 %f m32 %f Pos %f %f R (%f, %f = %f)', [M.m31, M.m32, Position.X, Position.Y, R.Left, R.Right, R.Right - R.Left]);
                ////////////////////////////////////////////////////////////////
                // Tracking 하면 Container 좌우측이 표시되지 않는 부분 개선
                //  - M.m31 : 좌측 보정(Canvas 좌표는 Container 시작점부터)
                //  - R.Right : 우측 보정

                if Self.RotationAngle = 0 then
                begin
                  ParentAbsP  := TControl(Self.Parent).LocalToAbsolute(PointF(0, 0));
                  // m31 := Paernt의 절대 좌표 + Content 이동거리
                  M.m31 := ParentAbsP.X;
                  M.m32 := ParentAbsP.Y;

                  R.Left := 0;
                  R.Right := R.Left + (Self.Width / Self.Scale.X);

                  R.Top := 0;
                  R.Bottom := R.Top + (Self.Height / Self.Scale.X);
                end
                else
                begin
                  P := TControl(Self.Parent).LocalToAbsolute(PointF(0, 0));
                  RotateTopP := Self.AbsoluteToLocal(P);
                  RotateLeftP := Self.AbsoluteToLocal(P.Add(PointF(0, TControl(Self.Parent).Height)));
                  Self.Canvas.DrawLine(
                    RotateTopP,
                    Self.AbsoluteToLocal(P.Add(PointF(TControl(Self.Parent).Width, TControl(Self.Parent).Height))),
                    1);

                    // 1차완료 - 확대 시 오류 있음
//                  R.Top := R.Top - Abs(Self.Position.Y - RotateTopP.Y)/ Self.Scale.X;
//                  R.Bottom := R.Bottom + Abs(Self.Position.Y - RotateTopP.Y)/ Self.Scale.X;
//                  R.Left := R.Left - Abs(Self.Position.X - RotateLeftP.X)/ Self.Scale.X;
//                  R.Right := R.Right + Abs(Self.Position.X - RotateLeftP.X)/ Self.Scale.X;

                  R.Top := R.Top -  (Abs(Self.Position.Y - RotateTopP.Y)) / Self.Scale.X;
                  R.Bottom := (R.Bottom + Abs(Self.Position.Y - RotateTopP.Y)) / Self.Scale.X;
                  R.Left := R.Left - (Abs(Self.Position.X - RotateLeftP.X)) / Self.Scale.X;
                  R.Right := (R.Right + Abs(Self.Position.X - RotateLeftP.X)) / Self.Scale.X;

                  Debug('T: %f, B: %f, L: %f, R: %F', [R.Top, R.Bottom, R.Left, R.Right]);
                end;


                Canvas.SetMatrix(M);
                Canvas.IntersectClipRect(R);
                //
                ////////////////////////////////////////////////////////////////
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

procedure TThContent.SetContentScale(const Value: Single);
begin
  if FContentScale = Value then
    Exit;

  FContentScale := Value;

  Scale.X := Value;
  Scale.Y := Value;
end;

procedure TThContent.AddTrackingPos(const Value: TPointF);
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

  ClipChildren := True;

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
    FDownPos := PointF(X, Y);
    FCurrentPos := PointF(X, Y);
//    FCurrentPos := ScalePoint(PointF(X, Y), FContentScale, FContentScale);
//    Self.Cursor := crSizeAll;
  end;
end;

procedure TThContainer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  TP: TPointF;
begin
  if FPressed and FMouseTracking then
  begin
    TP := PointF(X - FCurrentPos.X, Y - FCurrentPos.Y);

    FContent.AddTrackingPos(TP);

    FCurrentPos := PointF(X, Y);
  end;
end;

procedure TThContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
{// 굳이 범위로 지정하지 않아도 될듯
  // 정교한 맞춤을 위한 이동은? 시간도 추가?
  R.TopLeft := FDownPos.Subtract(PointF(3, 3));
  R.BottomRight := FDownPos.Add(PointF(3, 3));

  if PtInRect(R, PointF(X, Y)) then
}
  if FDownPos = PointF(X, Y) then
    BlankClick;

//  Self.Cursor := crDefault;
end;

procedure TThContainer.BlankClick;
begin

end;

procedure TThContainer.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  P: TPointF;
begin
  inherited;

  P := Platform.GetMousePos;
  P := ScreenToLocal(P);

  if WheelDelta < 0 then
    ZoomOut(P)
  else
    ZoomIn(P)
  ;
end;

procedure TThContainer.Paint;
begin
  // (Test) Background color
  Canvas.Fill.Color := claPink;
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
begin
  if (FContent <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContent.FRecalcUpdateRect := True; // need to recalc
  end;
end;

procedure TThContainer.Test(A, B: Single);
begin
  FContent.test := A;
  FContent.test2 := B;
end;

procedure TThContainer.Zoom(AScale: Single; AZoomPos: TPointF);
var
  P: TPointF;
begin
  // scale 적용필요

  P.X := Width / AScale - Width / FContent.ContentScale;
  P.X := P.X * (AZoomPos.X / Width);
  P.X := P.X * AScale;
  P.Y := Height / AScale - Height / FContent.ContentScale;
  P.Y := P.Y * (AZoomPos.Y / Height);
  P.Y := P.Y * AScale;

  FContent.ContentScale := AScale;

  FContent.Position.Point := FContent.Position.Point.Add(P);

  Debug([AScale, FContent.Position.X, P.X, Width, Width / AScale, Width - Width / AScale]);

// 기준점 으로 이동하는 기능 추가 필요(마우스가 있는 좌표가 확대/축소 후에도 마우스 위치에 있도록)
end;

procedure TThContainer.ZoomIn;
begin
  ZoomIn(ClipRect.CenterPoint);
end;

procedure TThContainer.Zoom(AScale: Single);
begin
  Zoom(AScale, ClipRect.CenterPoint);
end;

procedure TThContainer.ZoomIn(AZoomPos: TPointF);
begin
  FContentScale := FContentScale * 1.1;

  Zoom(FContentScale, AZoomPos);
end;

procedure TThContainer.ZoomOut;
begin
  ZoomOut(ClipRect.CenterPoint);
end;

procedure TThContainer.ZoomOut(AZoomPos: TPointF);
begin
  FContentScale := FContentScale * 0.9;

  Zoom(FContentScale, AZoomPos);
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
