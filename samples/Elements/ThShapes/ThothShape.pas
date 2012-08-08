unit ThothShape;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types;

type
  TThSelectionPoint = class(TControl)
  private
    FPressed: Boolean;

    FFill: TBrush;
    FStroke: TBrush;
    FOverFill: TBrush;
    FStrokeThickness: Single;
    FParentBounds: Boolean;
    FGripSize: Single;
    FOnTrack: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure SetGripSize(const Value: Single);
  protected
    procedure Paint; override;

    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    function PointInObject(X, Y: Single): Boolean; override;
    function GetUpdateRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property Fill: TBrush read FFill write FFill;
    property OverFill: TBrush read FOverFill write FOverFill;
    property Stroke: TBrush read FStroke write FStroke;

    property GripSize: Single read FGripSize write SetGripSize;
    property ParentBounds: Boolean read FParentBounds write FParentBounds default True;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
  end;

  TThShape = class(TControl)
  private
    FHighlight: Boolean;
    FSelected: Boolean;

    FFill: TBrush;
    FStrokeThickness: Single;
    FStroke: TBrush;
    FStrokeCap: TStrokeCap;
    FStrokeJoin: TStrokeJoin;
    FStrokeDash: TStrokeDash;

    function IsStrokeThicknessStored: Boolean;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TBrush);
    procedure SetStrokeCap(const Value: TStrokeCap);
    procedure SetStrokeDash(const Value: TStrokeDash);
    procedure SetStrokeJoin(const Value: TStrokeJoin);
    procedure SetStrokeThickness(const Value: Single);
    procedure SetSelected(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    function GetShapeRect: TRectF; virtual;

    procedure Painting; override;
    procedure Paint; override;

    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure PaintShape; virtual; abstract;
    procedure PaintHighlightShape; virtual; abstract;

    procedure ShowSelection; virtual; abstract;
    procedure HideSelection; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Fill: TBrush read FFill write SetFill;
    property Stroke: TBrush read FStroke write SetStroke;
    property StrokeThickness: Single read FStrokeThickness
      write SetStrokeThickness stored IsStrokeThicknessStored;
    property StrokeCap: TStrokeCap read FStrokeCap write SetStrokeCap default TStrokeCap.scFlat;
    property StrokeDash: TStrokeDash read FStrokeDash write SetStrokeDash default TStrokeDash.sdSolid;
    property StrokeJoin: TStrokeJoin read FStrokeJoin write SetStrokeJoin default TStrokeJoin.sjMiter;
    property ShapeRect: TRectF read GetShapeRect;

    property Selected: Boolean read FSelected write SetSelected;
  end;

  TThRectangle = class(TThShape)
  private
    FYRadius: Single;
    FXRadius: Single;

    FSelectionPoints: array[0..3] of TThSelectionPoint;
  protected
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;

    procedure PaintShape; override;
    procedure PaintHighlightShape; override;

    procedure ShowSelection; override;
    procedure HideSelection; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
  end;

implementation

{ TThSelectionPoint }

constructor TThSelectionPoint.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;
  ParentBounds := True;

  FFill := TBrush.Create(TBrushKind.bkSolid, $FFFFFFFF);
  FOverFill := TBrush.Create(TBrushKind.bkSolid, $FFFF0000);
  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF1072C5);
  FStroke.Color := $FF1072C5;
  FStrokeThickness := 1;

  FGripSize := 3;

  Width := FGripSize * 2;
  Height := FGripSize * 2;
end;

destructor TThSelectionPoint.Destroy;
begin

  inherited;
end;

procedure TThSelectionPoint.DoMouseEnter;
begin
  inherited;
  Repaint;
end;

procedure TThSelectionPoint.DoMouseLeave;
begin
  inherited;
  Repaint;
end;

function TThSelectionPoint.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result, GripSize + 1, GripSize + 1);
end;

procedure TThSelectionPoint.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
    FPressed := True;
end;

procedure TThSelectionPoint.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  R: TRectF;
  ParentWidth, ParentHeight: Single;
begin
  inherited;
  if FPressed and (Parent <> nil) and (Parent is TControl) then
  begin
    P := LocalToAbsolute(PointF(X, Y));
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);
    if ParentBounds then
    begin
      // ParentRect 안에서만 이동
      if (Parent <> nil) and (Parent is TControl) then
      begin
        R := TThShape(Parent).GetShapeRect;

        if P.X < R.Left then
          P.X := R.Left;
        if P.Y < R.Top then
          P.Y := R.Top;

        if P.X > R.Right then
          P.X := R.Right;
        if P.Y > R.Bottom then
          P.Y := R.Bottom;
      end
      else
      if (Canvas <> nil) then
      begin
        if P.X > Canvas.Width then
          P.X := Canvas.Width;
        if P.Y > Canvas.Height then
          P.Y := Canvas.Height;
      end;
    end;
    Position.X := P.X;
    Position.Y := P.Y;

    if Assigned(FOnTrack) then
      FOnTrack(Self);
  end;
end;

procedure TThSelectionPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  FPressed := False;
end;

procedure TThSelectionPoint.Paint;
begin
  inherited;

  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.Stroke.Assign(FStroke);
  if IsMouseOver then
    Canvas.Fill.Assign(FOverFill)
  else
    Canvas.Fill.Assign(FFill);

  Canvas.FillEllipse(RectF(-(GripSize), -(GripSize), (GripSize), (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(-(GripSize), -(GripSize), (GripSize), (GripSize)), AbsoluteOpacity);
end;

function TThSelectionPoint.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if (Abs(P.X) < GripSize) and (Abs(P.Y) < GripSize) then
    Result := True;
end;

procedure TThSelectionPoint.SetGripSize(const Value: Single);
begin
  if FGripSize <> Value then
  begin
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;

    if FGripSize < FStrokeThickness + 1 then
      FGripSize := FStrokeThickness + 1;

    Repaint;
  end;
end;

procedure TThSelectionPoint.SetHeight(const Value: Single);
begin
  inherited SetHeight(FGripSize * 2);
end;

procedure TThSelectionPoint.SetWidth(const Value: Single);
begin
  inherited SetWidth(FGripSize * 2);
end;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FSelected   := False;
  FHighlight  := False;

  FFill := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF000000);
  FStroke.Color := $FF000000;
  FStroke.OnChanged := StrokeChanged;
  FStrokeThickness := 1;
end;

destructor TThShape.Destroy;
begin

  inherited;
end;

procedure TThShape.DoMouseEnter;
begin
  inherited;

  FHighlight := True;
  Repaint;
end;

procedure TThShape.DoMouseLeave;
begin
  inherited;

  FHighlight := False;
  Repaint;
end;

procedure TThShape.FillChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

procedure TThShape.StrokeChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
  if FStroke.Kind <> TBrushKind.bkNone then
    InflateRect(Result, -(FStrokeThickness / 2) - 3, -(FStrokeThickness / 2) - 3);
end;

procedure TThShape.Paint;
begin
  if FSelected then
  begin
    PaintHighlightShape;
//    ShowSelection;
  end
  else
  begin
    if FHighlight then  PaintHighlightShape
    else                PaintShape          ;
    HideSelection;
  end;
end;

procedure TThShape.Painting;
begin
  inherited;

  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);
  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.StrokeCap := FStrokeCap;
  Canvas.StrokeJoin := FStrokeJoin;
  Canvas.StrokeDash := FStrokeDash;
end;

procedure TThShape.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TThShape.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected then
    ShowSelection;
  Repaint;
end;

procedure TThShape.SetStroke(const Value: TBrush);
begin
  FStroke.Assign(Value);
end;

procedure TThShape.SetStrokeCap(const Value: TStrokeCap);
begin
  if FStrokeCap = Value then
    Exit;

  FStrokeCap := Value;
  Repaint;
end;

procedure TThShape.SetStrokeDash(const Value: TStrokeDash);
begin
  if FStrokeDash = Value then
    Exit;

  FStrokeDash := Value;
  Repaint;
end;

procedure TThShape.SetStrokeJoin(const Value: TStrokeJoin);
begin
  if FStrokeJoin = Value then
    Exit;

  FStrokeJoin := Value;
  Repaint;
end;

procedure TThShape.SetStrokeThickness(const Value: Single);
begin
  if FStrokeThickness = Value then
    Exit;

  FStrokeThickness := Value;
  Repaint;
end;

function TThShape.IsStrokeThicknessStored: Boolean;
begin
  Result := StrokeThickness <> 1;
end;

{ TThRectangle }

constructor TThRectangle.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  FXRadius := 0;
  FYRadius := 0;

  for I := 0 to 3 do
  begin
    FSelectionPoints[I] := TThSelectionPoint.Create(Self);
    FSelectionPoints[I].Parent := Self;
    FSelectionPoints[I].Visible := False;
  end;
end;

procedure TThRectangle.PaintHighlightShape;
var
  R: TRectF;
  Off: Single;
begin
  R := GetShapeRect;
  OffsetRect(R, 3, 3);

  Canvas.Fill.Color := claGray;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.StrokeThickness := 0;

  Canvas.FillRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);

  Painting;

  R := GetShapeRect;
  Canvas.FillRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

procedure TThRectangle.PaintShape;
var
  R: TRectF;
  Off: Single;
begin
  R := GetShapeRect;
  Canvas.FillRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, XRadius, YRadius, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

procedure TThRectangle.SetXRadius(const Value: Single);
begin
  if FXRadius = Value then
    Exit;

  FXRadius := Value;
  Repaint;
end;

procedure TThRectangle.SetYRadius(const Value: Single);
begin
  if FYRadius = Value then
    Exit;

  FYRadius := Value;
  Repaint;
end;

procedure TThRectangle.ShowSelection;
var
  I: Integer;
  R: TRectF;
begin
  R := GetShapeRect;

//  TControl(Parent).Repaint;

  FSelectionPoints[0].Position.Point := PointF(R.Left, R.Top);
  FSelectionPoints[1].Position.Point := PointF(R.Right, R.Top);
  FSelectionPoints[2].Position.Point := PointF(R.Left, R.Bottom);
  FSelectionPoints[3].Position.Point := PointF(R.Right, R.Bottom);

//  FSelectionPoints[0].Position.Point := PointF(R.Left - (FSelectionPoints[0].GripSize / 2), R.Top - (FSelectionPoints[0].GripSize / 2));
//  FSelectionPoints[1].Position.Point := PointF(R.Right - (FSelectionPoints[0].GripSize / 2), R.Top - (FSelectionPoints[0].GripSize / 2));
//  FSelectionPoints[2].Position.Point := PointF(R.Left - (FSelectionPoints[0].GripSize / 2), R.Bottom - (FSelectionPoints[0].GripSize / 2));
//  FSelectionPoints[3].Position.Point := PointF(R.Right - (FSelectionPoints[0].GripSize / 2), R.Bottom - (FSelectionPoints[0].GripSize / 2));

  for I := 0 to 3 do
  begin
    FSelectionPoints[I].Visible := True;
  end;
end;

procedure TThRectangle.HideSelection;
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    FSelectionPoints[I].Visible := False;
  end;
end;

end.
