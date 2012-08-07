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
  end;

  TThShape = class(TControl)
  private
    FFill: TBrush;
    FStrokeThickness: Single;
    FStroke: TBrush;
    FStrokeCap: TStrokeCap;
    FStrokeJoin: TStrokeJoin;
    FStrokeDash: TStrokeDash;
    FSelected: Boolean;
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
    procedure AfterPaint; override;

    procedure ShowSelection; virtual; abstract;
    procedure HideSelection; virtual; abstract;
    procedure DrawHighlight; virtual; abstract;
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
    FCorners: TCorners;
    FCornerType: TCornerType;
    FSides: TSides;

    FSelectionPoints: array[0..3] of TThSelectionPoint;

    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
  protected
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetCornerType(const Value: TCornerType); virtual;
    procedure SetSides(const Value: TSides); virtual;

    procedure Paint; override;

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
    property Corners: TCorners read FCorners write SetCorners
      stored IsCornersStored;
    property CornerType: TCornerType read FCornerType write SetCornerType
      default TCornerType.ctRound;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
  end;

implementation

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

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
    InflateRect(Result, -(FStrokeThickness / 2), -(FStrokeThickness / 2));
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

procedure TThShape.AfterPaint;
begin
  inherited AfterPaint;

  Canvas.StrokeDash := TStrokeDash.sdSolid;
  Canvas.StrokeThickness := 1;
end;

procedure TThShape.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TThShape.SetSelected(const Value: Boolean);
begin
  FSelected := Value;

  if FSelected then    ShowSelection
  else                 HideSelection  ;
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
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;

  for I := 0 to 3 do
  begin
    FSelectionPoints[I] := TThSelectionPoint.Create(Self);
    FSelectionPoints[I].Parent := Self;
    FSelectionPoints[I].Visible := False;
  end;
end;

function TThRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

function TThRectangle.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TThRectangle.Paint;
var
  R: TRectF;
  Off: Single;
begin
  R := GetShapeRect;
  if Sides <> AllSides then
  begin
    Off := R.Left;
    if not(TSide.sdTop in FSides) then
      R.Top := R.Top - Off;
    if not(TSide.sdLeft in FSides) then
      R.Left := R.Left - Off;
    if not(TSide.sdBottom in FSides) then
      R.Bottom := R.Bottom + Off;
    if not(TSide.sdRight in FSides) then
      R.Right := R.Right + Off;
    Canvas.FillRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
    Canvas.DrawRectSides(GetShapeRect, XRadius, YRadius, FCorners,
      AbsoluteOpacity, Sides, CornerType);
  end
  else
  begin
    Canvas.FillRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
    Canvas.DrawRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
  end;
end;

procedure TThRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners = Value then
    Exit;

  FCorners := Value;
  Repaint;
end;

procedure TThRectangle.SetCornerType(const Value: TCornerType);
begin
  if FCornerType = Value then
    Exit;

  FCornerType := Value;
  Repaint;
end;

procedure TThRectangle.SetSides(const Value: TSides);
begin
  if FSides = Value then
    Exit;

  FSides := Value;
  Repaint;
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
begin
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
begin
  inherited;
  if FPressed and (Parent <> nil) and (Parent is TControl) then
  begin
    P := LocalToAbsolute(PointF(X, Y));
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);
    if ParentBounds then
    begin
      if P.X < 0 then
        P.X := 0;
      if P.Y < 0 then
        P.Y := 0;
      if (Parent <> nil) and (Parent is TControl) then
      begin
        if P.X > TControl(Parent).Width then
          P.X := TControl(Parent).Width;
        if P.Y > TControl(Parent).Height then
          P.Y := TControl(Parent).Height;
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
//    if Assigned(FOnTrack) then
//      FOnTrack(Self);
  end;
end;

procedure TThSelectionPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed then
  begin
//    if Assigned(FOnChange) then
//      FOnChange(Self);
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

end.
