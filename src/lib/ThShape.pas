unit ThShape;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types, System.SysUtils, FMX.Platform;

type
  TSelectionPosition = (spTopLeft, spTop, spTopRight, spLeft, spRight, spBottomLeft, spBottom, spBottomRight{, spCustom});

  TThShapeControl = class(TControl)
  protected
    FInheritedOpacity: Boolean;
    FInheritedScale: Boolean;

    function GetAbsoluteOpacity: Single; override;
    function GetAbsoluteMatrix: TMatrix; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThSelectionPoint = class(TThShapeControl)
  private
    FSelectionPosition: TSelectionPosition;
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
    function GetUpdateRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PointInObject(X, Y: Single): Boolean; override;

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

    property SelectionPosition: TSelectionPosition read FSelectionPosition write FSelectionPosition;
  end;

  TThShape = class(TControl)
  private
    FPressed: Boolean;
    FDownPos: TPointF;
    FHighlight: Boolean;
    FSelected: Boolean;

    FFill: TBrush;
    FStrokeThickness: Single;
    FStroke: TBrush;
    FStrokeCap: TStrokeCap;
    FStrokeJoin: TStrokeJoin;
    FStrokeDash: TStrokeDash;
    FShadowSize: Single;
    FShadowColor: TAlphaColor;
    FGripSize: Single;
    FMinHeight: Single;
    FMinWidth: Single;

    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TBrush);
    procedure SetStrokeCap(const Value: TStrokeCap);
    procedure SetStrokeDash(const Value: TStrokeDash);
    procedure SetStrokeJoin(const Value: TStrokeJoin);
    procedure SetStrokeThickness(const Value: Single);
    procedure SetSelected(const Value: Boolean);
    procedure SetShadowSize(const Value: Single);
    procedure SetGripSize(const Value: Single);

    procedure AddSelectionPoints(const Args: array of TSelectionPosition);
    procedure SetSelectionPoints(const Args: array of TSelectionPosition);
    function GetSelectionPoint(Index: Integer): TThSelectionPoint;
  protected
    FSelectionPoints: TList;

    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SelectionPointTrack(Sender: TObject); virtual;
    procedure SelectionPointChange(Sender: TObject); virtual;
    function GetShapeRect: TRectF; virtual;
    function GetShadowRect: TRectF; virtual;

    procedure Paint; override;

    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure PaintShape; virtual;
    procedure PaintShadow; virtual;

    procedure ShowSelection; virtual;
    procedure HideSelection; virtual;

    function PtInShape(APt: TPointF): Boolean; virtual;
    property SelectionPoint[Index: Integer]: TThSelectionPoint read GetSelectionPoint;

    function PointInObject(X, Y: Single): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Fill: TBrush read FFill write SetFill;
    property Stroke: TBrush read FStroke write SetStroke;
    property StrokeThickness: Single read FStrokeThickness write SetStrokeThickness;
    property StrokeCap: TStrokeCap read FStrokeCap write SetStrokeCap default TStrokeCap.scFlat;
    property StrokeDash: TStrokeDash read FStrokeDash write SetStrokeDash default TStrokeDash.sdSolid;
    property StrokeJoin: TStrokeJoin read FStrokeJoin write SetStrokeJoin default TStrokeJoin.sjMiter;
    property ShapeRect: TRectF read GetShapeRect;

    property GripSize: Single read FGripSize write SetGripSize;
    property ShadowSize: Single read FShadowSize write SetShadowSize;
    property Selected: Boolean read FSelected write SetSelected;

    property MinWidth: Single read FMinWidth write FMinWidth;
    property MinHeight: Single read FMinHeight write FMinHeight;
  end;

  TThLineShape = class(TThShape)
  protected
    procedure PaintShadow; override;
  end;

  TThRectangle = class(TThShape)
  protected
    procedure PaintShape; override;
    procedure PaintShadow; override;
  public
    constructor Create(AOnwer: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

  TThLine = class(TThLineShape)
  protected
    procedure PaintShape; override;
    procedure PaintShadow; override;

    procedure SelectionPointTrack(Sender: TObject); override;
    procedure SelectionPointChange(Sender: TObject); override;

    function PtInShape(APt: TPointF): Boolean; override;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  published
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

implementation

uses
  System.Math;

const
  __GRIP_SIZE   = 3;
  __SHADOW_SIZE = 3;
  __MIN_SIZE    = 50;

{ TThShapeControl }

constructor TThShapeControl.Create(AOwner: TComponent);
begin
  inherited;

  FInheritedOpacity := True;
  FInheritedScale := True;
end;

// Parent의 Scale 변경에 영향을 받지 않도록 처리
function TThShapeControl.GetAbsoluteMatrix: TMatrix;
begin
  Result := inherited GetAbsoluteMatrix;
  if not FInheritedScale then
  begin
//    Result.m31 := Position.X
//    Result.m32
    Result.m11 := Scale.X;
    Result.m22 := Scale.Y;
  end;
end;

function TThShapeControl.GetAbsoluteOpacity: Single;
begin
  if FInheritedOpacity then
    Result := inherited
  else
    Result := FOpacity;
end;

{ TThSelectionPoint }

constructor TThSelectionPoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoCapture := True;
  ParentBounds := True;

  FInheritedOpacity := False;
  FInheritedScale := False;

  FFill := TBrush.Create(TBrushKind.bkSolid, $FFFFFFFF);
  FOverFill := TBrush.Create(TBrushKind.bkSolid, $FFFF0000);
  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF1072C5);
  FStroke.Color := $FF1072C5;
  FStrokeThickness := 1;

  FGripSize := __GRIP_SIZE;

  Width := FGripSize * 2;
  Height := FGripSize * 2;
end;

destructor TThSelectionPoint.Destroy;
begin
  FStroke.Free;
  FFill.Free;

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

        P.X := Max(P.X, R.Left);
        P.X := Min(P.X, R.Right);
        P.Y := Max(P.Y, R.Top);
        P.Y := Min(P.Y, R.Bottom);
      end
      else if (Canvas <> nil) then
      begin
        P.X := Min(P.X, Canvas.Width);
        P.Y := Min(P.Y, Canvas.Height);
      end;
    end;

    Position.Point := P;

    if Assigned(FOnTrack) then
      FOnTrack(Self);
  end;
end;

procedure TThSelectionPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed then
    if Assigned(FOnChange) then
      FOnChange(Self);
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

  AutoCapture := True;

  FSelected   := False;
  FHighlight  := False;
  FGripSize   := __GRIP_SIZE;
  FShadowSize := __SHADOW_SIZE;
  FShadowColor  := claGray;

  FSelectionPoints := TList.Create;

  FMinWidth := __MIN_SIZE;
  FMinHeight := __MIN_SIZE;

  FFill := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF000000);
  FStroke.Color := $FF000000;
  FStroke.OnChanged := StrokeChanged;
  FStrokeThickness := 1;
end;

destructor TThShape.Destroy;
var
  I: Integer;
begin
  FStroke.Free;
  FFill.Free;
  for I := FSelectionPoints.Count - 1 downto 0 do
    TThSelectionPoint(FSelectionPoints[I]).Free;
  FSelectionPoints.Free;

  inherited;
end;

procedure TThShape.AddSelectionPoints(const Args: array of TSelectionPosition);
var
  I: Integer;
  SP: TThSelectionPoint;
begin
  FSelectionPoints.Clear;

  for I := 0 to Length(Args) - 1 do
  begin
    SP := TThSelectionPoint.Create(Self);
    SP.SelectionPosition := Args[I];
    SP.OnTrack := SelectionPointTrack;
    SP.OnChange := SelectionPointChange;
    SP.Parent := Self;
    SP.ParentBounds := False;
    SP.Visible := False;

    FSelectionPoints.Add(SP);
  end;
end;

procedure TThShape.SetSelectionPoints(const Args: array of TSelectionPosition);
var
  I: Integer;
begin
  for I := 0 to FSelectionPoints.Count - 1 do
    if Length(Args) > I then
      GetSelectionPoint(I).SelectionPosition := Args[I];
end;

procedure TThShape.DoMouseEnter;
begin
  inherited;
end;

procedure TThShape.DoMouseLeave;
begin
  inherited;

  FHighlight := False;
  Repaint;
end;

function TThShape.GetSelectionPoint(Index: Integer): TThSelectionPoint;
begin
  Result := nil;
  if FSelectionPoints.Count > Index then
    Result := TThSelectionPoint(FSelectionPoints[Index]);
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
  if FStroke.Kind <> TBrushKind.bkNone then
  begin
    InflateRect(Result, -(FGripSize / 2 / Scale.X), -(FGripSize / 2 / Scale.Y));
    Result.Right := Result.Right - FShadowSize  / Scale.X;
    Result.Bottom := Result.Bottom - FShadowSize / Scale.Y;
  end;
end;

function TThShape.GetShadowRect: TRectF;
begin
  Result := GetShapeRect;
  OffsetRect(Result, FShadowSize / Scale.X, FShadowSize / Scale.Y);
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and (PtInShape(PointF(X, Y))) then
  begin
    Selected := True;
    FPressed := True;
    FDownPos := PointF(X, Y);
  end
  else
  begin
    if Assigned(Parent) and (Parent is TControl) then
    begin

    end;
  end;
end;

procedure TThShape.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  inherited;
  if FPressed and (Parent <> nil) and (Parent is TControl) then
  begin
    P := LocalToAbsolute(PointF(X, Y));
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);

    Position.X := Position.X + (X - FDownPos.X);
    Position.Y := Position.Y + (Y - FDownPos.Y);

//    if Assigned(FOnTrack) then
//      FOnTrack(Self);
  end
  else
  begin
    P := Platform.GetMousePos;
    P := ScreenToLocal(P);

    FHighlight := PtInShape(P);
    Repaint;
  end;

end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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

procedure TThShape.Paint;
begin
  if FHighlight or FSelected then
    PaintShadow;

  PaintShape;

  if FSelected and (FUpdating = 0) then
    ShowSelection;
end;

procedure TThShape.PaintShadow;
begin
  Canvas.Fill.Color := FShadowColor;
  Canvas.StrokeThickness := FShadowSize;
  Canvas.Stroke.Kind := TBrushKind.bkNone;

  Canvas.Stroke.Color := FShadowColor;
  Canvas.StrokeCap := FStrokeCap;
  Canvas.StrokeJoin := FStrokeJoin;
  Canvas.StrokeDash := FStrokeDash;
end;

procedure TThShape.PaintShape;
begin
  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);
  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.StrokeCap := FStrokeCap;
  Canvas.StrokeJoin := FStrokeJoin;
  Canvas.StrokeDash := FStrokeDash;
end;

function TThShape.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := inherited;

  if Result then
  begin
    P := AbsoluteToLocal(PointF(X, Y));
    Result := PtInShape(P);
  end;
end;

function TThShape.PtInShape(APt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, APt);
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

procedure TThShape.SelectionPointChange(Sender: TObject);
begin

end;

procedure TThShape.SelectionPointTrack(Sender: TObject);
var
  SP: TThSelectionPoint;
  R: TRectF;
  P: TPointF;
  W, H: Single;
begin
  SP := TThSelectionPoint(Sender);
  R := GetShapeRect;

  P := Position.Point;
  W := Width;
  H := Height;

  // Left
  if sp.SelectionPosition in [spLeft, spTopLeft, spBottomLeft] then
  begin
    W := W - SP.Position.X;
    if W < FMinWidth then
    begin
      SP.Position.X := SP.Position.X - (FMinWidth - W);
      W := FMinWidth;
    end;
    P.X := P.X + SP.Position.X;
  end;

  // Right
  if sp.SelectionPosition in [spRight, spTopRight, spBottomRight] then
  begin
    W := SP.Position.X + SP.GripSize * 2;
    W := Max(W, FMinWidth);
  end;

  // Top
  if sp.SelectionPosition in [spTop, spTopLeft, spTopRight] then
  begin
    H := H - SP.Position.Y;
    if H < FMinHeight then
    begin
      SP.Position.Y := SP.Position.Y - (FMinHeight - H);
      H := FMinHeight;
    end;
    P.Y := P.Y + SP.Position.Y;
  end;

  // Bottom
  if sp.SelectionPosition in [spBottom, spBottomLeft, spBottomRight] then
  begin
    H := SP.Position.Y + SP.GripSize * 2;
    H := Max(H, FMinHeight);
  end;

  Width := W;
  Height := H;
  Position.Point := P;

  ShowSelection;
end;

procedure TThShape.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TThShape.SetGripSize(const Value: Single);
var
  I: Integer;
begin
  if FGripSize = Value then
    Exit;

  FGripSize := Value;

  for I := 0 to FSelectionPoints.Count - 1 do
    SelectionPoint[I].GripSize := FGripSize;

  Repaint;

  if FSelected then
    ShowSelection;
end;

procedure TThShape.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected then
    ShowSelection
  else
    HideSelection;

  Repaint;
end;

procedure TThShape.SetShadowSize(const Value: Single);
begin
  if FShadowSize = Value then
    Exit;
  FShadowSize := Value;
  Repaint;

  if FSelected then
    ShowSelection;
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

procedure TThShape.ShowSelection;
var
  I: Integer;
  P: TPointF;
  R: TRectF;
  SP: TThSelectionPoint;
begin
  R := GetShapeRect;

  for I := 0 to FSelectionPoints.Count - 1 do
  begin
    SP := SelectionPoint[I];

    case SP.SelectionPosition of
      spTopLeft:      P := PointF(R.Left, R.Top);
      spTop:          P := PointF(RectWidth(R) / 2, R.Top);
      spTopRight:     P := PointF(R.Right, R.Top);
      spLeft:         P := PointF(R.Left, RectHeight(R) / 2);
      spRight:        P := PointF(R.Right, RectHeight(R) / 2);
      spBottomLeft:   P := PointF(R.Left, R.Bottom);
      spBottom:       P := PointF(RectWidth(R) / 2, R.Bottom);
      spBottomRight:  P := PointF(R.Right, R.Bottom);
    end;

    SP.Position.Point := P;
    SP.Visible := True;
  end;

  Repaint;
end;

procedure TThShape.HideSelection;
var
  I: Integer;
begin
  for I := 0 to FSelectionPoints.Count - 1 do
    SelectionPoint[I].Visible := False;
end;

{ TThLineShape }

procedure TThLineShape.PaintShadow;
begin
//  Canvas.Fill.Color := FShadowColor;
  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
end;

{ TThRectangle }

constructor TThRectangle.Create;
begin
  inherited;

  AddSelectionPoints([spTopLeft, spTopRight, spBottomLeft, spBottomRight
//  ,spLeft, spTop, spRight, spBottom // TEST
  ]);
end;

procedure TThRectangle.PaintShadow;
var
  R: TRectF;
begin
  inherited;
  R := GetShadowRect;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

procedure TThRectangle.PaintShape;
var
  R: TRectF;
begin
  inherited;
  R := GetShapeRect;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

{ TThLine }

constructor TThLine.Create(AOnwer: TComponent);
begin
  inherited;

  FStrokeCap := TStrokeCap.scRound;

  AddSelectionPoints([spTopLeft, spBottomRight
//  ,spLeft, spTop, spRight, spBottom // TEST
  ]);
end;

destructor TThLine.Destroy;
begin
//  FEndPosition.Free;

  inherited;
end;

procedure TThLine.PaintShape;
var
  P1, P2: TPointF;
begin
  inherited;

  if SelectionPoint[0].SelectionPosition = spTopLeft then
  begin
    P1 := GetShapeRect.TopLeft;
    P2 := GetShapeRect.BottomRight;
  end
  else
  begin
    P1 := PointF(GetShapeRect.Left, GetShapeRect.Bottom);
    P2 := PointF(GetShapeRect.Right, GetShapeRect.Top);
  end;

  Canvas.DrawLine(P1, P2, AbsoluteOpacity);
end;

procedure TThLine.PaintShadow;
var
  P1, P2: TPointF;
begin
  inherited;

  if SelectionPoint[0].SelectionPosition = spTopLeft then
  begin
    P1 := GetShadowRect.TopLeft;
    P2 := GetShadowRect.BottomRight;
  end
  else
  begin
    P1 := PointF(GetShadowRect.Left, GetShadowRect.Bottom);
    P2 := PointF(GetShadowRect.Right, GetShadowRect.Top);
  end;
  Canvas.DrawLine(P1, P2, AbsoluteOpacity);
end;

function TThLine.PtInShape(APt: TPointF): Boolean;
var
  Rad: Single;
  W, H, Y0, Y1: Single;
  R: Single;  // Range(Line에서 마우스를 인식하는 범위)
  RX,         // R의 X좌표
  YR: Single; // Apt.X의 유효 Y좌표 범위
  X, Y: Single;
begin
  Result := False;

  W := RectWidth(GetShapeRect);
  H := RectHeight(GetShapeRect);

  X := APt.X;
  if SelectionPoint[0].SelectionPosition = spTopLeft then
    Y := Apt.Y
  else
    Y := H - APt.Y;

  Rad := ArcTan(H/W);
  Y0  := Tan(Rad) * X;
  R   := 5;
  RX   := R / Cos(DegToRad(90) - Rad);
  Y1  := Tan(Rad) * (X - RX);
  YR   := Abs(Y1 - Y0);

  Result := Abs(Y0 - Y) < YR;
end;

procedure TThLine.SelectionPointTrack(Sender: TObject);
var
  SP, OtherSP: TThSelectionPoint;
  R: TRectF;
  P: TPointF;
  W, H: Single;
begin
  SP := TThSelectionPoint(Sender);
  R := GetShapeRect;

  P := Position.Point;
  W := Width;
  H := Height;

  // Left
  if SP.SelectionPosition in [spTopLeft, spBottomLeft] then
  begin
    W := W - SP.Position.X;
    P.X := P.X + SP.Position.X;
  end;

  // Top
  if SP.SelectionPosition in [spTopLeft, spTopRight] then
  begin
    H := H - SP.Position.Y;
    P.Y := P.Y + SP.Position.Y;
  end;

  // Right
  if SP.SelectionPosition in [spTopRight, spBottomRight] then
    W := SP.Position.X + FGripSize * 2;

  // Bottom
  if SP.SelectionPosition in [spBottomLeft, spBottomRight] then
    H := SP.Position.Y + FGripSize * 2;

  Position.X := P.X;
  Position.Y := P.Y;
  Width := W;//Abs(W);
  Height := H; //Abs(H);
end;

procedure TThLine.SelectionPointChange(Sender: TObject);
  // 좌우, 상하가 변했을때 선택점 반전 처리
  procedure _ExchangeSelectionPosition;
  begin
    if SelectionPoint[0].SelectionPosition = spTopLeft then
      SetSelectionPoints([spBottomLeft, spTopRight])
    else
      SetSelectionPoints([spTopLeft, spBottomRight]);
  end;

var
  P: TPointF;
  W, H: Single;
begin
  inherited;

  W := Width;
  H := Height;
  P := Position.Point;

  if W < 0 then
  begin
    P.X := P.X + W - GripSize * 2;
    W := W - GripSize * 4;
    if H >= 0 then
      _ExchangeSelectionPosition;
  end;
  if H < 0 then
  begin
    P.Y := P.Y + H - GripSize * 2;
    H := H - GripSize * 4;
    if W >= 0 then
      _ExchangeSelectionPosition;
  end;

  Width := Abs(W);
  Height := Abs(H);
  Position.Point := P;
end;

end.
