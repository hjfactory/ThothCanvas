unit ThothObjects;

{
  selection은 Canvas에서 처리?? 매번 Repaint 해야함
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, System.Math,
  ThothTypes;

var
  TestObj: Integer = 0;

type
  TThInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

  TThShape = class;
  TThShapeClass = class of TThShape;

  TSelectionPointRec = record
//    Direction
  end;

///////////////////////////////////////////////////////
// Shape
  TThShape = class(TShape, IThShape)
  private
    FIndex: Integer;
    FThCanvas: IThCanvas;

    FHideSelection: Boolean;
    FMinSize: Integer;

    FRatio: Single;
    FLeftTop, FLeftBottom, FRightTop, FRightBottom: Boolean;
    FLeftTopHot, FLeftBottomHot, FRightTopHot, FRightBottomHot: Boolean;
    FDownPos, FMovePos: TPointF;
    FGripSize: Single;
    FSelected: Boolean;
    FSelections: array of TSelectionPointRec;

    function LocalToParent(P: TPointF): TPointF;
    procedure SetGripSize(const Value: Single);
    function GetEndPos: TPointF;
    function GetStartPos: TPointF;
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectionCount(const Value: Integer);
    function GetSelectionCount: Integer;
  protected
    procedure Paint; override;
    procedure DrawSelection; virtual;
    procedure SetSelectionPos; virtual;

    property SelectionCount: Integer read GetSelectionCount write SetSelectionCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property GripSize: Single read FGripSize write SetGripSize;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

    property StartPos: TPointF read GetStartPos;
    property EndPos: TPointF read GetEndPos;

    property Selected: Boolean read FSelected write SetSelected;
    property Index: Integer read FIndex write FIndex;
  end;

  TThLine = class(TThShape)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThRectangle = class(TThShape)
  private
    FSides: TSides;
    procedure SetSides(const Value: TSides); virtual;
    function IsSidesStored: Boolean;
  private
    FCornerType: TCornerType;
    FCorners: TCorners;
    FXRadius: Single;
    FYRadius: Single;
    function IsCornersStored: Boolean;
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

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

  TThCircle = class(TThShape)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses
  ThothCanvas, WinAPI.Windows;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FIndex := -1;

  FThCanvas := TThCanvas(AOwner);
  FGripSize := 3;
  FSelected := False;
  Inc(TestObj);
end;

destructor TThShape.Destroy;
begin
  Dec(TestObj);
  OutputDebugString(PChar('############## Object Destroy: ' + Self.ClassName + '(' + IntToStr(TestObj) + ')'));

  inherited;
end;

function TThShape.GetSelectionCount: Integer;
begin

end;

function TThShape.GetStartPos: TPointF;
begin
  Result := Position.Point;

//  Result := PointF(
//          IfThen(Width > 0, Position.X, Position.X + Width),
//          IfThen(Height > 0, Position.Y, Position.Y + Height));
end;

function TThShape.GetEndPos: TPointF;
begin
  Result := PointF(Position.X + Width, Position.Y + Height);
//  Result := PointF(
//          IfThen(Width < 0, Position.X, Position.X + Width),
//          IfThen(Height < 0, Position.Y, Position.Y + Height));
end;

function TThShape.LocalToParent(P: TPointF): TPointF;
begin
  Result.X := Position.X + P.X;
  Result.Y := Position.Y + P.Y;
end;

procedure TThShape.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if Value then
    SetSelectionPos;

  Repaint;
  TThCanvas(FThCanvas).Repaint;
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
  R: TRectF;
begin
  inherited;

  if Button = TMouseButton.mbLeft then
    FDownPos := PointF(X, Y);

//  if TThCanvas(FThCanvas).DrawMode = dmNone then
//    TThCanvas(FThCanvas).DrawMode := dmSelect;

  if TThCanvas(FThCanvas).DrawMode = dmNone then
  begin
    if ssShift in Shift then
      if FSelected then
        TThCanvas(FThCanvas).Unselection(Self)
      else
        TThCanvas(FThCanvas).AddSelection(Self)
    else
      if not FSelected then
        TThCanvas(FThCanvas).SetSelection(Self);
    TThCanvas(FThCanvas).DrawMode := dmSelect;
  end;

  P := LocalToParent(PointF(X, Y));
  TThCanvas(FThCanvas).MouseDown(Button, Shift, P.X, P.Y);


  if TThCanvas(FThCanvas).DrawMode in [dmSelect] then
  begin

    if FSelected then
    begin
      if Button = TMouseButton.mbLeft then
      begin
        FRatio := Width / Height;
        R := LocalRect;
        R := RectF(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize));
        if PointInRect(FDownPos, R) then
        begin
          FLeftTop := True;
          Exit;
        end;
        R := LocalRect;
        R := RectF(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize));
        if PointInRect(FDownPos, R) then
        begin
          FRightTop := True;
          Exit;
        end;
        R := LocalRect;
        R := RectF(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize));
        if PointInRect(FDownPos, R) then
        begin
          FRightBottom := True;
          Exit;
        end;
        R := LocalRect;
        R := RectF(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize));
        if PointInRect(FDownPos, R) then
        begin
          FLeftBottom := True;
          Exit;
        end;
      end;
    end
    else
    begin
      if ssShift in Shift then
        TThCanvas(FThCanvas).AddSelection(Self)
      else
        TThCanvas(FThCanvas).SetSelection(Self);
    end;
  end;
end;

procedure TThShape.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  R: TRectF;
begin
  inherited;

  P := LocalToParent(PointF(X, Y));
  TThCanvas(FThCanvas).MouseMove(Shift, P.X, P.Y);


  if Shift = [] then
  begin
    // handle painting for hotspot mouse hovering
    FMovePos := PointF(X, Y);
    P := LocalToAbsolute(FMovePos);
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);

    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FMovePos, R) xor FLeftTopHot then
    begin
      FLeftTopHot := not FLeftTopHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FMovePos, R) xor FRightTopHot then
    begin
      FRightTopHot := not FRightTopHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FMovePos, R) xor FRightBottomHot then
    begin
      FRightBottomHot := not FRightBottomHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FMovePos, R) xor FLeftBottomHot then
    begin
      FLeftBottomHot := not FLeftBottomHot;
      Repaint;
    end;
  end;

end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToParent(PointF(X, Y));
  TThCanvas(FThCanvas).MouseUp(Button, Shift, P.X, P.Y);
end;

procedure TThShape.Paint;
begin
  inherited;

  if FHideSelection or not FSelected then
    Exit;
  DrawSelection;
end;

procedure TThShape.DoMouseLeave;
begin
  inherited;

  FLeftTopHot := False;
  FLeftBottomHot := False;
  FRightTopHot := False;
  FRightBottomHot := False;
  Repaint;
end;

procedure TThShape.DrawSelection;
var
  R: TRectF;
begin
  R := LocalRect;
//  InflateRect(R, -0.5, -0.5);
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  Canvas.Fill.Color := $FFFFFFFF;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Color := $FF1072C5;
  Canvas.StrokeDash := TStrokeDash.sdDash;
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.StrokeDash := TStrokeDash.sdSolid;

  R := LocalRect;
//  InflateRect(R, -0.5, -0.5);
  if FLeftTopHot then
    Canvas.Fill.Color := $FFFF0000
  else
    Canvas.Fill.Color := $FFFFFFFF;
  Canvas.FillEllipse(RectF(R.Left - (GripSize), R.Top - (GripSize),
    R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Left - (GripSize), R.Top - (GripSize),
    R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  if FRightTopHot then
    Canvas.Fill.Color := $FFFF0000
  else
    Canvas.Fill.Color := $FFFFFFFF;
  Canvas.FillEllipse(RectF(R.Right - (GripSize), R.Top - (GripSize),
    R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Right - (GripSize), R.Top - (GripSize),
    R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  if FLeftBottomHot then
    Canvas.Fill.Color := $FFFF0000
  else
    Canvas.Fill.Color := $FFFFFFFF;
  Canvas.FillEllipse(RectF(R.Left - (GripSize), R.Bottom - (GripSize),
    R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Left - (GripSize), R.Bottom - (GripSize),
    R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  if FRightBottomHot then
    Canvas.Fill.Color := $FFFF0000
  else
    Canvas.Fill.Color := $FFFFFFFF;
  Canvas.FillEllipse(RectF(R.Right - (GripSize), R.Bottom - (GripSize),
    R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Right - (GripSize), R.Bottom - (GripSize),
    R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
end;

procedure TThShape.SetSelectionCount(const Value: Integer);
begin
//  FSelectionCount := Value;
end;

procedure TThShape.SetSelectionPos;
begin

end;

procedure TThShape.SetGripSize(const Value: Single);
begin
  if FGripSize <> Value then
  begin
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;
    if FGripSize < 1 then
      FGripSize := 1;
    Repaint;
  end;
end;

{ TThLine }

constructor TThLine.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TThLine.Paint;
begin
  inherited;

  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Color := $FFFF0000;
//  Canvas.StrokeDash := TStrokeDash.sdDash;
  Canvas.DrawLine(GetShapeRect.TopLeft, GetShapeRect.BottomRight,
    AbsoluteOpacity);
end;

{ TThRectangle }

constructor TThRectangle.Create(AOwner: TComponent);
begin
  inherited;

  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;

  Fill.Kind := TBrushKind.bkNone;
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
  inherited;
end;

procedure TThRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

procedure TThRectangle.SetCornerType(const Value: TCornerType);
begin
  if FCornerType <> Value then
  begin
    FCornerType := Value;
    Repaint;
  end;
end;

procedure TThRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    FSides := Value;
    Repaint;
  end;
end;

procedure TThRectangle.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then
  begin
    FXRadius := Value;
    Repaint;
  end;
end;

procedure TThRectangle.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then
  begin
    FYRadius := Value;
    Repaint;
  end;
end;

{ TTHCircle }

constructor TThCircle.Create(AOwner: TComponent);
begin
  inherited;

  Fill.Kind := TBrushKind.bkNone;
end;

procedure TThCircle.Paint;
var
  R: TRectF;
begin
  R := RectF(0, 0, Max(Width, Height), Max(Width, Height));
  FitRect(R, GetShapeRect);
  Canvas.FillEllipse(R, AbsoluteOpacity);
  Canvas.DrawEllipse(R, AbsoluteOpacity);
end;

{ TThInterfacedObject }

function TThInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TThInterfacedObject._AddRef: Integer;
begin
  Result := 0;
end;

function TThInterfacedObject._Release: Integer;
begin
  Result := 0;
end;

end.
