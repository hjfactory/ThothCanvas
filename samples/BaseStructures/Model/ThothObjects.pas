unit ThothObjects;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects,
  ThothTypes;

type
  TThShape = class;
  TThShapeClass = class of TThShape;

///////////////////////////////////////////////////////
// Shape
  TThShape = class(TShape, IThShape)
  private
    FThCanvas: IThCanvas;
    //
    function LocalToParent(P: TPointF): TPointF;
  public
    constructor Create(AOwner: TComponent); override;

//    property ThCanvas: TThCanvas read FThCanvas;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

  TThLine = class(TThShape)
    // implement
      // Selection point
      //
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
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
    procedure Paint; override;
    function IsCornersStored: Boolean;
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
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

  TTHCircle = class(TThShape)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;


implementation

uses
  ThothCanvas;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FThCanvas := TThCanvas(AOwner);
end;

function TThShape.LocalToParent(P: TPointF): TPointF;
begin
//  FOrm1.Memo1.Lines.Add(Format('LocalToParent', []));

  Result.X := Position.X + P.X;
  Result.Y := Position.Y + P.Y;
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToParent(PointF(X, Y));
  FThCanvas.MouseDown(Button, Shift, P.X, P.Y);
//  FThCanvas.DrawBegin(Self);
end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

//  P := LocalToParent(PointF(X, Y));
//  FThCanvas.MouseUp(Button, Shift, P.X, P.Y);
//  FThCanvas.DrawEnd(Self);
end;

{ TThLine }

constructor TThLine.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TThLine.Paint;
begin
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

constructor TTHCircle.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TTHCircle.Paint;
begin
  inherited;

end;

end.
