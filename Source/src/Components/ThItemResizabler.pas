unit ThItemResizabler;

interface

uses
  ThItemResizablerIF, System.Classes, System.Types, FMX.Types, System.UITypes,
  System.Generics.Collections, ThConsts;

type
  TSpotCorner = (
                              scTop          = 1,
                              scLeft         = 2,
                              scRight        = 4,
                              scBottom       = 8,
                              scTopLeft      = 3{rsdTop or rsdLeft},
                              scTopRight     = 5{rsdTop or rsdRight},
                              scBottomLeft   = 10{rsdBottom or rsdLeft},
                              scBottomRight  = 12{rsdBottom or rsdRight}{, spCustom});

const
  HORIZONTAL_CORNERS  = TSpotCorner(Ord(scLeft) or Ord(scRight));
  VERTICAL_CORNERS    = TSpotCorner(Ord(scTop) or Ord(scBottom));

function AndSpotCorner(D1, D2: TSpotCorner): TSpotCorner;

type
//  TItemResizableSpot = class;
//  TResizableSpotTrackEvent = procedure(Spot: TItemResizableSpot) of object;
  TItemResizableSpot = class(TControl, IItemResizableSpot)
  private
    FSpotCorner: TSpotCorner;
    FOnTracking: TTrackEvent;
    procedure SetSpotCorner(const Value: TSpotCorner);
  public
    constructor Create(AOwner: TComponent; ASpotCorner: TSpotCorner); reintroduce; virtual;

    property SpotCorner: TSpotCorner read FSpotCorner write SetSpotCorner;
    property OnTrack: TTrackEvent read FOnTracking write FOnTracking;
  end;

  TResizableSpotClass = class of TItemResizableSpot;
  TItemResizabler = class(TInterfacedObject, IItemResizabler)
  private
    FSpotClass: TResizableSpotClass;
    FList: TList;
    FOnTracking: TTrackEvent;
    function GetSpots(Index: Integer): IItemResizableSpot;
    function GetCount: Integer;
  protected
    FParent: TControl;
    function GetResizablerRect: TRectF; virtual; abstract;
    procedure DoResizableSpotTrack(Sender: TObject; X, Y: Single);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure SetSpotClass(ASpotClass: TResizableSpotClass);
    procedure SetResizableSpots(Spots: array of TSpotCorner);

    property Spots[Index: Integer] : IItemResizableSpot read GetSpots; default;
    property Count: Integer read GetCount;
    property OnTrack: TTrackEvent read FOnTracking write FOnTracking;
  end;

  TThItemCircleResizableSpot = class(TItemResizableSpot)
  private
    FMouseDownPos: TPointF;
  protected
    procedure Paint; override;
    function GetUpdateRect: TRectF; override;
//    function GetClipRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent; ADirection: TSpotCorner); override;
    function PointInObject(X, Y: Single): Boolean; override;
  end;

  TThItemFillResizabler = class(TItemResizabler)
  protected
    function GetResizablerRect: TRectF; override;
  public
    property OnTrack;
  end;

implementation

uses
  System.UIConsts, CommonUtils;

function AndSpotCorner(D1, D2: TSpotCorner): TSpotCorner;
begin
  Result := TSpotCorner(Ord(D1) and Ord(D2))
end;

{ TItemResizableSpot }

constructor TItemResizableSpot.Create(AOwner: TComponent;
  ASpotCorner: TSpotCorner);
begin
  inherited Create(AOwner);

  Opacity := 1;
  FSpotCorner := ASpotCorner;
end;

procedure TItemResizableSpot.SetSpotCorner(const Value: TSpotCorner);
begin
  FSpotCorner := Value;
end;

{ TItemResizabler }

constructor TItemResizabler.Create(AOwner: TComponent);
begin
  FParent := TControl(AOwner);
  FList := TList.Create;
end;

destructor TItemResizabler.Destroy;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TControl(FList[I]).Free;

  inherited;
end;

procedure TItemResizabler.DoResizableSpotTrack(Sender: TObject; X, Y: Single);
begin
  if Assigned(OnTrack) then
    OnTrack(Sender, X, Y);
end;

function TItemResizabler.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TItemResizabler.GetSpots(Index: Integer): IItemResizableSpot;
begin
  if FList.Count > Index then
    Result := IItemResizableSpot(TItemResizableSpot(FList[Index]));
end;

procedure TItemResizabler.SetResizableSpots(
  Spots: array of TSpotCorner);
var
  I: Integer;
  Spot: TItemResizableSpot;
begin
  assert(Assigned(FSpotClass), 'Not assigned items Spot class');

  for I := 0 to Length(Spots) - 1 do
  begin
    Spot := FSpotClass.Create(FParent, Spots[I]);
    Spot.Parent := FParent;
    Spot.OnTrack := DoResizableSpotTrack;
    Spot.Visible := False;
    FList.Add(Spot);
  end;
end;

procedure TItemResizabler.SetSpotClass(ASpotClass: TResizableSpotClass);
begin
  FSpotClass := ASpotClass;
end;

{ TThItemCircleResizableSpot }

constructor TThItemCircleResizableSpot.Create(AOwner: TComponent; ADirection: TSpotCorner);
begin
  inherited;

  AutoCapture := True;

  Width := ItemResizableSpotRadius * 2;
  Height := ItemResizableSpotRadius * 2;
end;

procedure TThItemCircleResizableSpot.DoMouseEnter;
begin
  inherited;

  Repaint;
//  InvalidateRect(ClipRect);
end;

procedure TThItemCircleResizableSpot.DoMouseLeave;
begin
  inherited;

//  InvalidateRect(ClipRect);
  Repaint;
end;

function TThItemCircleResizableSpot.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result,
    ItemResizableSpotRadius + Canvas.StrokeThickness,
    ItemResizableSpotRadius + Canvas.StrokeThickness);
end;

procedure TThItemCircleResizableSpot.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FPressed then
  begin
    FMouseDownPos := PointF(X, Y);
  end;
end;

procedure TThItemCircleResizableSpot.MouseMove(Shift: TShiftState; X,
  Y: Single);
var
  Gap: TPointF;
begin
  inherited;

  if FPressed then
  begin
    Gap := PointF(X, Y).Subtract(FMouseDownPos);  // Down and Move Gap
    Position.Point := Position.Point.Add(Gap);

//    Debug('X: %f, Parent.X: %f, Down: %f, Curr: %f, Gap: %f, Pos: %f', [X, TControl(Parent).Position.X, FMouseDownPos.X, FMouseCurrPos.X, Gap.X, Position.X]);
    if Assigned(OnTrack) then
      OnTrack(Self, Gap.X, Gap.Y);
  end;
end;

procedure TThItemCircleResizableSpot.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TThItemCircleResizableSpot.Paint;
var
  R: TRectF;
begin
  inherited;

  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := $FF222222;
  if IsMouseOver then
    Canvas.Fill.Color := ItemResizableSpotOverColor
  else
    Canvas.Fill.Color := ItemResizableSpotOutColor;

  R := RectF(0, 0, 0, 0);
  InflateRect(R, ItemResizableSpotRadius, ItemResizableSpotRadius);
  Canvas.FillEllipse(R, 1);
  Canvas.DrawEllipse(R, 1);
end;

{ TItemFillResizabler }

function TThItemFillResizabler.GetResizablerRect: TRectF;
var
  I: Integer;
begin
  Result.Empty;
  for I := 0 to FList.Count - 1 do
    Result := UnionRect(Result, TThItemCircleResizableSpot(FList[I]).ClipRect);
end;

function TThItemCircleResizableSpot.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if (Abs(P.X) < ItemResizableSpotRadius) and (Abs(P.Y) < ItemResizableSpotRadius) then
    Result := True;
end;

end.
