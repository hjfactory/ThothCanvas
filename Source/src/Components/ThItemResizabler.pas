unit ThItemResizabler;

interface

uses
  ThItemResizablerIF, System.Classes, System.Types, FMX.Types, System.UITypes,
  System.Generics.Collections;

type
  TItemResizableSpot = class(TControl, IItemResizableSpot)
  private
    FDirection: TResizableSpotDirection;
  public
    constructor Create(AOwner: TComponent; ADirection: TResizableSpotDirection); reintroduce; virtual;

    property Direction: TResizableSpotDirection read FDirection;
  end;

  TResizableSpotClass = class of TItemResizableSpot;

  TItemResizabler = class(TInterfacedObject, IItemResizabler)
  private
    FSpotClass: TResizableSpotClass;
    FList: TList;
    FOnTracking: TNotifyEvent;
    function GetSpots(Index: Integer): TItemResizableSpot;
    function GetCount: Integer;
  protected
    FParent: TControl;
    function GetResizablerRect: TRectF; virtual; abstract;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure SetSpotClass(ASpotClass: TResizableSpotClass);
    procedure SetResizableSpots(Spots: array of TResizableSpotDirection);

    property Spots[Index: Integer] : TItemResizableSpot read GetSpots; default;
    property Count: Integer read GetCount;
    property OnTrack: TNotifyEvent read FOnTracking write FOnTracking;
  end;

  TThItemCircleResizableSpot = class(TItemResizableSpot)
  protected
    procedure Paint; override;
    function GetUpdateRect: TRectF; override;
//    function GetClipRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent; ADirection: TResizableSpotDirection); override;
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
  ThConsts, System.UIConsts;


{ TItemResizableSpot }

constructor TItemResizableSpot.Create(AOwner: TComponent;
  ADirection: TResizableSpotDirection);
begin
  inherited Create(AOwner);

  Opacity := 1;
  FDirection := ADirection;
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

function TItemResizabler.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TItemResizabler.GetSpots(Index: Integer): TItemResizableSpot;
begin
  Result := FList[Index];
end;

procedure TItemResizabler.SetResizableSpots(
  Spots: array of TResizableSpotDirection);
var
  I: Integer;
  Spot: TItemResizableSpot;
begin
  assert(Assigned(FSpotClass), 'Not assigned items Spot class');

  for I := 0 to Length(Spots) - 1 do
  begin
    Spot := FSpotClass.Create(FParent, Spots[I]);
    Spot.Parent := FParent;
    Spot.Visible := False;
    FList.Add(Spot);
  end;
end;

procedure TItemResizabler.SetSpotClass(ASpotClass: TResizableSpotClass);
begin
  FSpotClass := ASpotClass;
end;

{ TThItemCircleResizableSpot }

constructor TThItemCircleResizableSpot.Create(AOwner: TComponent; ADirection: TResizableSpotDirection);
begin
  inherited;

  AutoCapture := True;

  Width := ITEM_RESIZABLESPOT_SIZE * 2;
  Height := ITEM_RESIZABLESPOT_SIZE * 2;
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
  InflateRect(Result, ITEM_RESIZABLESPOT_SIZE + 1, ITEM_RESIZABLESPOT_SIZE + 1);
end;

procedure TThItemCircleResizableSpot.MouseMove(Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
//  R: TRectF;
begin
  inherited;

  if FPressed and (Parent <> nil) and (Parent is TControl) then
  begin
    P := LocalToAbsolute(PointF(X, Y));
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);

//        P.X := Min(P.X, Canvas.Width);
//        P.Y := Min(P.Y, Canvas.Height);

    Position.Point := P;

//    if Assigned(FOnTrack) then
//      FOnTrack(Self);
  end;
end;

procedure TThItemCircleResizableSpot.Paint;
var
  R: TRectF;
begin
  inherited;

  Canvas.StrokeThickness := 0;
  Canvas.Stroke.Color := $FF222222;
  if IsMouseOver then
    Canvas.Fill.Color := ITEM_RESIZABLESPOT_OVERCOLOR
  else
    Canvas.Fill.Color := ITEM_RESIZABLESPOT_OUTCOLOR;

  R.Empty;
  InflateRect(R, ITEM_RESIZABLESPOT_SIZE, ITEM_RESIZABLESPOT_SIZE);
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
  if (Abs(P.X) < (ITEM_RESIZABLESPOT_SIZE)) and (Abs(P.Y) < (ITEM_RESIZABLESPOT_SIZE)) then
    Result := True;
end;

end.
