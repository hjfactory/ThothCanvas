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
    constructor Create(AOwner: TComponent; ADirection: TResizableSpotDirection);

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
    constructor Create(AOwner: TComponent; ASpotClass: TResizableSpotClass);
    destructor Destroy; override;
    procedure SetResizableSpots(Spots: array of TResizableSpotDirection);

    property Spots[Index: Integer] : TItemResizableSpot read GetSpots; default;
    property Count: Integer read GetCount;
    property OnTrack: TNotifyEvent read FOnTracking write FOnTracking;
  end;

  TThItemCircleResizableSpot = class(TItemResizableSpot)
  protected
    procedure Paint; override;
    function GetUpdateRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
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

constructor TItemResizabler.Create(AOwner: TComponent; ASpotClass: TResizableSpotClass);
begin
  FParent := TControl(AOwner);
  FSpotClass := ASpotClass;
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
  for I := 0 to Length(Spots) - 1 do
  begin
    Spot := FSpotClass.Create(FParent, Spots[I]);
    Spot.Parent := FParent;
    Spot.Visible := False;
    FList.Add(Spot);
  end;
end;

{ TThItemCircleResizableSpot }

procedure TThItemCircleResizableSpot.DoMouseEnter;
begin
  inherited;

  Repaint;
//  InvalidateRect(ClipRect);
end;

procedure TThItemCircleResizableSpot.DoMouseLeave;
begin
  inherited;

  Repaint;
//  InvalidateRect(ClipRect);
end;

function TThItemCircleResizableSpot.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result, ITEM_RESIZABLESPOT_SIZE + 1, ITEM_RESIZABLESPOT_SIZE + 1);
end;

procedure TThItemCircleResizableSpot.Paint;
var
  R: TRectF;
begin
  inherited;

  Canvas.StrokeThickness := 1;
//  Canvas.Stroke.Assign(FStroke);
  if IsMouseOver then
    Canvas.Fill.Color := ITEM_RESIZABLESPOT_OVERCOLOR
  else
    Canvas.Fill.Color := ITEM_RESIZABLESPOT_OUTCOLOR;

  R.Empty;
  InflateRect(R, ITEM_RESIZABLESPOT_SIZE, ITEM_RESIZABLESPOT_SIZE);
  Canvas.FillEllipse(R, AbsoluteOpacity);
  Canvas.DrawEllipse(R, AbsoluteOpacity);
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
  if (Abs(P.X) < (ITEM_RESIZABLESPOT_SIZE/2)) and (Abs(P.Y) < (ITEM_RESIZABLESPOT_SIZE/2)) then
    Result := True;
end;

end.
