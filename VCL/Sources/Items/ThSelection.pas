unit ThSelection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes,
  ThDrawItem;

type
  THandleDirection = (
      rhdNone,

      rhdTopLeft,
      rhdTop,
      rhdTopRight,
      rhdRight,
      rhdBottomRight,
      rhdBottom,
      rhdBottomLeft,
      rhdLeft,

      rhdLineStart = 9,
      rhdLineEnd
    );

type
  TThItemResizeHandle = class
  private
    FRect: TFloatRect;
    FDirection: THandleDirection;
    FPoly: TThPoly;
  public
    constructor Create(ADirection: THandleDirection);
    property Direction: THandleDirection read FDirection;
    property Poly: TThPoly read FPoly write FPoly;
  end;

  TThItemSelection = class(TInterfacedObject, IThItemSelection)
  private
    FItem: TThDrawItem;
    FHotHandle: TThItemResizeHandle;
    function GetHandleAtPoint(APoint: TFloatPoint): TThItemResizeHandle;
    procedure SetHotHandle(const Value: TThItemResizeHandle);
    property HotHandle: TThItemResizeHandle read FHotHandle write SetHotHandle;
  protected

    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    FHandles: TArray<TThItemResizeHandle>;

    procedure CreateHandles; virtual; abstract;
    procedure FreeHandles; virtual;
    procedure RealignHandles; virtual; abstract;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure Realign;

    procedure MouseOver(const APoint: TFloatPoint); virtual;

    function PtInSelection(APoint: TFloatPoint): Boolean; virtual;
  public
    constructor Create(AParent: TThDrawItem);
    destructor Destroy; override;
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    function GetShape: TThShapeItem;
    property Shape: TThShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;
  end;

implementation

uses
  Vcl.Forms,
  System.Math,
  System.UITypes,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

const
  HANDLE_CURSOR: array [THandleDirection] of TCursor = (
    crDefault,
    crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE,         // TL, T, TR, R
    crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE,         // BR, B, BL, L
    crSizeNWSE, crSizeNWSE
  );

{ TThItemResizeHandle }

constructor TThItemResizeHandle.Create(ADirection: THandleDirection);
begin
  FDirection := ADirection;
end;

{ TThItemSelection }

constructor TThItemSelection.Create(AParent: TThDrawItem);
begin
  FFillColor := clWhite32;
  FHotColor := clRed32;
  FBorderColor := clBlack32;

  FRadius := 4;
  FBorderWidth := 1;

  FItem := AParent;

  CreateHandles;
  Realign;
end;

destructor TThItemSelection.Destroy;
begin
  FreeHandles;

  inherited;
end;

procedure TThItemSelection.FreeHandles;
var
  H: TThItemResizeHandle;
begin
  for H in FHandles do
    H.Free;
end;

procedure TThItemSelection.Realign;
begin
  RealignHandles;
end;

procedure TThItemSelection.SetHotHandle(const Value: TThItemResizeHandle);
begin
  FHotHandle := Value;

  if FHotHandle = nil then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := HANDLE_CURSOR[FHotHandle.Direction];
end;

procedure TThItemSelection.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  H: TThItemResizeHandle;
begin
  for H in FHandles do
  begin
    if H = FHotHandle then
      PolygonFS(Bitmap, H.Poly, FHotColor)
    else
      PolygonFS(Bitmap, H.Poly, FFillColor);
    PolylineFS(Bitmap, H.Poly, FBorderColor, True, FBorderWidth);
  end;
end;

procedure TThItemSelection.MouseOver(const APoint: TFloatPoint);
begin
//  FHotHandle := GetHandleAtPoint(APoint);
//  if Assigned(FHotHandle) then
//    Screen.Cursor := crSizeAll
//  else
//    Screen.Cursor := crDefault;
end;

function TThItemSelection.PtInSelection(APoint: TFloatPoint): Boolean;
begin
  HotHandle := GetHandleAtPoint(APoint);
  Result := Assigned(HotHandle);
end;

function TThItemSelection.GetHandleAtPoint(APoint: TFloatPoint): TThItemResizeHandle;
var
  H: TThItemResizeHandle;
begin
  Result := nil;
  for H in FHandles do
  begin
    if PointInPolygon(APoint, H.Poly) then
      Exit(H);
  end;
end;

{ TThShapeSelection }

function TThShapeSelection.GetShape: TThShapeItem;
begin
  Result := TThShapeItem(FItem);
end;

procedure TThShapeSelection.CreateHandles;
var
  I: Integer;
begin
  SetLength(FHandles, 8);

  for I := Ord(rhdTopLeft) to Ord(rhdLeft) do
    FHandles[I-Ord(rhdTopLeft)] := TThItemResizeHandle.Create(THandleDirection(I));
end;

procedure TThShapeSelection.RealignHandles;
  function HandlePoint(R: TFloatRect; D: THandleDirection): TFloatPoint;
  var
    CP: TFloatPoint;
  begin
    CP.X := (R.Left+R.Right)/2;
    CP.Y := (R.Top+R.Bottom)/2;

    case D of
      rhdTopLeft:     Result := FloatPoint(R.Left,  R.Top);
      rhdTop:         Result := FloatPoint(CP.X,    R.Top);
      rhdTopRight:    Result := FloatPoint(R.Right, R.Top);
      rhdRight:       Result := FloatPoint(R.Right, CP.Y);
      rhdBottomRight: Result := FloatPoint(R.Right, R.Bottom);
      rhdBottom:      Result := FloatPoint(CP.X,    R.Bottom);
      rhdBottomLeft:  Result := FloatPoint(R.Left,  R.Bottom);
      rhdLeft:        Result := FloatPoint(R.Left,  CP.Y);
    end;
  end;
var
  I: Integer;
  R: TFloatRect;
  P: TFloatPoint;
begin
  for I := Low(FHandles) to High(FHandles) do
  begin
    P := HandlePoint(Shape.Rect, FHandles[I].Direction);
    R := FloatRect(P, P);
    InflateRect(R, FRadius+1, FRadius+1);

    FHandles[I].Poly := Rectangle(R);
//    FHandles[I].Poly := Circle(P, FRadius);
//    FHandles[I].Rect := FloatRect(OffsetPoint(P, -FRadius, -FRadius), OffsetPoint(P, FRadius, FRadius));
  end;
end;

end.
