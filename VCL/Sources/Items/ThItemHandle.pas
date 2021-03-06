unit ThItemHandle;

interface

uses
  GR32,
  ThTypes, ThUtils, ThClasses,
  System.UITypes,
  System.Generics.Collections;

type
  TThItemHandle = class(TThInterfacedObject, IThItemHandle)
  private
    FRadius: Single;
    function GetCursor: TCursor; virtual;
    procedure SetPoint(const Value: TFloatPoint);
  protected
    FPoint: TFloatPoint;
    function GetPoly: TThPoly; overload;
  public
    constructor Create(ARadius: Single);

    property Point: TFloatPoint read FPoint write SetPoint;
    property Poly: TThPoly read GetPoly;
    property Cursor: TCursor read GetCursor;
    property Radius: Single read FRadius;

    function GetPoly(APoint: TFloatPoint): TThPoly; overload; virtual;
  end;

  TShapeHandleDirection = (
    shdTopLeft,
    shdTop,
    shdTopRight,
    shdRight,
    shdBottomRight,
    shdBottom,
    shdBottomLeft,
    shdLeft
  );
  TThShapeHandle = class(TThItemHandle)
  private
    FDirection: TShapeHandleDirection;
    function GetCursor: TCursor; override;
  public
    constructor Create(ADirection: TShapeHandleDirection; ARadius: Single); reintroduce;

    property Direction: TShapeHandleDirection read FDirection;
  end;

  TLineHandleDirection = (
    shdLineFrom,
    shdLineTo
  );
  TThLineHandle = class(TThItemHandle)
  private
    FDirection: TLineHandleDirection;
    function GetCursor: TCursor; override;
  public
    constructor Create(ADirection: TLineHandleDirection; ARadius: Single); reintroduce;

    property Direction: TLineHandleDirection read FDirection;
  end;

  TThCustomItemHandles = class(TInterfacedObject, IThItemHandles)
  private
    FVisible: Boolean;

    procedure SetHotHandle(const Value: IThItemHandle);
    function GetHotHandle: IThItemHandle;

    function GetHandleAtPoint(APoint: TFloatPoint): TThItemHandle;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  protected
    FParentItem: IThItem;

    FMouseDowned: Boolean;

    FHandles: TArray<TThItemHandle>;
    FHotHandle: IThItemHandle;

    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    procedure DrawHandles(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure MouseDown(const APoint: TFloatPoint); virtual;
    procedure MouseMove(const APoint: TFloatPoint); virtual;
    procedure MouseUp(const APoint: TFloatPoint); virtual;

    procedure CreateHandles; virtual; abstract;
    procedure FreeHandles; virtual;
    procedure RealignHandles; virtual; abstract;
    procedure ReleaseHotHandle;
  public
    constructor Create(AParent: IThItem); virtual;
    destructor Destroy; override;

    function PtInHandles(APoint: TFloatPoint): Boolean; virtual;

    property HandleRadius: Single read FRadius;

    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

uses
  Winapi.Windows,
  System.Math,
  Vcl.Forms,
  ThItem,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

const
  DEF_HANDLE_RADIUS = 4;

{ TThItemHandle }

constructor TThItemHandle.Create(ARadius: Single);
begin
  FRadius := DEF_HANDLE_RADIUS;
end;

function TThItemHandle.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TThItemHandle.GetPoly(APoint: TFloatPoint): TThPoly;
begin
  Result := Circle(APoint, FRadius);
end;

procedure TThItemHandle.SetPoint(const Value: TFloatPoint);
begin
  FPoint := Value;
end;

function TThItemHandle.GetPoly: TThPoly;
begin
  Result := GetPoly(FPoint);
end;

{ TThShapeHandle }

constructor TThShapeHandle.Create(ADirection: TShapeHandleDirection; ARadius: Single);
begin
  inherited Create(ARadius);

  FDirection := ADirection;
end;

function TThShapeHandle.GetCursor: TCursor;
const
  HANDLE_CURSOR: array[TShapeHandleDirection] of TCursor = (
    crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE,         // TL, T, TR, R
    crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE          // BR, B, BL, L
  );
begin
  Result := HANDLE_CURSOR[FDirection];
end;

{ TThLineHandle }

constructor TThLineHandle.Create(ADirection: TLineHandleDirection; ARadius: Single);
begin
  inherited Create(ARadius);

  FDirection := ADirection;
end;

function TThLineHandle.GetCursor: TCursor;
const
  HANDLE_CURSOR: array [TLineHandleDirection] of TCursor = (
    crSizeNWSE, // From
    crSizeNWSE  // To
  );
begin
  Result := HANDLE_CURSOR[FDirection];
end;

{ TThCustomItemHandles }

constructor TThCustomItemHandles.Create(AParent: IThItem);
begin
  FFillColor := clWhite32;
  FHotColor := clRed32;
  FBorderColor := clBlack32;

  FRadius := DEF_HANDLE_RADIUS;
  FBorderWidth := 1;

  FParentItem := AParent;

  FMouseDowned := False;

  CreateHandles;
  RealignHandles;

  FHotHandle := nil;
end;

destructor TThCustomItemHandles.Destroy;
begin
  FreeHandles;

  inherited;
end;

procedure TThCustomItemHandles.DrawHandles(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  P: TFloatPoint;
  H: TThItemHandle;
  Poly: TThPoly;
begin
  for H in FHandles do
  begin
    P := H.Point.Scale(AScale).Offset(AOffset);
    Poly := H.GetPoly(P);

    if H = TThItemHandle(FHotHandle) then
      PolygonFS(Bitmap, Poly, FHotColor)
    else
      PolygonFS(Bitmap, Poly, FFillColor);
    PolylineFS(Bitmap, Poly, FBorderColor, True, FBorderWidth);
  end;
end;

function TThCustomItemHandles.GetHandleAtPoint(
  APoint: TFloatPoint): TThItemHandle;
var
  H: TThItemHandle;
begin
  Result := nil;
  for H in FHandles do
  begin
    if PtInCircle(APoint, H.Point, H.Radius * 2) then
//    if PointInPolygon(APoint, H.Poly) then
      Exit(H);
  end;
end;

function TThCustomItemHandles.PtInHandles(APoint: TFloatPoint): Boolean;
begin
  Result := Assigned(GetHandleAtPoint(APoint));
end;

function TThCustomItemHandles.GetHotHandle: IThItemHandle;
begin
  Result := FHotHandle;
end;

function TThCustomItemHandles.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TThCustomItemHandles.MouseDown(const APoint: TFloatPoint);
begin
  FMouseDowned := True;
end;

procedure TThCustomItemHandles.MouseMove(const APoint: TFloatPoint);
begin
  if not FMouseDowned then
    SetHotHandle(GetHandleAtPoint(APoint));
end;

procedure TThCustomItemHandles.MouseUp(const APoint: TFloatPoint);
begin
  SetHotHandle(GetHandleAtPoint(APoint));
  FMouseDowned := False;
end;

procedure TThCustomItemHandles.FreeHandles;
var
  H: TThItemHandle;
begin
  for H in FHandles do
    H.Free;
end;

procedure TThCustomItemHandles.ReleaseHotHandle;
begin
  FMouseDowned := False;
  SetHotHandle(nil);
end;

procedure TThCustomItemHandles.SetHotHandle(const Value: IThItemHandle);
begin
  if FHotHandle = Value then
    Exit;

  FHotHandle := Value;

  if FHotHandle = nil then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := FHotHandle.Cursor;
end;

procedure TThCustomItemHandles.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

end.
