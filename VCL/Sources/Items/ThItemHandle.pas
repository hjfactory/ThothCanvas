unit ThItemHandle;

interface

uses
  GR32,
  ThTypes, ThItem, ThUtils, ThClasses,
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
    procedure SetHotHandle(const Value: IThItemHandle);
    function GetHotHandle: IThItemHandle;

    function GetHandleAtPoint(APoint: TFloatPoint): TThItemHandle;
  protected
    FParentItem: TThItem;

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
  public
    constructor Create(AParent: TThItem);
    destructor Destroy; override;

    function PtInHandles(APoint: TFloatPoint): Boolean; virtual;

    property HotHandle: IThItemHandle read GetHotHandle write SetHotHandle;
    property HandleRadius: Single read FRadius;
  end;

implementation

uses
  Winapi.Windows,

  System.Math,
  Vcl.Forms,
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
  HANDLE_CURSOR: array [TShapeHandleDirection] of TCursor = (
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

constructor TThCustomItemHandles.Create(AParent: TThItem);
begin
  FFillColor := clWhite32;
  FHotColor := clRed32;
  FBorderColor := clBlack32;

  FRadius := DEF_HANDLE_RADIUS;
  FBorderWidth := 1;

  FParentItem := AParent;

  CreateHandles;
  RealignHandles;
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
  ItemHandle: TThItemHandle;
  Poly: TThPoly;
begin
  for H in FHandles do
  begin
//    ItemHandle := TThItemHandle(H);
    P := H.Point.Scale(AScale).Offset(AOffset);
    Poly := H.GetPoly(P);
//    Poly := ScalePolygon(H.Poly, AScale.X, AScale.Y);
//    TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);
//    Poly := TranslatePolygon(H.Poly, AOffset.X, AOffset.Y);

    if H = TThItemHandle(FHotHandle) then
      PolygonFS(Bitmap, Poly, FHotColor)
    else
      PolygonFS(Bitmap, Poly, FFillColor);
    PolylineFS(Bitmap, Poly, FBorderColor, True, FBorderWidth);
  end;
end;

procedure TThCustomItemHandles.FreeHandles;
var
  H: TThItemHandle;
begin
  for H in FHandles do
    H.Free;
end;

function TThCustomItemHandles.GetHandleAtPoint(
  APoint: TFloatPoint): TThItemHandle;
var
  H: TThItemHandle;
begin
  Result := nil;
  for H in FHandles do
  begin
    if PointInPolygon(APoint, H.Poly) then
      Exit(H);
  end;
end;

function TThCustomItemHandles.GetHotHandle: IThItemHandle;
begin
  Result := FHotHandle;
end;

procedure TThCustomItemHandles.MouseDown(const APoint: TFloatPoint);
begin
end;

procedure TThCustomItemHandles.MouseMove(const APoint: TFloatPoint);
begin
end;

procedure TThCustomItemHandles.MouseUp(const APoint: TFloatPoint);
begin
  HotHandle := GetHandleAtPoint(APoint);
end;

function TThCustomItemHandles.PtInHandles(APoint: TFloatPoint): Boolean;
begin
//  HotHandle := GetHandleAtPoint(APoint);
  Result := Assigned(GetHandleAtPoint(APoint));
end;

procedure TThCustomItemHandles.SetHotHandle(const Value: IThItemHandle);
begin
  FHotHandle := Value;

  if FHotHandle = nil then
  begin
    Screen.Cursor := crDefault;
    OutputDebugString(PChar('FHotHandle = nil'));
  end
  else
  begin
    Screen.Cursor := FHotHandle.Cursor;
    OutputDebugString(PChar('FHotHandle assigned'));
  end
end;

end.
