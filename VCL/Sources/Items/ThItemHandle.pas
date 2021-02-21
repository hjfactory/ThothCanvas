unit ThItemHandle;

interface

uses
  GR32,
  ThTypes, ThDrawItem, ThUtils,
  System.UITypes,
  System.Generics.Collections;

type
  TThItemHandle = class
  private
    FRadius: Single;
    function GetCursor: TCursor; virtual;
  protected
    FPoint: TFloatPoint;
    function GetPoly: TThPoly; overload;
  public
    constructor Create;

    property Point: TFloatPoint read FPoint write FPoint;
    property Poly: TThPoly read GetPoly;
    property Cursor: TCursor read GetCursor;

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
    constructor Create(ADirection: TShapeHandleDirection); reintroduce;

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
    constructor Create(ADirection: TLineHandleDirection); reintroduce;

    property Direction: TLineHandleDirection read FDirection;
  end;

  TThCustomItemHandles = class(TInterfacedObject, IThItemHandles)
  private

    procedure SetHotHandle(const Value: TThItemHandle);

    function GetHandleAtPoint(APoint: TFloatPoint): TThItemHandle;
  protected
    FItem: TThDrawItem;

    FHandles: TArray<TThItemHandle>;
    FHotHandle: TThItemHandle;

    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    procedure DrawHandles(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure MouseDown(const APoint: TFloatPoint); virtual;
    procedure MouseMove(const APoint: TFloatPoint); virtual;
    procedure MouseUp(const APoint: TFloatPoint); virtual;
    procedure MouseOver(const APoint: TFloatPoint); virtual;

    function IsOverHandle: Boolean;

    procedure CreateHandles; virtual; abstract;
    procedure FreeHandles; virtual;
    procedure RealignHandles; virtual; abstract;
  public
    constructor Create(AParent: TThDrawItem);
    destructor Destroy; override;

    function PtInHandles(APoint: TFloatPoint): Boolean; virtual;

    property HotHandle: TThItemHandle read FHotHandle write SetHotHandle;
  end;

implementation

uses
  System.Math,
  Vcl.Forms,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

const
  DEF_HANDLE_RADIUS = 4;

{ TThItemHandle }

constructor TThItemHandle.Create;
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

function TThItemHandle.GetPoly: TThPoly;
begin
  Result := GetPoly(FPoint);
end;

{ TThShapeHandle }

constructor TThShapeHandle.Create(ADirection: TShapeHandleDirection);
begin
  inherited Create;

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

constructor TThLineHandle.Create(ADirection: TLineHandleDirection);
begin
  inherited Create;

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

constructor TThCustomItemHandles.Create(AParent: TThDrawItem);
begin
  FFillColor := clWhite32;
  FHotColor := clRed32;
  FBorderColor := clBlack32;

  FBorderWidth := 1;

  FItem := AParent;

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
  Poly: TThPoly;
begin
  for H in FHandles do
  begin
    P := H.Point.Scale(AScale).Offset(AOffset);
    Poly := H.GetPoly(P);
//    Poly := ScalePolygon(H.Poly, AScale.X, AScale.Y);
//    TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);
//    Poly := TranslatePolygon(H.Poly, AOffset.X, AOffset.Y);

    if H = FHotHandle then
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
    if PointInPolygon(APoint, H.Poly) then
      Exit(H);
end;

function TThCustomItemHandles.IsOverHandle: Boolean;
begin
  Result := Assigned(FHotHandle);
end;

procedure TThCustomItemHandles.MouseDown(const APoint: TFloatPoint);
begin

end;

procedure TThCustomItemHandles.MouseMove(const APoint: TFloatPoint);
begin

end;

procedure TThCustomItemHandles.MouseOver(const APoint: TFloatPoint);
begin

end;

procedure TThCustomItemHandles.MouseUp(const APoint: TFloatPoint);
begin
  HotHandle := GetHandleAtPoint(APoint);
end;

function TThCustomItemHandles.PtInHandles(APoint: TFloatPoint): Boolean;
begin
  HotHandle := GetHandleAtPoint(APoint);
  Result := Assigned(HotHandle);
end;

procedure TThCustomItemHandles.SetHotHandle(const Value: TThItemHandle);
begin
  FHotHandle := Value;

  if FHotHandle = nil then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := FHotHandle.Cursor;
end;

end.
