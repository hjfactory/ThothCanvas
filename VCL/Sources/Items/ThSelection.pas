unit ThSelection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThUtils,
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
    FDirection: THandleDirection;
    FPoint: TFloatPoint;
//    FPoly: TThPoly;
    function GetPoly: TThPoly; overload;
  public
    constructor Create(ADirection: THandleDirection);
    property Direction: THandleDirection read FDirection;
    property Poly: TThPoly read GetPoly;// write FPoly;
    property Point: TFloatPoint read FPoint;

    function GetPoly(APoint: TFloatPoint): TThPoly; overload;
  end;

  TThItemSelection = class(TInterfacedObject, IThItemSelection)
  private
    FItem: TThDrawItem;
    FHotHandle: TThItemResizeHandle;

    function GetHandleAtPoint(APoint: TFloatPoint): TThItemResizeHandle;
    procedure SetHotHandle(const Value: TThItemResizeHandle);
    function IsOverHandle: Boolean;
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

    procedure MouseDown(const APoint: TFloatPoint); virtual;
    procedure MouseMove(const APoint: TFloatPoint); virtual;
    procedure MouseUp(const APoint: TFloatPoint); virtual;
    procedure MouseOver(const APoint: TFloatPoint); virtual;

    function PtInSelection(APoint: TFloatPoint): Boolean; virtual;
  public
    constructor Create(AParent: TThDrawItem);
    destructor Destroy; override;

    property HotHandle: TThItemResizeHandle read FHotHandle write SetHotHandle;
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    FLastPoint: TFloatPoint;

    function GetShape: TThShapeItem;
    property Shape: TThShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;

    procedure MouseDown(const APoint: TFloatPoint); override;
    procedure MouseMove(const APoint: TFloatPoint); override;
    procedure MouseUp(const APoint: TFloatPoint); override;
  end;

  TThShapeSelections = class(TList<TThShapeSelection>)
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
  HANDLE_RADIUS = 4;
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

function TThItemResizeHandle.GetPoly: TThPoly;
begin
  Result := GetPoly(FPoint);
end;

function TThItemResizeHandle.GetPoly(APoint: TFloatPoint): TThPoly;
begin
  Result := Circle(APoint, HANDLE_RADIUS);
end;

{ TThItemSelection }

constructor TThItemSelection.Create(AParent: TThDrawItem);
begin
  FFillColor := clWhite32;
  FHotColor := clRed32;
  FBorderColor := clBlack32;

  FRadius := HANDLE_RADIUS;
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
  P: TFloatPoint;
  H: TThItemResizeHandle;
  Poly: TThPoly;
begin
  for H in FHandles do
  begin
    P := H.Point.Scale(AScale).Offset(AOffset);
    Poly := H.GetPoly(P);
//    Poly :=
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

function TThItemSelection.IsOverHandle: Boolean;
begin
  Result := Assigned(FHotHandle);
end;

procedure TThItemSelection.MouseDown(const APoint: TFloatPoint);
begin
end;

procedure TThItemSelection.MouseMove(const APoint: TFloatPoint);
begin
end;

procedure TThItemSelection.MouseUp(const APoint: TFloatPoint);
begin
end;

procedure TThItemSelection.MouseOver(const APoint: TFloatPoint);
begin
  HotHandle := GetHandleAtPoint(APoint);
end;

{ TThShapeSelection }

function TThShapeSelection.GetShape: TThShapeItem;
begin
  Result := TThShapeItem(FItem);
end;

procedure TThShapeSelection.MouseDown(const APoint: TFloatPoint);
begin
  FLastPoint := APoint;
end;

procedure TThShapeSelection.MouseMove(const APoint: TFloatPoint);
var
  R: TFloatrect;
begin
  if not Assigned(FHotHandle) then
    Exit;

  R := Shape.Rect;
  case FHotHandle.Direction of
    rhdTopLeft:       R.TopLeft := APoint;
    rhdTop:           R.Top := APoint.Y;
    rhdTopRight:      R.TopRight := APoint;
    rhdRight:         R.Right := APoint.X;
    rhdBottomRight:   R.BottomRight := APoint;
    rhdBottom:        R.Bottom := APoint.Y;
    rhdBottomLeft:    R.BottomLeft := APoint;
    rhdLeft:          R.Left := APoint.X;
  end;
  Shape.ResizeItem(R);
end;

procedure TThShapeSelection.MouseUp(const APoint: TFloatPoint);
begin
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
begin
  for I := Low(FHandles) to High(FHandles) do
    FHandles[I].FPoint := HandlePoint(Shape.Rect, FHandles[I].Direction);
end;

end.
