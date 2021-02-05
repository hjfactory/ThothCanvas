unit ThSelection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes,
  ThDrawItem;

type
  TResizeHandleDirection = (
      rhdTopLeft,
      rhdTop,
      rhdTopRight,
      rhdRight,
      rhdBottomRight,
      rhdBottom,
      rhdBottomLeft,
      rhdLeft,

      rhdLineStart,
      rhdLineEnd
    );

  TThItemSelection = class(TInterfacedObject, IThItemSelection)
  type
    TThItemResizeHandle = record
      Direction: TResizeHandleDirection;
      Poly: TThPoly;
      Rect: TFloatRect;
      procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    end;
  private
    FItem: TThDrawItem;
  protected
    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    FHandles: TArray<TThItemResizeHandle>;

    procedure InitHandles; virtual; abstract;
    procedure RealignHandles; virtual; abstract;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure Realign;

    procedure MouseOver(const APoint: TFloatPoint); virtual; abstract;
  public
    constructor Create(AParent: TThDrawItem);
    destructor Destroy; override;
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    function GetShape: TThShapeItem;
    property Shape: TThShapeItem read GetShape;
  protected
    procedure InitHandles; override;
    procedure RealignHandles; override;
    procedure MouseOver(const APoint: TFloatPoint); override;
  end;

implementation

uses
  Vcl.Forms,
  System.Math,
  System.UITypes,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

{ TThItemSelection.TThItemResizeHandle }

procedure TThItemSelection.TThItemResizeHandle.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
//  Self.Poly
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

  InitHandles;
  Realign;
end;

destructor TThItemSelection.Destroy;
begin

  inherited;
end;

procedure TThItemSelection.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  H: TThItemResizeHandle;
begin
  for H in FHandles do
  begin
    PolygonFS(Bitmap, H.Poly, FFillColor);
    PolylineFS(Bitmap, H.Poly, FBorderColor, True, FBorderWidth);
  end;
end;

procedure TThItemSelection.Realign;
begin
  RealignHandles;
end;

{ TThShapeSelection }

function TThShapeSelection.GetShape: TThShapeItem;
begin
  Result := TThShapeItem(FItem);
end;

procedure TThShapeSelection.InitHandles;
var
  I: Integer;
begin
  SetLength(FHandles, 8);

  for I := Low(FHandles) to High(FHandles) do
    FHandles[I].Direction := TResizeHandleDirection(I);
end;

procedure TThShapeSelection.RealignHandles;
  function HandlePt(R: TFloatRect; D: TResizeHandleDirection): TFloatPoint;
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
    P := HandlePt(Shape.Rect, FHandles[I].Direction);
    FHandles[I].Rect := FloatRect(P, P);
    InflateRect(FHandles[I].Rect, FRadius+1, FRadius+1);
    FHandles[I].Poly := Rectangle(FHandles[I].Rect);
//    FHandles[I].Poly := Circle(P, FRadius);
//    FHandles[I].Rect := FloatRect(OffsetPoint(P, -FRadius, -FRadius), OffsetPoint(P, FRadius, FRadius));
  end;
end;

procedure TThShapeSelection.MouseOver(const APoint: TFloatPoint);
var
  H: TThItemResizeHandle;
  Poly: TThPoly;
begin
  for H in FHandles do
  begin
    Poly := H.Poly;
    if PointInPolygon(APoint, H.Poly) then
    begin
      Screen.Cursor := crSizeAll;
      Exit;
    end;
  end;
end;

end.
