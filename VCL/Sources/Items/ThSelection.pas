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
  public
    constructor Create(AParent: TThDrawItem);
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure Realign;
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    function GetShape: TThShapeItem;
    property Shape: TThShapeItem read GetShape;
  protected
    procedure InitHandles; override;
    procedure RealignHandles; override;
  end;

implementation

uses
  GR32_Polygons,
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

  FRadius := 6;
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
  begin
    case D of
      rhdTopLeft:     Result := FloatPoint(R.Left, R.Top);
      rhdTop:         Result := FloatPoint((R.Left+R.Right)/2, R.Top);
      rhdTopRight:    Result := FloatPoint(R.Right, R.Top);
      rhdRight:       Result := FloatPoint(R.Right, (R.Top+R.Bottom)/2);
      rhdBottomRight: Result := FloatPoint(R.Right, R.Bottom);
      rhdBottom:      Result := FloatPoint((R.Left+R.Right)/2, R.Bottom);
      rhdBottomLeft:  Result := FloatPoint(R.Left, R.Bottom);
      rhdLeft:        Result := FloatPoint(R.Left, (R.Top+R.Bottom)/2);
    end;
  end;
var
  I: Integer;
  P: TFloatPoint;
begin
  for I := Low(FHandles) to High(FHandles) do
  begin
    P := HandlePt(Shape.Rect, FHandles[I].Direction);
    FHandles[I].Poly := Circle(P, FRadius);
  end;
end;

end.
