{
  Role
    - Show Mouse-Over Item Connection Handle(연결자 표시)
}
unit ThItemConnection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThItemHandle;

type
  TThItemAnchorPoint = TThShapeHandle;

  TThItemAnchorPoints = class(TThCustomItemHandles, IThItemConnection)
  protected
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    procedure CreateHandles; override;
    procedure RealignHandles; override;
  public
    constructor Create(AParent: IThItem); override;
  end;

implementation

uses
  ThItem;

constructor TThItemAnchorPoints.Create(AParent: IThItem);
begin
  inherited;

  FFillColor := clGray32;
  FHotColor := clRed32;
end;

procedure TThItemAnchorPoints.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  // Draw frame

  // Draw Anchor point
  DrawHandles(Bitmap, AScale, AOffset);
end;

procedure TThItemAnchorPoints.CreateHandles;
const
  ANCHOR_HANDLES: array[0..3] of TShapeHandleDirection = (
    shdTop,
    shdRight,
    shdBottom,
    shdLeft
  );
var
  I: Integer;
begin
  SetLength(FHandles, Length(ANCHOR_HANDLES));

  for I := Low(ANCHOR_HANDLES) to High(ANCHOR_HANDLES) do
    FHandles[I] := TThItemAnchorPoint.Create(ANCHOR_HANDLES[I], FRadius);
end;

procedure TThItemAnchorPoints.RealignHandles;
  function HandlePoint(R: TFloatRect; D: TShapeHandleDirection): TFloatPoint;
  var
    Center: TFloatPoint;
  begin
    Center.X := (R.Left+R.Right)/2;
    Center.Y := (R.Top+R.Bottom)/2;

    case D of
      shdTop:     Result := FloatPoint(Center.X,  R.Top);
      shdRight:   Result := FloatPoint(R.Right,   Center.Y);
      shdBottom:  Result := FloatPoint(Center.X,  R.Bottom);
      shdLeft:    Result := FloatPoint(R.Left,    Center.Y);
    end;
  end;
var
  I: Integer;
  Shape: TThFaceShapeItem;
begin
  Shape := TThFaceShapeItem(FParentItem);
  for I := Low(FHandles) to High(FHandles) do
    FHandles[I].Point := HandlePoint(Shape.Rect, TThItemAnchorPoint(FHandles[I]).Direction);
end;

end.
