{
  Role
    -
}
unit ThItemSelection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThUtils, ThItemHandle,
  ThDrawItem;

type
  TThItemSelection = class(TThCustomItemHandles, IThItemSelection)
  protected
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
  public
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    function GetShape: TThFillShapeItem;
    property Shape: TThFillShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;

    procedure MouseMove(const APoint: TFloatPoint); override;
  end;

  TThLineSelection = class(TThItemSelection)
  private
    function GetShape: TThLineShapeItem;
    property Shape: TThLineShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;

    procedure MouseMove(const APoint: TFloatPoint); override;
  end;

implementation

uses
  Vcl.Forms,
  System.Math,
  System.UITypes,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

{ TThItemSelection }

procedure TThItemSelection.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  // Draw frame

  // Draw handles
  DrawHandles(Bitmap, AScale, AOffset);
end;

{ TThShapeSelection }

procedure TThShapeSelection.CreateHandles;
var
  I: Integer;
begin
  SetLength(FHandles, 8);

  for I := Ord(Low(TShapeHandleDirection)) to Ord(High(TShapeHandleDirection)) do
    FHandles[I] := TThShapeHandle.Create(TShapeHandleDirection(I));
end;

procedure TThShapeSelection.RealignHandles;
  function HandlePoint(R: TFloatRect; D: TShapeHandleDirection): TFloatPoint;
  var
    CP: TFloatPoint;
  begin
    CP.X := (R.Left+R.Right)/2;
    CP.Y := (R.Top+R.Bottom)/2;

    case D of
      shdTopLeft:     Result := FloatPoint(R.Left,  R.Top);
      shdTop:         Result := FloatPoint(CP.X,    R.Top);
      shdTopRight:    Result := FloatPoint(R.Right, R.Top);
      shdRight:       Result := FloatPoint(R.Right, CP.Y);
      shdBottomRight: Result := FloatPoint(R.Right, R.Bottom);
      shdBottom:      Result := FloatPoint(CP.X,    R.Bottom);
      shdBottomLeft:  Result := FloatPoint(R.Left,  R.Bottom);
      shdLeft:        Result := FloatPoint(R.Left,  CP.Y);
    end;
  end;
var
  I: Integer;
begin
  for I := Low(FHandles) to High(FHandles) do
    FHandles[I].Point := HandlePoint(Shape.Rect, TThShapeHandle(FHandles[I]).Direction);
end;

function TThShapeSelection.GetShape: TThFillShapeItem;
begin
  Result := TThFillShapeItem(FItem);
end;

procedure TThShapeSelection.MouseMove(const APoint: TFloatPoint);
var
  R: TFloatrect;
begin
  if not Assigned(FHotHandle) then
    Exit;

  R := Shape.Rect;
  case TThShapeHandle(FHotHandle).Direction of
    shdTopLeft:       R.TopLeft := APoint;
    shdTop:           R.Top := APoint.Y;
    shdTopRight:      R.TopRight := APoint;
    shdRight:         R.Right := APoint.X;
    shdBottomRight:   R.BottomRight := APoint;
    shdBottom:        R.Bottom := APoint.Y;
    shdBottomLeft:    R.BottomLeft := APoint;
    shdLeft:          R.Left := APoint.X;
  end;
  Shape.ResizeItem(R);
end;

{ TThLineSelection }

procedure TThLineSelection.CreateHandles;
begin
  SetLength(FHandles, 2);

  FHandles[0] := TThLineHandle.Create(shdLineFrom);
  FHandles[1] := TThLineHandle.Create(shdLineTo);
end;

procedure TThLineSelection.RealignHandles;
begin
  FHandles[0].Point := Shape.FromPoint;
  FHandles[1].Point := Shape.ToPoint;
end;

function TThLineSelection.GetShape: TThLineShapeItem;
begin
  Result := TThLineShapeItem(FItem);
end;

procedure TThLineSelection.MouseMove(const APoint: TFloatPoint);
begin
  if not Assigned(FHotHandle) then
    Exit;

  case TThLineHandle(FHotHandle).Direction of
    shdLineFrom:  Shape.ResizeItem(APoint, Shape.ToPoint);
    shdLineTo:    Shape.ResizeItem(Shape.FromPoint, APoint);
  end;
end;

end.
