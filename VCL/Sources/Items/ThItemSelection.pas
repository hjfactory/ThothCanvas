{
  Role
    -
}
unit ThItemSelection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThUtils,
  ThItem, ThItemHandle;

type
  TThItemSelection = class(TThCustomItemHandles, IThItemSelection)
  protected
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure ResizeItem(const APoint: TFloatPoint); virtual; abstract;
  public
    function IsResizing: Boolean;
  end;

  TThShapeSelection = class(TThItemSelection)
  private
    function GetShape: TThFillShapeItem;
    property Shape: TThFillShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;

    procedure ResizeItem(const APoint: TFloatPoint); override;
  end;

  TThLineSelection = class(TThItemSelection)
  private
    function GetShape: TThLineShapeItem;
    property Shape: TThLineShapeItem read GetShape;
  protected
    procedure CreateHandles; override;
    procedure RealignHandles; override;

    procedure ResizeItem(const APoint: TFloatPoint); override;
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

function TThItemSelection.IsResizing: Boolean;
begin
  Result := Assigned(FHotHandle) and FMouseDowned;
end;

{ TThShapeSelection }

procedure TThShapeSelection.CreateHandles;
var
  I: Integer;
begin
  SetLength(FHandles, 8);

  for I := Ord(Low(TShapeHandleDirection)) to Ord(High(TShapeHandleDirection)) do
    FHandles[I] := TThShapeHandle.Create(TShapeHandleDirection(I), FRadius);
end;

procedure TThShapeSelection.RealignHandles;
  function HandlePoint(R: TFloatRect; D: TShapeHandleDirection): TFloatPoint;
  var
    Center: TFloatPoint;
  begin
    Center.X := (R.Left+R.Right)/2;
    Center.Y := (R.Top+R.Bottom)/2;

    case D of
      shdTopLeft:     Result := FloatPoint(R.Left,    R.Top);
      shdTop:         Result := FloatPoint(Center.X,  R.Top);
      shdTopRight:    Result := FloatPoint(R.Right,   R.Top);
      shdRight:       Result := FloatPoint(R.Right,   Center.Y);
      shdBottomRight: Result := FloatPoint(R.Right,   R.Bottom);
      shdBottom:      Result := FloatPoint(Center.X,  R.Bottom);
      shdBottomLeft:  Result := FloatPoint(R.Left,    R.Bottom);
      shdLeft:        Result := FloatPoint(R.Left,    Center.Y);
    end;
  end;
var
  I: Integer;
begin
  for I := Low(FHandles) to High(FHandles) do
    TThItemHandle(FHandles[I]).Point := HandlePoint(Shape.Rect, TThShapeHandle(FHandles[I]).Direction);
end;

function TThShapeSelection.GetShape: TThFillShapeItem;
begin
  Result := TThFillShapeItem(FParentItem);
end;

procedure TThShapeSelection.ResizeItem(const APoint: TFloatPoint);
var
  R: TFloatrect;
begin
  if not Assigned(FHotHandle) then
    Exit;

  if not FMouseDowned then
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

  FHandles[0] := TThLineHandle.Create(shdLineFrom, FRadius);
  FHandles[1] := TThLineHandle.Create(shdLineTo, FRadius);
end;

procedure TThLineSelection.RealignHandles;
begin
  FHandles[0].Point := Shape.FromPoint;
  FHandles[1].Point := Shape.ToPoint;
end;

procedure TThLineSelection.ResizeItem(const APoint: TFloatPoint);
begin
  if not Assigned(FHotHandle) then
    Exit;

  case TThLineHandle(FHotHandle).Direction of
    shdLineFrom:  Shape.ResizeItem(APoint, Shape.ToPoint);
    shdLineTo:    Shape.ResizeItem(Shape.FromPoint, APoint);
  end;
end;

function TThLineSelection.GetShape: TThLineShapeItem;
begin
  Result := TThLineShapeItem(FParentItem);
end;

end.
