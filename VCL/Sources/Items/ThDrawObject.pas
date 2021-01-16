unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes,
  ThDrawStyle,
  ThDrawItem;

type
  TThDrawObject = class(TInterfacedObject, IThDrawObject)
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure Move(const X, Y: TFloat); overload; virtual;
    procedure Move(const APoint: TFloatPoint); overload; virtual; abstract;

    function CreateItem: TObject; virtual;
    procedure Clear; virtual;
  end;

  /// Brush Objects
  TThBrushDrawObject = class(TThDrawObject)
  private
    FDrawStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;
    destructor Destroy; override;
    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  TThPenDrawObject = class(TThBrushDrawObject)
  private
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
    function GetPenStyle: TThPenStyle;

    property PenStyle: TThPenStyle read GetPenStyle;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure Move(const APoint: TFloatPoint); override;
    function CreateItem: TObject; override;
    procedure Clear; override;
  end;

  TThEraserDrawObject = class(TThBrushDrawObject)
  private
    FDrawItems: TThDrawItems;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThDrawItems);
  end;

  TThObjErsDrawObject = class(TThEraserDrawObject)
  public
    procedure Move(const APoint: TFloatPoint); override;
    procedure Clear; override;
  end;

  /// Shape Objects
  TThShapeDrawObject = class(TThDrawObject)
  end;

implementation

uses
  ThGraphicsUtils;

{ TThDrawObject }

procedure TThDrawObject.Move(const X, Y: TFloat);
begin
  Move(FloatPoint(X, Y));
end;

procedure TThDrawObject.Clear;
begin
end;

function TThDrawObject.CreateItem: TObject;
begin
  Result := nil;
end;

procedure TThDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
end;

{ TThBrushDrawObject }

constructor TThBrushDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

{ TThPenObject }

constructor TThPenDrawObject.Create(AStyle: IThDrawStyle);
begin
  inherited Create(AStyle);

  FPath := TList<TFloatPoint>.Create;
end;

destructor TThPenDrawObject.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Color: TColor32;
  PolyPoly: TThPolyPoly;
begin
  Color := PenStyle.Color;
  ModifyALpha(Color, PenStyle.Alpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, Color);
end;

procedure TThPenDrawObject.Move(const APoint: TFloatPoint);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
  Thickness: Integer;
begin
  Thickness := 12;

  FPath.Add(APoint);

  if FPath.Count = 1 then
  begin
    Poly := Circle(APoint, Thickness / 2);
    FPolyPoly := PolyPolygon(Poly);
    FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
  end
  else
  begin
    LastP := FPath.Items[FPath.Count-2];
    Poly := BuildPolyline([LastP, APoint], Thickness, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly, 3);

    with TClipper.Create do
    try
      AddPaths(FPolyPolyPath, ptSubject, True);
      AddPath(PolyPath, ptClip, True);

      Execute(ctUnion, FPolyPolyPath, pftNonZero);
    finally
      Free;
    end;

    FPolyPoly := AAPoint2AAFloatPoint(FPolyPolyPath, 3);
  end;
end;

procedure TThPenDrawObject.Clear;
begin
  inherited;

  FPath.Clear;
  FPolyPolyPath := nil;
  FPolyPoly := nil;
end;

function TThPenDrawObject.CreateItem: TObject;
begin
  Result := TThPenDrawItem.Create(FPath.ToArray, FPolyPoly,
    PenStyle.Thickness, PenStyle.Color, PenStyle.Alpha);
end;

function TThPenDrawObject.GetPenStyle: TThPenStyle;
begin
  Result := TThPenStyle(FDrawStyle);
end;

destructor TThBrushDrawObject.Destroy;
begin

  inherited;
end;

{ TThEraserDrawObject }

constructor TThEraserDrawObject.Create(AStyle: IThDrawStyle;
  AItems: TThDrawItems);
begin
  inherited Create(AStyle);

  FDrawItems := AItems;
end;

function TThEraserDrawObject.GetDrawStyle: TThEraserStyle;
begin
  Result := TThEraserStyle(FDrawStyle);
end;

{ TThObjErsDrawObject }

procedure TThObjErsDrawObject.Move(const APoint: TFloatPoint);
var
  Item: TThDrawItem;
  Poly: TThPoly;
  PolyRect, DestRect: TFloatRect;
  EraserPath: TPath;
  ItemPaths, DestPaths: TPaths;
begin
  Poly := Circle(APoint, DrawStyle.Thickness / 2);
  PolyRect := PolygonBounds(Poly);

  for Item in FDrawItems do
  begin
    IntersectRect(DestRect, PolyRect, Item.Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    EraserPath := AAFloatPoint2AAPoint(Poly);
    ItemPaths := AAFloatPoint2AAPoint(Item.PolyPoly);
    with TClipper.Create do
    begin
      StrictlySimple := True;
      AddPaths(ItemPaths, ptSubject, True);
      AddPath(EraserPath, ptClip, True);

      Execute(ctIntersection, DestPaths, pftNonZero);
    end;

    if Length(DestPaths) > 0 then
      TThPenDrawItem(Item).IsDeletion := True;
  end;
end;

procedure TThObjErsDrawObject.Clear;
var
  I: Integer;
  Item: TThDrawItem;
begin
  for I := FDrawItems.Count - 1 downto 0 do
  begin
    Item := FDrawItems[I];
    if TThPenDrawItem(Item).IsDeletion then
      FDrawItems.Delete(I);
  end;
end;

end.
