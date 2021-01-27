{
  Role
    Store drawing datas.
}

unit ThDrawItem;

interface

uses
  System.Generics.Collections,
  GR32, GR32_Polygons, GR32_VectorUtils, clipper,
  ThTypes, ThUtils, ThClasses, ThDrawStyle;


type
  TThDrawItem = class;
  TThShapeDrawItem = class;
  TThDrawItemClass = class of TThDrawItem;

  TThDrawItems = class(TObjectList<TThDrawItem>)
  public
    // APoint에 포함되는 최상위 객체 반환
    function PtInItem(APoint: TFloatPoint): TThDrawItem;
    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;
  end;

  TThShapeDrawItems = class(TList<TThShapeDrawItem>)
  public
    // 모든 항목 APoint만큼 이동
    procedure Move(APoint: TFloatPoint);
  end;

  TThDrawItem = class(TThInterfacedObject, IThDrawItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
    function GetBounds: TFloatRect;
  protected
//    procedure AddPoint(APoint: TFloatPoint); virtual; abstract;
//    procedure SetStyle(AStyle: IThDrawStyle); virtual; abstract;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;

    property Bounds: TFloatRect read GetBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThBrushDrawItem = class(TThDrawItem)
  end;

  TThPenDrawItem = class(TThBrushDrawItem)
  private
//    FPath: TArray<TFloatPoint>;
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;

    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;

    FIsDeletion: Boolean;
    function GetColor: TColor32;
  public
    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly;
      AThickness: Integer; AColor: TColor32; AAlpha: Byte); overload;
    constructor Create(AStyle: IThDrawStyle); overload;
    destructor Destroy; override;

    procedure Start(APoint: TFloatPoint);
    procedure Move(APoint: TFloatPoint);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

//    property Path: TThPath read FPath;
    property Color: TColor32 read GetColor;
    property IsDeletion: Boolean read FIsDeletion write FIsDeletion default False;
  end;

  TThShapeDrawItem = class(TThDrawItem)
  private
    FIsSelection: Boolean;

    FRect: TFloatRect;

    FBorderWidth: Integer;
    FColor: TColor32;
    FBorderColor: TColor32;
  public
    constructor Create(ARect: TFloatRect; APoly: TThPoly; AColor: TColor32;
      ABorderWidth: Integer; ABorderColor: TColor32); overload;
    constructor Create(AStyle: IThDrawStyle); overload;

    procedure Start(APoint: TFloatPoint); virtual;
    procedure Move(APoint: TFloatPoint); virtual;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property IsSelection: Boolean read FIsSelection write FIsSelection;

    property Color: TColor32 read FColor;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BorderColor: TColor32 read FBorderColor write FBorderColor;
  end;

  TThRectDrawItem = class(TThShapeDrawItem)
  public
    procedure Move(APoint: TFloatPoint); override;
  end;

  TThRoundRectDrawItem = class(TThRectDrawItem)
  public
    procedure Move(APoint: TFloatPoint); override;
  end;

implementation

{ TThDrawItems }

function TThDrawItems.PtInItem(APoint: TFloatPoint): TThDrawItem;
var
  I: Integer;
  Item: TThDrawItem;
begin
  Result := nil;

  for I := Count - 1 downto 0 do
  begin
    Item := Items[I];
    if not PtInRect(Item.Bounds, APoint) then
      Continue;

    if not PtInPolyPolygon(APoint, Item.PolyPoly) then
      Continue;

    Exit(Item);
  end;
end;

function TThDrawItems.PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  PolyPath: TPath;
  ItemPaths, DestPaths: TPaths;
begin
  PolyRect := PolygonBounds(APoly);

  for I := 0 to Count - 1 do
  begin
    // Rect로 1차 교차 비교
    IntersectRect(DestRect, PolyRect, Items[I].Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    // Polygon 교차 비교(Clipper로 교차 영역 생성 후 비었는지 확인)
    PolyPath := AAFloatPoint2AAPoint(APoly);
    ItemPaths := AAFloatPoint2AAPoint(Items[I].PolyPoly);

    with TClipper.Create do
    try
      StrictlySimple := True;
      AddPaths(ItemPaths, ptSubject, True);
      AddPath(PolyPath, ptClip, True);

      Execute(ctIntersection, DestPaths, pftNonZero);
    finally
      Free;
    end;

    if Length(DestPaths) > 0 then
      Result := Result + [Items[I]];
  end;
end;

{ TThShapeDrawItems }

procedure TThShapeDrawItems.Move(APoint: TFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].FBounds := EmptyRect;
    TranslatePolyPolygonInplace(Items[I].FPolyPoly, APoint.X, APoint.Y);
  end;
end;

function TThDrawItem.GetBounds: TFloatRect;
begin
  if IsRectEmpty(FBounds) then
    FBounds := PolypolygonBounds(FPolyPoly);
  Result := FBounds;
end;

{ TThFreeDrawItem }

procedure TThPenDrawItem.Start(APoint: TFloatPoint);
var
  Poly: TThPoly;
begin
  FPath.Add(APoint);

  Poly := Circle(APoint, FThickness / 2);
  FPolyPoly := PolyPolygon(Poly);
  FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
end;

procedure TThPenDrawItem.Move(APoint: TFloatPoint);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
  Clipper: TClipper;
begin
  FPath.Add(APoint);

  LastP := FPath.Items[FPath.Count-2];
  Poly := BuildPolyline([LastP, APoint], FThickness, jsRound, esRound);
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

constructor TThPenDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly;
  AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FPath := TList<TFloatPoint>.Create;
  FPath.AddRange(APath);
  FPolyPoly := APolyPoly;

  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

constructor TThPenDrawItem.Create(AStyle: IThDrawStyle);
var
  Style: TThPenStyle;
begin
  Style := TThPenStyle(AStyle);
  Create(nil, nil, Style.Thickness, Style.Color, Style.Alpha);
end;

destructor TThPenDrawItem.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenDrawItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, Color);
end;

function TThPenDrawItem.GetColor: TColor32;
var
  LAlpha: Byte;
begin
  Result := FColor;
  LAlpha := FAlpha;
  if FIsDeletion then
    LAlpha := Round(LAlpha * 0.2);
  ModifyAlpha(Result, LAlpha);
end;

{ TThShapeDrawItem }

constructor TThShapeDrawItem.Create(ARect: TFloatRect; APoly: TThPoly;
  AColor: TColor32; ABorderWidth: Integer; ABorderColor: TColor32);
begin
  FRect := ARect;
  FPolyPoly := PolyPolygon(APoly);

  FColor := AColor;
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

constructor TThShapeDrawItem.Create(AStyle: IThDrawStyle);
var
  Style: TThShapeStyle;
begin
  Style := TThShapeStyle(AStyle);
  Create(EmptyRect, nil, Style.Color, Style.BorderWidth, Style.BorderColor);
end;

procedure TThShapeDrawItem.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  if FIsSelection then
    PolyPolygonFS(Bitmap, PolyPoly, clGray32)
  else
    PolyPolygonFS(Bitmap, PolyPoly, FColor);

  PolyPolylineFS(Bitmap, PolyPoly, FBorderColor, True, FBorderWidth);
end;

procedure TThShapeDrawItem.Start(APoint: TFloatPoint);
begin
  FRect.TopLeft := APoint;
end;

procedure TThShapeDrawItem.Move(APoint: TFloatPoint);
begin
  FRect.BottomRight := APoint;
end;

{ TThRectDrawItem }

procedure TThRectDrawItem.Move(APoint: TFloatPoint);
var
  Poly: TThPoly;
begin
  inherited;

  Poly := Rectangle(FRect);
  FPolyPoly := PolyPolygon(Poly);
end;

{ TThRoundRectDrawItem }

{ TThRoundRectDrawItem }

procedure TThRoundRectDrawItem.Move(APoint: TFloatPoint);
var
  Poly: TThPoly;
begin
  inherited;

  Poly := RoundRect(FRect, Abs(FRect.Bottom - FRect.Top) / 2);
  FPolyPoly := PolyPolygon(Poly);
end;

end.
