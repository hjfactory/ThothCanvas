{
  Role
    Store drawing datas.
}

unit ThDrawItem;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThClasses;

type
  TThDrawItem = class;

  TThDrawItem = class(TThInterfacedObject, IThDrawItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
  protected
    procedure Realign;
    procedure DoRealign; virtual;
  public
    constructor Create; virtual;
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;

    function PtInItem(APt: TFloatPoint): Boolean; virtual;

    property Bounds: TFloatRect read FBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThBrushDrawItem = class(TThDrawItem)
  end;

  TThPenDrawItem = class(TThBrushDrawItem)
  private
    FPath: TThPath;

    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;

    FIsDeletion: Boolean;
    function GetColor: TColor32;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetStyle(APath: TThPath; APolyPoly: TThPolyPoly;
      AThickness: Integer; AColor: TColor32; AAlpha: Byte); overload;
    procedure SetStyle(AStyle: IThDrawStyle); overload;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
    procedure DrawPoly(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint; APath: TThPath; APolyPoly: TThPolyPoly); virtual;

    property IsDeletion: Boolean read FIsDeletion write FIsDeletion default False;

    property Path: TThPath read FPath;
    property Thickness: Single read FThickness;
    property Color: TColor32 read GetColor;
    property Alpha: Byte read FAlpha;
  end;

  TThShapeItem = class(TThDrawItem)
  private
    FSelected: Boolean;
    FSelection: IThItemSelection;

    FRect: TFloatRect;
    FBorderWidth: Integer;
    FColor: TColor32;

    FBorderColor: TColor32;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure DoRealign; override;
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; virtual; abstract;
  public
    procedure SetStyle(ARect: TFloatRect; APoly: TThPoly; AColor: TColor32;
      ABorderWidth: Integer; ABorderColor: TColor32); overload;
    procedure SetStyle(AStyle: IThDrawStyle); overload;

    procedure DrawRect(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint; ARect: TFloatRect); virtual;
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure MoveItem(APoint: TFloatPoint);
    procedure ResizeItem(ARect: TFloatRect);

    function PtInItem(APt: TFloatPoint): Boolean; override;

    property Selected: Boolean read FSelected write SetSelected;
    property Selection: IThItemSelection read FSelection;

    property Rect: TFloatRect read FRect;
    property Color: TColor32 read FColor;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BorderColor: TColor32 read FBorderColor write FBorderColor;
  end;

  TThDrawItems = class(TObjectList<TThDrawItem>)
  public
    // APoint에 포함되는 최상위 객체 반환
    function PtInItem(APoint: TFloatPoint): TThDrawItem;
    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;

    procedure MouseOver(APoint: TFLoatPoint);
  end;

  TThSelectedItems = class(TList<TThShapeItem>)
  public
    // 모든 항목 APoint만큼 이동
    procedure MoveItem(APoint: TFloatPoint);
  end;

implementation

uses
  System.Math,
  GR32_Polygons, GR32_VectorUtils, GR32_Clipper,
  ThUtils, ThDrawStyle, ThSelection;

{ TThDrawItem }

constructor TThDrawItem.Create;
begin
end;

procedure TThDrawItem.DoRealign;
begin
end;

function TThDrawItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  Result := PtInRect(FBounds, APt) and PtInPolyPolygon(APt, FPolyPoly);
end;

procedure TThDrawItem.Realign;
begin
  DoRealign;
  FBounds := PolypolygonBounds(FPolyPoly);
end;

{ TThFreeDrawItem }

constructor TThPenDrawItem.Create;
begin
end;

procedure TThPenDrawItem.SetStyle(APath: TThPath; APolyPoly: TThPolyPoly;
  AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;

  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

procedure TThPenDrawItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThPenStyle;
begin
  Style := TThPenStyle(AStyle);
  SetStyle(nil, nil, Style.Thickness, Style.Color, Style.Alpha);
end;

destructor TThPenDrawItem.Destroy;
begin
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

procedure TThPenDrawItem.DrawPoly(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint; APath: TThPath; APolyPoly: TThPolyPoly);
begin
  FPolyPoly := APolyPoly;
  Realign;

  Draw(Bitmap, AScale, AOffset);
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

procedure TThShapeItem.SetStyle(ARect: TFloatRect; APoly: TThPoly;
  AColor: TColor32; ABorderWidth: Integer; ABorderColor: TColor32);
begin
  FRect := ARect;
  FPolyPoly := PolyPolygon(APoly);

  FColor := AColor;
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThShapeItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThShapeStyle;
begin
  Style := TThShapeStyle(AStyle);
  SetStyle(EmptyRect, nil, Style.Color, Style.BorderWidth, Style.BorderColor);
end;

procedure TThShapeItem.DoRealign;
begin
  inherited;

  FRect.Realign;
//  FRect.Width   := Max(FRect.Width, FMinimunSize.X);
//  FRect.Height  := Max(FRect.Height, FMinimunSize.Y);

  FPolyPoly := RectToPolyPoly(FRect);

  if Assigned(FSelection) then
    FSelection.Realign;
end;

procedure TThShapeItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, FColor);

  PolyPolylineFS(Bitmap, PolyPoly, FBorderColor, True, FBorderWidth);

  if FSelected and Assigned(FSelection) then
    FSelection.Draw(Bitmap, AScale, AOffset);
end;

procedure TThShapeItem.DrawRect(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
  ARect: TFloatRect);
begin
  FRect := ARect;
  Realign;

  Draw(Bitmap, AScale, AOffset);
end;

procedure TThShapeItem.MoveItem(APoint: TFloatPoint);
begin
  FRect := OffsetRect(FRect, APoint);
  Realign;
end;

function TThShapeItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  if Assigned(FSelection) and FSelection.PtInSelection(APt) then
    Exit(True);

  Result := inherited PtInItem(APt);
end;

procedure TThShapeItem.ResizeItem(ARect: TFloatRect);
begin
  FRect := ARect;
  Realign;
end;

procedure TThShapeItem.SetSelected(const Value: Boolean);
begin
  if Value = FSelected then
    Exit;
  FSelected := Value;

  if Value then
    FSelection := TThShapeSelection.Create(Self)
  else
    FSelection := nil; // Free(ARC)
end;

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

    if Item.PtInItem(APoint) then
      Exit(Item);
  end;
end;

procedure TThDrawItems.MouseOver(APoint: TFLoatPoint);
var
  I: Integer;
  Item: TThShapeItem;
begin
  for I := Count - 1 downto 0 do
  begin
    Item := Items[I] as TThShapeItem;
    if Assigned(Item.Selection) then
      Item.Selection.MouseOver(APoint);
  end;
end;

function TThDrawItems.PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;
var
  I: Integer;
  PolyRect, DestRect: TFloatRect;
  DestPaths: TThPolyPoly;
begin
  PolyRect := PolygonBounds(APoly);
//
  for I := 0 to Count - 1 do
  begin
    // Rect로 1차 교차 비교
    IntersectRect(DestRect, PolyRect, Items[I].Bounds);
    if IsRectEmpty(DestRect) then
      Continue;

    // Polygon 교차 비교(Clipper로 교차 영역 생성 후 비었는지 확인)
    with TClipper.Create do
    try
      AddPaths(Items[I].PolyPoly, ptSubject);
      AddPath(APoly, ptClip);

      Execute(ctIntersection, frNonZero, DestPaths);
    finally
      Free;
    end;

    if Length(DestPaths) > 0 then
      Result := Result + [Items[I]];
  end;
end;

{ TThShapeDrawItems }

procedure TThSelectedItems.MoveItem(APoint: TFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MoveItem(APoint);
end;

end.
