{
  Role
}

unit ThDrawItem;

interface

uses
  System.Generics.Collections,
  GR32, GR32_Polygons, GR32_VectorUtils,
  ThTypes, ThUtils;


type
  TThDrawItem = class;
  TTHDrawItemArray = array of TThDrawItem;
  TThDrawItems = class(TObjectList<TThDrawItem>)
  public
    // APoint에 포함되는 최상위 객체 반환
    function PtInItem(APoint: TFloatPoint): TThDrawItem;
    // APoly 영역에 포함되는 객체 목록 반환
    function PolyInItems(APoly: TThPoly): TThDrawItemArray;
  end;

  TThDrawItem = class
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
    function GetBounds: TFloatRect;
  protected
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;
    property Bounds: TFloatRect read GetBounds;
    property PolyPoly: TThPolyPoly read FPolyPoly;
  end;

  TThBrushDrawItem = class(TThDrawItem)
  end;

  TThPenDrawItem = class(TThBrushDrawItem)
  private
    FPath: TArray<TFloatPoint>;

    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;

    FIsDeletion: Boolean;
  public
    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly;
      AThickness: Integer; AColor: TColor32; AAlpha: Byte);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Path: TThPath read FPath;
    property IsDeletion: Boolean read FIsDeletion write FIsDeletion default False;
  end;

  TThShapeDrawItem = class(TThDrawItem)
  private
    FIsSelection: Boolean;
  public
    property IsSelection: Boolean read FIsSelection write FIsSelection;
  end;

  TThRectDrawItem = class(TThShapeDrawItem)
  private
    FRect: TFloatRect;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
  public
    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single;
      AColor: TColor32; AAlpha: Byte);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;
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
    Item := TThDrawItem(Items[I]);
    if not PtInRect(Item.Bounds, APoint) then
      Continue;

    if not PtInPolyPolygon(APoint, Item.PolyPoly) then
      Continue;

    Exit(Item);
  end;
end;

function TThDrawItems.PolyInItems(APoly: TThPoly): TThDrawItemArray;
begin

end;

{ TThDrawItem }

function TThDrawItem.GetBounds: TFloatRect;
begin
  if IsRectEmpty(FBounds) then
    FBounds := PolypolygonBounds(FPolyPoly);
  Result := FBounds;
end;

{ TThFreeDrawItem }

constructor TThPenDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly;
  AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;

  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

procedure TThPenDrawItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  LColor: TColor32;
  LAlpha: Byte;
  PolyPoly: TThPolyPoly;
begin
  LColor := FColor;
  LAlpha := FAlpha;
  if FIsDeletion then
    LAlpha := Round(LAlpha * 0.2);

  ModifyAlpha(LColor, LAlpha);

  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, LColor);
end;

{ TThRectangleItem }

constructor TThRectDrawItem.Create(ARect: TFloatRect; APoly: TThPoly;
  AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FRect := ARect;
  FPolyPoly := PolyPolygon(APoly);
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;


procedure TThRectDrawItem.Draw(Bitmap: TBitmap32; AScale,
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
end;

end.
