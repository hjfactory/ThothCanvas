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
    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly); reintroduce; overload;
    destructor Destroy; override;

    procedure SetStyle(AThickness: Integer; AColor: TColor32; AAlpha: Byte); overload;
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

    FBorderWidth: Integer;

    FBorderColor: TColor32;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure DoRealign; override;
    function GetSelection: IThItemSelection; virtual;
  public
    procedure SetStyle(AStyle: IThDrawStyle); virtual; abstract;

    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint); virtual; abstract;
    procedure MoveItem(APoint: TFloatPoint); virtual; abstract;

    function PtInItem(APt: TFloatPoint): Boolean; override;

    property Selected: Boolean read FSelected write SetSelected;
    property Selection: IThItemSelection read FSelection;

    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BorderColor: TColor32 read FBorderColor write FBorderColor;
  end;

  TThFillShapeItem = class(TThShapeItem)
  private
    FRect: TFloatRect;
    FColor: TColor32;
  protected
    procedure DoRealign; override;
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; virtual; abstract;
    function GetSelection: IThItemSelection; override;
  public
    constructor Create(ARect: TFloatRect; AColor: TColor32;
      ABorderWidth: Integer; ABorderColor: TColor32); reintroduce;

    procedure SetStyle(AColor: TColor32;
      ABorderWidth: Integer; ABorderColor: TColor32); reintroduce; overload;
    procedure SetStyle(AStyle: IThDrawStyle); overload; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint); override;

    procedure ResizeItem(ARect: TFloatRect);
    procedure MoveItem(APoint: TFloatPoint); override;

    property Rect: TFloatRect read FRect;
    property Color: TColor32 read FColor;
  end;

  TThLineShapeItem = class(TThShapeItem)
  private
    FFromPoint, FToPoint: TFloatPoint;
  protected
    procedure DoRealign; override;
    function GetSelection: IThItemSelection; override;
    function PointToPolyPoly(AFromPoint, AToPoint: TFloatPoint): TThPolyPoly; virtual; abstract;
  public
    constructor Create(AFromPoint, AToPoint: TFloatPoint; ABorderWidth: Integer;
      ABorderColor: TColor32); reintroduce;

    procedure SetStyle(ABorderWidth: Integer; ABorderColor: TColor32); reintroduce; overload;
    procedure SetStyle(AStyle: IThDrawStyle); overload; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint); override;

    procedure ResizeItem(AFromPoint, AToPoint: TFloatPoint);
    procedure MoveItem(APoint: TFloatPoint); override;

    property FromPoint: TFloatPoint read FFromPoint;
    property ToPoint: TFloatPoint read FToPoint;
  end;

  TThDrawItems = class(TObjectList<TThDrawItem>)
  public
    // APoint에 포함되는 최상위 객체 반환
    function PtInItem(APoint: TFloatPoint): TThDrawItem;
    // APoly 영역에 포함되는 객체 배열 반환
    function PolyInItems(APoly: TThPoly): TArray<TThDrawItem>;

//    procedure MouseOver(APoint: TFLoatPoint);
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
  ThUtils, ThDrawStyle, ThItemSelection;

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

{ TThPenDrawItem }

constructor TThPenDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;
end;

procedure TThPenDrawItem.SetStyle(AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

procedure TThPenDrawItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThPenStyle;
begin
  Style := TThPenStyle(AStyle);
  SetStyle(Style.Thickness, Style.Color, Style.Alpha);
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

{ TThShapeItem }

procedure TThShapeItem.DoRealign;
begin
  inherited;
end;

function TThShapeItem.GetSelection: IThItemSelection;
begin
end;

function TThShapeItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  if Assigned(FSelection) and FSelection.PtInHandles(APt) then
    Exit(True);

  Result := inherited PtInItem(APt);
end;

procedure TThShapeItem.SetSelected(const Value: Boolean);
begin
  if Value = FSelected then
    Exit;
  FSelected := Value;

  if Value then
    FSelection := GetSelection
  else
    FSelection := nil; // Free(ARC)
end;

{ TThFillShapeItem }

constructor TThFillShapeItem.Create(ARect: TFloatRect; AColor: TColor32;
  ABorderWidth: Integer; ABorderColor: TColor32);
begin
  FRect := ARect;
  FPolyPoly := RectToPolyPoly(FRect);

  FColor := AColor;
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThFillShapeItem.SetStyle(AColor: TColor32; ABorderWidth: Integer;
  ABorderColor: TColor32);
begin
  FColor := AColor;
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThFillShapeItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThShapeStyle;
begin
  Style := TThShapeStyle(AStyle);
  SetStyle(Style.Color, Style.BorderWidth, Style.BorderColor);
end;

procedure TThFillShapeItem.DoRealign;
begin
  inherited;

  FRect.Realign;

  FPolyPoly := RectToPolyPoly(FRect);

  if Assigned(FSelection) then
    FSelection.RealignHandles;
end;

procedure TThFillShapeItem.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
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

procedure TThFillShapeItem.DrawPoints(Bitmap: TBitmap32; AScale, AOffset,
  AFromPoint, AToPoint: TFloatPoint);
begin
  FRect.TopLeft     := AFromPoint;
  FRect.BottomRight := AToPoint;
  Realign;

  Draw(Bitmap, AScale, AOffset);
end;

function TThFillShapeItem.GetSelection: IThItemSelection;
begin
  Result := TThShapeSelection.Create(Self);
end;

procedure TThFillShapeItem.MoveItem(APoint: TFloatPoint);
begin
  FRect := OffsetRect(FRect, APoint);
  Realign;
end;

procedure TThFillShapeItem.ResizeItem(ARect: TFloatRect);
begin
  FRect := ARect;
  Realign;
end;

{ TThLineShapeItem }

constructor TThLineShapeItem.Create(AFromPoint, AToPoint: TFloatPoint;
  ABorderWidth: Integer; ABorderColor: TColor32);
begin
  FFromPoint := AFromPoint;
  FToPoint := AToPoint;

  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThLineShapeItem.SetStyle(ABorderWidth: Integer;
  ABorderColor: TColor32);
begin
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThLineShapeItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThShapeStyle;
begin
  Style := TThShapeStyle(AStyle);
  SetStyle(Style.BorderWidth, Style.BorderColor);
end;

procedure TThLineShapeItem.DoRealign;
begin
  inherited;

  FPolyPoly := PointToPolyPoly(FFromPoint, FToPoint);

  if Assigned(FSelection) then
    FSelection.RealignHandles;
end;

procedure TThLineShapeItem.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, FBorderColor);

//  PolyPolylineFS(Bitmap, PolyPoly, FBorderColor, True, FBorderWidth);

  if FSelected and Assigned(FSelection) then
    FSelection.Draw(Bitmap, AScale, AOffset);
end;

procedure TThLineShapeItem.DrawPoints(Bitmap: TBitmap32; AScale, AOffset,
  AFromPoint, AToPoint: TFloatPoint);
begin
  FFromPoint  := AFromPoint;
  FToPoint    := AToPoint;
  Realign;

  Draw(Bitmap, AScale, AOffset);
end;

function TThLineShapeItem.GetSelection: IThItemSelection;
begin
  Result := TThLineSelection.Create(Self);
end;

procedure TThLineShapeItem.MoveItem(APoint: TFloatPoint);
begin
  FFromPoint := FFromPoint.Offset(APoint);
  FToPoint := FToPoint.Offset(APoint);
  Realign;
end;

procedure TThLineShapeItem.ResizeItem(AFromPoint, AToPoint: TFloatPoint);
begin
  FFromPoint := AFromPoint;
  FToPoint := AToPoint;
  Realign;
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

//procedure TThDrawItems.MouseOver(APoint: TFLoatPoint);
//var
//  I: Integer;
//  Item: TThShapeItem;
//begin
//  for I := Count - 1 downto 0 do
//  begin
//    Item := Items[I] as TThShapeItem;
//    if Assigned(Item.Selection) then
//      Item.Selection.MouseOver(APoint);
//  end;
//end;

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

{ TThSelectedItems }

procedure TThSelectedItems.MoveItem(APoint: TFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MoveItem(APoint);
end;

end.
