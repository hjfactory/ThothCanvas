{
  Role
    Store drawing datas.
}

unit ThItem;

interface

uses
  System.Generics.Collections,
  GR32,
  ThTypes, ThClasses;

type
  TThItem = class(TThInterfacedObject, IThItem)
  private
    FBounds: TFloatRect;
    FPolyPoly: TThPolyPoly;
    function GetPolyPoly: TThPolyPoly;
  protected
    function GetBounds: TFloatRect; virtual;

    procedure Realign;
    procedure DoRealign; virtual;
  public
    constructor Create; virtual;
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual; abstract;

    procedure MouseMove(APoint: TFloatPoint); virtual;
    procedure MouseEnter(APoint: TFloatPoint); virtual;
    procedure MouseLeave(APoint: TFloatPoint); virtual;

    function PtInItem(APt: TFloatPoint): Boolean; virtual;

    property Bounds: TFloatRect read GetBounds;
    property PolyPoly: TThPolyPoly read GetPolyPoly;
  end;

  TThDrawItem = class(TThItem)
  end;

  TThPenItem = class(TThDrawItem)
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

  TThShapeItem = class(TThItem, IThShapeItem, IThSelectableItem)
  private
    FSelected: Boolean;
    FSelection: IThItemSelection;

    FBorderWidth: Integer;

    FBorderColor: TColor32;
    procedure SetSelected(const Value: Boolean);
    function GetSelected: Boolean;
  protected
    function GetBounds: TFloatRect; override;

    procedure DoRealign; override;
    function CreateSelection: IThItemSelection; virtual; abstract;
    function GetSelection: IThItemSelection;
  public
    procedure SetStyle(AStyle: IThDrawStyle); virtual; abstract;

    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint); virtual; abstract;
    procedure MoveItem(APoint: TFloatPoint); virtual; abstract;

    function PtInItem(APt: TFloatPoint): Boolean; override;

    procedure MouseMove(APoint: TFloatPoint); override;
    procedure MouseEnter(APoint: TFloatPoint); override;
    procedure MouseLeave(APoint: TFloatPoint); override;

    property Selected: Boolean read GetSelected write SetSelected;
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
    function CreateSelection: IThItemSelection; override;
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
    function CreateSelection: IThItemSelection; override;
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

implementation

uses
  Vcl.Forms,
  System.UITypes,
  System.Math,
  GR32_Polygons, GR32_VectorUtils, GR32_Clipper,
  ThUtils, ThItemStyle, ThItemSelection;

{ TThItem }

constructor TThItem.Create;
begin
end;

procedure TThItem.DoRealign;
begin
end;

function TThItem.GetBounds: TFloatRect;
begin
  Result := FBounds;
end;

function TThItem.GetPolyPoly: TThPolyPoly;
begin
  Result := FPolyPoly
end;

procedure TThItem.MouseMove(APoint: TFloatPoint);
begin
end;

procedure TThItem.MouseEnter(APoint: TFloatPoint);
begin
end;

procedure TThItem.MouseLeave(APoint: TFloatPoint);
begin
end;

function TThItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  Result := PtInRect(FBounds, APt) and PtInPolyPolygon(APt, FPolyPoly);
end;

procedure TThItem.Realign;
begin
  DoRealign;
  FBounds := PolypolygonBounds(FPolyPoly);
end;

{ TThPenItem }

constructor TThPenItem.Create(APath: TThPath; APolyPoly: TThPolyPoly);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;
end;

procedure TThPenItem.SetStyle(AThickness: Integer; AColor: TColor32; AAlpha: Byte);
begin
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;

procedure TThPenItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThPenStyle;
begin
  Style := TThPenStyle(AStyle);
  SetStyle(Style.Thickness, Style.Color, Style.Alpha);
end;

destructor TThPenItem.Destroy;
begin
  inherited;
end;

procedure TThPenItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, Color);
end;

procedure TThPenItem.DrawPoly(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint; APath: TThPath; APolyPoly: TThPolyPoly);
begin
  FPolyPoly := APolyPoly;
  Realign;

  Draw(Bitmap, AScale, AOffset);
end;

function TThPenItem.GetColor: TColor32;
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

function TThShapeItem.GetBounds: TFloatRect;
var
  Radius: Single;
  Selection: TThItemSelection;
begin
  Result := inherited GetBounds;
  if Assigned(FSelection) then
  begin
    Radius := TThItemSelection(FSelection).HandleRadius;
    InflateRect(Result, Radius, Radius)
  end;
end;

function TThShapeItem.GetSelected: Boolean;
begin
  Result := FSelected
end;

function TThShapeItem.GetSelection: IThItemSelection;
begin
  Result := FSelection;
end;

procedure TThShapeItem.MouseMove(APoint: TFloatPoint);
begin
  // Selection > HotHandle ����
  if Assigned(FSelection) then
    FSelection.MouseMove(APoint);
end;

procedure TThShapeItem.MouseEnter(APoint: TFloatPoint);
begin
  Screen.Cursor := crSizeAll;
end;

procedure TThShapeItem.MouseLeave(APoint: TFloatPoint);
begin
  Screen.Cursor := crDefault;

  if Assigned(FSelection) then
    FSelection.HotHandle := nil;
end;

function TThShapeItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  Result := False;
  if PtInRect(Bounds, APt) then
  begin
    if Assigned(FSelection) and FSelection.PtInHandles(APt) then
      Exit(True);

    Result := PtInPolyPolygon(APt, FPolyPoly);
  end;
end;

procedure TThShapeItem.SetSelected(const Value: Boolean);
begin
  if Value = FSelected then
    Exit;
  FSelected := Value;

  if Value then
    FSelection := CreateSelection
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

function TThFillShapeItem.CreateSelection: IThItemSelection;
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

function TThLineShapeItem.CreateSelection: IThItemSelection;
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

end.
