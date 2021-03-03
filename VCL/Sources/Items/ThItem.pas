{
  Role
    Store drawing datas.
}

unit ThItem;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  GR32,
  ThTypes, ThClasses, ThItemSelection;

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

    function PtInItem(APt: TFloatPoint): Boolean; virtual;

    property Bounds: TFloatRect read GetBounds;
    property PolyPoly: TThPolyPoly read GetPolyPoly;
  end;

  TThCustomDrawItem = class(TThItem)
  end;

  TThPenItem = class(TThCustomDrawItem)
  private
    FPath: TThPath;

    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;

    FIsDeletion: Boolean;
    function GetColor: TColor32;
  public
//    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly); reintroduce; overload;
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
//    FSelectionClass: TThItemSelectionClass;

    FBorderWidth: Integer;

    FBorderColor: TColor32;

    // IThSelectableItem
    procedure SetSelected(const Value: Boolean);
    function GetSelected: Boolean;
    function GetSelection: IThItemSelection;
  protected
    function GetBounds: TFloatRect; override;

    procedure DoRealign; override;
    function CreateSelection: IThItemSelection; virtual; abstract;

    // IThSelectableItem
    procedure MoveItem(APoint: TFloatPoint); virtual; abstract;

    procedure MouseDown(APoint: TFloatPoint); virtual;
    procedure MouseMove(APoint: TFloatPoint); virtual;
    procedure MouseUp(APoint: TFloatPoint); virtual;
    procedure MouseEnter(APoint: TFloatPoint); virtual;
    procedure MouseLeave(APoint: TFloatPoint); virtual;

    // IThShapeItem
    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetStyle(AStyle: IThDrawStyle); virtual; abstract;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function PtInItem(APt: TFloatPoint): Boolean; override;

    property Selected: Boolean read GetSelected write SetSelected;
    property Selection: IThItemSelection read FSelection;

    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BorderColor: TColor32 read FBorderColor write FBorderColor;
  end;

  {Naming: ClosedShapeItem, FillableShapeItem}
  TThFaceShapeItem = class(TThShapeItem, IThConnectableItem)
  private
    FRect: TFloatRect;
    FColor: TColor32;
    FConnection: IThItemConnection;
  protected
    procedure DoRealign; override;
    function RectToPolyPoly(ARect: TFloatRect): TThPolyPoly; virtual; abstract;

    procedure MouseDown(APoint: TFloatPoint); override;
    procedure MouseMove(APoint: TFloatPoint); override;
    procedure MouseUp(APoint: TFloatPoint); override;
    procedure MouseEnter(APoint: TFloatPoint); override;
    procedure MouseLeave(APoint: TFloatPoint); override;

    function CreateSelection: IThItemSelection; override;
    function CreateConnection: IThItemConnection; virtual;

    procedure ShowConnection;
    procedure HideConnection;
  public
//    constructor Create(ARect: TFloatRect; AColor: TColor32;
//      ABorderWidth: Integer; ABorderColor: TColor32); reintroduce;
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

  TThLineShapeItem = class(TThShapeItem, IThConnectorItem)
  private
    FFromPoint, FToPoint: TFloatPoint;
  protected
    procedure DoRealign; override;
    function CreateSelection: IThItemSelection; override;
    function PointToPolyPoly(AFromPoint, AToPoint: TFloatPoint): TThPolyPoly; virtual; abstract;
  public
//    constructor Create(AFromPoint, AToPoint: TFloatPoint; ABorderWidth: Integer;
//      ABorderColor: TColor32); reintroduce;

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
//  Winapi.Windows, // ODS
  Vcl.Forms,
  System.UITypes, System.Math,
  GR32_Polygons, GR32_VectorUtils,
  ThUtils, ThItemStyle,
  ThItemConnection;

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

function TThItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  Result := GR32.PtInRect(FBounds, APt) and PtInPolyPolygon(APt, FPolyPoly);
end;

procedure TThItem.Realign;
begin
  DoRealign;
  FBounds := PolypolygonBounds(FPolyPoly);
end;

{ TThPenItem }

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

constructor TThShapeItem.Create;
begin
  inherited;

  FSelection := CreateSelection
end;

destructor TThShapeItem.Destroy;
begin
  FSelection := nil; // Free(ARC)

  inherited;
end;

procedure TThShapeItem.DoRealign;
begin
  inherited;
end;

procedure TThShapeItem.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
  if FSelected and Assigned(FSelection) then
    FSelection.Draw(Bitmap, AScale, AOffset);
end;

function TThShapeItem.GetBounds: TFloatRect;
var
  Radius: Single;
begin
  Result := inherited GetBounds;
  if Assigned(FSelection) then
  begin
    Radius := TThItemSelection(FSelection).HandleRadius;
    GR32.InflateRect(Result, Radius, Radius)
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

procedure TThShapeItem.MouseDown(APoint: TFloatPoint);
begin
  if FSelected then
    FSelection.MouseDown(APoint);
end;

procedure TThShapeItem.MouseMove(APoint: TFloatPoint);
begin
  if FSelected then
    FSelection.MouseMove(APoint);
end;

procedure TThShapeItem.MouseUp(APoint: TFloatPoint);
begin
  if FSelected then
    FSelection.MouseUp(APoint);
end;

procedure TThShapeItem.MouseEnter(APoint: TFloatPoint);
begin
  if FSelected and TThItemSelection(FSelection).IsResizing then
    Exit;

  Screen.Cursor := crSizeAll;
end;

procedure TThShapeItem.MouseLeave(APoint: TFloatPoint);
begin
  if FSelected and TThItemSelection(FSelection).IsResizing then
    Exit;

  if FSelected then
    FSelection.ReleaseHotHandle;

  Screen.Cursor := crDefault;
end;

function TThShapeItem.PtInItem(APt: TFloatPoint): Boolean;
begin
  Result := False;
  if GR32.PtInRect(Bounds, APt) then
  begin
    if FSelected and FSelection.PtInHandles(APt) then
      Exit(True);

    Result := PtInPolyPolygon(APt, FPolyPoly);
  end;
end;

procedure TThShapeItem.SetSelected(const Value: Boolean);
begin
  if Value = FSelected then
    Exit;

  FSelected := Value;
//
//  if Value then
//    FSelection := CreateSelection
//  else
//    FSelection := nil; // Free(ARC)
end;

{ TThFillShapeItem }

procedure TThFaceShapeItem.SetStyle(AColor: TColor32; ABorderWidth: Integer;
  ABorderColor: TColor32);
begin
  FColor := AColor;
  FBorderWidth := ABorderWidth;
  FBorderColor := ABorderColor;
end;

procedure TThFaceShapeItem.SetStyle(AStyle: IThDrawStyle);
var
  Style: TThShapeStyle;
begin
  Style := TThShapeStyle(AStyle);
  SetStyle(Style.Color, Style.BorderWidth, Style.BorderColor);
end;

procedure TThFaceShapeItem.ShowConnection;
begin
  FConnection := CreateConnection;
end;

procedure TThFaceShapeItem.HideConnection;
begin
  FConnection := nil;
end;

procedure TThFaceShapeItem.DoRealign;
begin
  inherited;

//  FRect.Realign;

  FPolyPoly := RectToPolyPoly(FRect);

  if Assigned(FSelection) then
    FSelection.RealignHandles;
end;

procedure TThFaceShapeItem.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  PolyPoly: TThPolyPoly;
begin
  PolyPoly := ScalePolyPolygon(FPolyPoly, AScale.X, AScale.Y);
  TranslatePolyPolygonInplace(PolyPoly, AOffset.X, AOffset.Y);

  PolyPolygonFS(Bitmap, PolyPoly, FColor);

  PolyPolylineFS(Bitmap, PolyPoly, FBorderColor, True, FBorderWidth);

  if Assigned(FConnection) then
    FConnection.Draw(Bitmap, AScale, AOffset);

  inherited; // Draw Selection
end;

procedure TThFaceShapeItem.DrawPoints(Bitmap: TBitmap32; AScale, AOffset,
  AFromPoint, AToPoint: TFloatPoint);
begin
  FRect.TopLeft     := AFromPoint;
  FRect.BottomRight := AToPoint;
  Realign;

  Draw(Bitmap, AScale, AOffset);
end;

function TThFaceShapeItem.CreateConnection: IThItemConnection;
begin
  Result := TThItemAnchorPoints.Create(Self);
end;

function TThFaceShapeItem.CreateSelection: IThItemSelection;
begin
  Result := TThShapeSelection.Create(Self);
end;

procedure TThFaceShapeItem.MouseDown(APoint: TFloatPoint);
begin
  inherited;

  if Assigned(FConnection) then
    FConnection.MouseDown(APoint);
end;

procedure TThFaceShapeItem.MouseMove(APoint: TFloatPoint);
begin
  inherited;

  if Assigned(FConnection) then
    FConnection.MouseMove(APoint);
end;

procedure TThFaceShapeItem.MouseUp(APoint: TFloatPoint);
begin
  inherited;

  if Assigned(FConnection) then
    FConnection.MouseUp(APoint);
end;

procedure TThFaceShapeItem.MouseEnter(APoint: TFloatPoint);
begin
  inherited;
//  FConnection := CreateConnection;
end;

procedure TThFaceShapeItem.MouseLeave(APoint: TFloatPoint);
begin
//  FConnection := nil;
  inherited;
  HideConnection;
end;

procedure TThFaceShapeItem.MoveItem(APoint: TFloatPoint);
begin
  FRect := OffsetRect(FRect, APoint);
  Realign;
end;

procedure TThFaceShapeItem.ResizeItem(ARect: TFloatRect);
begin
  FRect := ARect;
  Realign;
end;

{ TThLineShapeItem }

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


  inherited;

//  if FSelected and Assigned(FSelection) then
//    FSelection.Draw(Bitmap, AScale, AOffset);
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
