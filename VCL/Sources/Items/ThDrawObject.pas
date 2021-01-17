{
  Role
    Draw shapes with the mouse movements.
}

unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32, GR32_Polygons, GR32_VectorUtils,
  clipper,

  ThTypes, ThClasses,
  ThDrawStyle, ThDrawItem;

type
  TThDrawObject = class(TInterfacedObject, IThDrawObject)
  private
    FDrawStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); virtual; abstract;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); virtual;

    function CreateItem: TObject; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  /// Brush Objects
  TThBrushDrawObject = class(TThDrawObject)
  public
    destructor Destroy; override;
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

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;
    function CreateItem: TObject; override;
  end;

  TThEraserDrawObject = class(TThBrushDrawObject)
  private
    FDrawItems: TThDrawItems;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThDrawItems); overload;
  end;

  TThObjErsDrawObject = class(TThEraserDrawObject)
  public
    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;
  end;

  /// Shape Objects
  TThShapeDrawObject = class(TThDrawObject)
  private
    FDownPos, FCurrPos: TFloatPoint;
    function GetDrawStyle: TThShapeStyle;
    property DrawStyle: TThShapeStyle read GetDrawStyle;
  public
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;

    function CreateItem: TObject; override;
  end;

  // Shape Select
  TThShapeSelectObject = class(TThDrawObject)
  private
    FDrawItems: TThDrawItems;
    FSelectItems: TList<TThShapeDrawItem>;

    procedure SetSelection(AItem: TThShapeDrawItem; AShift: TShiftState);
  public
    constructor Create(AItems: TThDrawItems); overload;
    destructor Destroy; override;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;

    property Items: TList<TThShapeDrawItem> read FSelectItems;

    procedure ClearSelection;
  end;

implementation

uses
  ThUtils;

{ TThDrawObject }

constructor TThDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

procedure TThDrawObject.Start;
begin
end;

procedure TThDrawObject.Done;
begin
end;

function TThDrawObject.CreateItem: TObject;
begin
  Result := nil;
end;

procedure TThDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
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

procedure TThPenDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
begin
  FPath.Add(APoint);

  Poly := Circle(APoint, PenStyle.Thickness / 2);
  FPolyPoly := PolyPolygon(Poly);
  FPolyPolyPath := AAFloatPoint2AAPoint(FPolyPoly, 3);
end;

procedure TThPenDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
begin
  FPath.Add(APoint);

  LastP := FPath.Items[FPath.Count-2];
  Poly := BuildPolyline([LastP, APoint], PenStyle.Thickness, jsRound, esRound);
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

procedure TThPenDrawObject.Done;
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

procedure TThObjErsDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
begin
  Move(APoint, AShift);
end;

procedure TThObjErsDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
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

procedure TThObjErsDrawObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
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

{ TThShapeDrawObject }

procedure TThShapeDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FDownPos := APoint;
end;

procedure TThShapeDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FCurrPos := APoint;
end;

procedure TThShapeDrawObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

end;

procedure TThShapeDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  ScalePolygonInplace(Poly, AScale.X, AScale.Y);
  TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

  PolygonFS(Bitmap, Poly, DrawStyle.Color);

  PolylineFS(Bitmap, Poly, DrawStyle.BorderColor, True, DrawStyle.BorderWidth);
end;

function TThShapeDrawObject.GetDrawStyle: TThShapeStyle;
begin
  Result := TThShapeStyle(FDrawStyle);
end;

function TThShapeDrawObject.CreateItem: TObject;
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  Result := TThRectDrawItem.Create(FloatRect(FDownPos, FCurrPos), Poly,
      DrawStyle.BorderWidth, DrawStyle.Color, 255);
end;

{ TThShapeSelectObject }

procedure TThShapeSelectObject.ClearSelection;
var
  Item: TThShapeDrawItem;
begin
  for Item in FSelectItems do
    Item.IsSelection := False;
  FSelectItems.Clear;
end;

constructor TThShapeSelectObject.Create(AItems: TThDrawItems);
begin
  FDrawItems := AItems;
  FSelectItems := TList<TThShapeDrawItem>.Create;
end;

destructor TThShapeSelectObject.Destroy;
begin
  FSelectItems.Free;

  inherited;
end;

procedure TThShapeSelectObject.SetSelection(AItem: TThShapeDrawItem; AShift: TShiftState);
begin
  if Assigned(AItem) then
  begin
    // 선택되지 않은 경우 선택 처리(이미 선택된 경우 무시)
    if AItem.IsSelection then
    begin
      if ssShift in AShift then
      begin
        AItem.IsSelection := False;
        FSelectItems.Remove(AItem);
      end;
    end
    else
    begin
      if not (ssShift in AShift)  then
        ClearSelection;

      AItem.IsSelection := True;
      FSelectItems.Add(AItem);
    end;
  end
  else
  begin
    if not (ssShift in AShift) then
      ClearSelection;
  end;
end;

procedure TThShapeSelectObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Item: TThDrawItem;
begin
  Item := FDrawItems.PtInItem(APoint);

  SetSelection(Item as TThShapeDrawItem, AShift);
end;

procedure TThShapeSelectObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
begin

end;

procedure TThShapeSelectObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

end;

end.
