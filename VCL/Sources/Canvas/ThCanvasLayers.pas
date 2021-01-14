unit ThCanvasLayers;

interface

uses
  System.Classes, System.Types, System.Math, System.Generics.Collections,
  Vcl.Controls,

  GR32,
  GR32_Layers,
  GR32_Polygons,
  GR32_VectorUtils,
  clipper,

  ThTypes,
  ThDrawItem,
  ThDrawStyle,
  ThDrawObject;

type
  TThCustomViewLayer = class(TPositionedLayer)
  private
    FHitTest: Boolean;
    function GetOffset: TFloatPoint;
    function GetScale: TFloatPoint;
  protected
    FDrawItems: TObjectList<TThDrawItem>;

    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure Clear;

    property HitTest: Boolean read FHitTest write FHitTest;
    property Scale: TFloatPoint read GetScale;
    property Offset: TFloatPoint read GetOffset;
  end;

  TThCustomDrawLayer = class(TThCustomViewLayer)
  private
    FMouseDowned: Boolean;

  protected
    FDrawStyle: IThDrawStyle;
    FDrawObject: IThDrawObject;

    procedure SetDrawStyle(const Value: IThDrawStyle); virtual;

    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property DrawStyle: IThDrawStyle read FDrawStyle write SetDrawStyle;
  end;

  TBrushDrawLayer = class(TThCustomDrawLayer)
  private
//    procedure AddEraserPoint(const X, Y: Integer);
//    procedure DeleteDrawItems;
  protected
    function  DoHitTest(X, Y: Integer): Boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  end;

  TShapeDrawLayer = class(TThCustomDrawLayer)
  private
    FMouseDowned: Boolean;

    FCanvasMode: TThCanvasMode;
    FShapeMode: TThShapeMode;

    FDrawItems: TObjectList<TThRectangleItem>;

    FDownPos, FCurrPos: TFloatPoint;
    procedure CreateDrawItem;

    procedure PaintDrawItem(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure PaintDrawItems(Buffer: TBitmap32; AScale, AOffset: TFloatPoint);
  protected
    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure SetCanvasMode(AMode: TThCanvasMode);
    property ShapeMode: TThShapeMode read FShapeMode write FShapeMode;

    procedure Clear;
  end;

implementation

{ TFreeDrawLayer }

uses
  DebugForm,
  ThGraphicsUtils;


{ TThCustomViewLayer }

constructor TThCustomViewLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FDrawItems := TObjectList<TThDrawItem>.Create(True);

  Scaled := True;
end;

destructor TThCustomViewLayer.Destroy;
begin
  FDrawItems.Clear;
  FDrawItems.Free;

  inherited;
end;

procedure TThCustomViewLayer.Clear;
begin
  FDrawItems.Clear;
  Update;
end;

function TThCustomViewLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  if not FHitTest then
    Exit(False);

  Result := inherited;
end;

function TThCustomViewLayer.GetOffset: TFloatPoint;
begin
  LayerCollection.GetViewportShift(Result.X, Result.Y);
end;

function TThCustomViewLayer.GetScale: TFloatPoint;
begin
  LayerCollection.GetViewportScale(Result.X, Result.Y);
end;

procedure TThCustomViewLayer.Paint(Buffer: TBitmap32);
var
  Item: TThDrawItem;
begin
  inherited;

  Buffer.BeginUpdate;
  for Item in FDrawItems do
    Item.Draw(Buffer, Scale, Offset);
  Buffer.EndUpdate;
end;

{ TThCustomDrawLayer }

constructor TThCustomDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FMouseDowned := False;

  MouseEvents := True;
end;

procedure TThCustomDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TFloatPoint;
begin
  inherited;

//  if FCanvasMode <> cmFreeDraw then
//    Exit;

  if Button = mbLeft then
  begin
    P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);

    FMouseDowned := True;

    FDrawObject.Move(P);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TFloatPoint;
begin
  inherited;

  if FMouseDowned then
  begin
    P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    FDrawObject.Move(P);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Item: TThDrawItem;
begin
  inherited;

  if FMouseDowned then
  begin
    FMouseDowned := False;
    Item := FDrawObject.CreateItem as TThDrawItem;
    if Assigned(Item) then
      FDrawItems.Add(Item);
    FDrawObject.Clear;
    Update;
  end;
end;

procedure TThCustomDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;

  if Assigned(FDrawObject) then
    FDrawObject.Draw(Buffer, Scale, Offset);
end;

procedure TThCustomDrawLayer.SetDrawStyle(const Value: IThDrawStyle);
var
  Style: TThPenStyle;
begin
  FDrawStyle := Value;

  TThBrushObject(FDrawObject).BrushStyle := FDrawStyle;
end;

{ TBrushDrawLayer }

constructor TBrushDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FDrawStyle := TThPenStyle.Create;
  FDrawObject := TThPenObject.Create(FDrawStyle);

  FMouseDowned := False;
end;

destructor TBrushDrawLayer.Destroy;
begin

  inherited;
end;

//procedure TBrushDrawLayer.AddEraserPoint(const X, Y: Integer);
//var
//  P: TFloatPoint;
//  Item: TThDrawItem;
//  Poly: TThPoly;
//  PolyRect, DestRect: TFloatRect;
//  EraserPath: TPath;
//  ItemPaths, DestPaths: TPaths;
//begin
//  P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
//  Poly := Circle(P, FThickness / 2);
//  PolyRect := PolygonBounds(Poly);
//  for Item in FDrawItems do
//  begin
//    IntersectRect(DestRect, PolyRect, Item.Bounds);
//    if IsRectEmpty(DestRect) then
//      Continue;
//
//    EraserPath := AAFloatPoint2AAPoint(Poly);
//    ItemPaths := AAFloatPoint2AAPoint(Item.PolyPoly);
//    with TClipper.Create do
//    begin
//      StrictlySimple := True;
//      AddPaths(ItemPaths, ptSubject, True);
//      AddPath(EraserPath, ptClip, True);
//
//      Execute(ctIntersection, DestPaths, pftNonZero);
//    end;

//    if Length(DestPaths) > 0 then
//      Item.IsToDelete := True;
//  end;
//end;

//procedure TBrushDrawLayer.DeleteDrawItems;
//var
//  I: Integer;
//  Item: TThPenDrawItem;
//begin
//  for I := FDrawItems.Count - 1 downto 0 do
//  begin
//    Item := FDrawItems[I];
//    if Item.IsToDelete then
//      FDrawItems.Delete(I);
//  end;
//end;

function TBrushDrawLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := Assigned(FDrawObject);
end;

//function TBrushDrawLayer.GetPenStyle: TThPenStyle;
//begin
//  Result := TThPenStyle(FDrawStyle);
//end;
//
//procedure TBrushDrawLayer.SetDrawMode(const Value: TThBrushDrawMode);
//begin
//  FDrawMode := Value;
//
//  case FDrawMode of
//    bdmNone:
//      FDrawObject := nil;
//    bdmPen:
//      FDrawObject := TThPenObject.Create(PenStyle);
//    bdmEraser:
//      FDrawObject := TThEraserObject.Create(EraserStyle);
//  end;
//end;

{ TThShapeDrawLayer }

constructor TShapeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  MouseEvents := True;
  Scaled := True;

  FDrawItems := TObjectList<TThRectangleItem>.Create(True);
end;

procedure TShapeDrawLayer.CreateDrawItem;
var
  Poly: TThPoly;
  Item: TThRectangleItem;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  Item := TThRectangleItem.Create(FloatRect(FDownPos, FCurrPos), Poly, 6, clGreen32, 200);
  FDrawItems.Add(Item);
end;

destructor TShapeDrawLayer.Destroy;
begin
  FDrawItems.Clear;
  FDrawItems.Free;

  inherited;
end;

procedure TShapeDrawLayer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmSelection then
    Exit;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    FDownPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  end;
end;

procedure TShapeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CurrP, LastP: TFloatPoint; // Adjusted point
begin
  if FCanvasMode <> cmSelection then
    Exit;

  if FMouseDowned then
  begin
    // Viewport to Local
    CurrP := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    DebugMousePos('ShapeDraw', PointF(X, Y));

    FCurrPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    Update;
  end;

end;

procedure TShapeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FCanvasMode <> cmSelection then
    Exit;

  if FMouseDowned then
  begin
    FMouseDowned := False;
    CreateDrawItem;
  end;
end;

procedure TShapeDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;
//  Buffer.ClipRect := GetAdjustedLocation.Rect;

  if FMouseDowned then
    PaintDrawItem(Buffer, Scale, Offset);
end;

procedure TShapeDrawLayer.PaintDrawItem(Buffer: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Rectangle(FloatRect(FDownPos, FCurrPos));
  ScalePolygonInplace(Poly, AScale.X, AScale.Y);
  TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

  PolygonFS(Buffer, Poly, clBlue32);

  PolylineFS(Buffer, Poly, clGray32, True, 6);
end;

procedure TShapeDrawLayer.PaintDrawItems(Buffer: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Color: TColor32;
  Alpha: Byte;
  Item: TThRectangleItem;
  Poly: TThPoly;
begin
  Buffer.BeginUpdate;
  for Item in FDrawItems do
  begin
    Color := Item.Color;
    Alpha := Item.Alpha;
    ModifyAlpha(Color, Alpha);

    Poly := ScalePolygon(Item.Poly, AScale.X, AScale.Y);
    TranslatePolygonInplace(Poly, AOffset.X, AOffset.Y);

    PolygonFS(Buffer, Poly, Item.Color);

    PolylineFS(Buffer, Poly, clGray32, True, 6);
  end;
  Buffer.EndUpdate;

end;

procedure TShapeDrawLayer.SetCanvasMode(AMode: TThCanvasMode);
begin
  FCanvasMode := AMode;
end;

procedure TShapeDrawLayer.Clear;
begin

end;

end.
