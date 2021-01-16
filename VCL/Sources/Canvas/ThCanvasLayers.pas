{
  Role
    Drawing shapes(Collaborate with ThDrawObject)
    Display drawn shapes(Collaborate with ThDrawItem)
}

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
  TThCustomLayer = class(TPositionedLayer)

  end;

  // Display drawn shapes
  TThCustomViewLayer = class(TThCustomLayer)
  private
    FHitTest: Boolean;
    function GetOffset: TFloatPoint;
    function GetScale: TFloatPoint;
  protected
    FDrawItems: TThDrawItems;

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

  // Drawing shapes
  TThCustomDrawLayer = class(TThCustomViewLayer)
  private
    FMouseDowned: Boolean;

  protected
    FDrawStyle: IThDrawStyle;
    [unsafe] FDrawObject: IThDrawObject;

    procedure SetDrawStyle(const Value: IThDrawStyle); virtual;

    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property DrawStyle: IThDrawStyle read FDrawStyle write SetDrawStyle;
  end;

  // 자유롭게 그리기(펜과 지우개) 위한 레이어
  TFreeDrawLayer = class(TThCustomDrawLayer)
  private
    FDrawMode: TThFreeDrawMode;
    
    FPenStyle: TThPenStyle;
    FEraStyle: TThEraserStyle;

    FPenDrawObj: TThPenDrawObject;
    FEraDrawObj: TThEraserDrawObject;
    
    procedure SetDrawMode(const Value: TThFreeDrawMode);
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function DoHitTest(X, Y: Integer): Boolean; override;
    
    property DrawMode: TThFreeDrawMode read FDrawMode write SetDrawMode;
  end;

  // 도형을 추가해 그리는 레이어
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

  // 배경 레이어
  TThBackgroundLayer = class(TBitmapLayer)
  public
    function DoHitTest(X, Y: Integer): Boolean; override;
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
  Result := FHitTest;
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

  if Button = mbLeft then
  begin
    P := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);

    FMouseDowned := True;

    if Assigned(FDrawObject) then
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
    if Assigned(FDrawObject) then
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
begin
  FDrawStyle := Value;

  TThBrushDrawObject(FDrawObject).DrawStyle := FDrawStyle;
end;

{ TBrushDrawLayer }

constructor TFreeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  // Default DrawStyle
  FPenStyle := TThPenStyle.Create;
  FEraStyle := TThEraserStyle.Create;

  FPenDrawObj := TThPenDrawObject.Create(FPenStyle);
  FEraDrawObj := TThObjErsDrawObject.Create(FEraStyle, FDrawItems); // 객체 지우개

  FDrawObject := FPenDrawObj;

  FMouseDowned := False;
end;

destructor TFreeDrawLayer.Destroy;
begin

  inherited;
end;

function TFreeDrawLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := inherited;
  if Result then
    Result := Assigned(FDrawObject);
end;


procedure TFreeDrawLayer.SetDrawMode(const Value: TThFreeDrawMode);
begin
  FDrawMode := Value;

  case Value of
    fdmPen: 
      FDrawObject := FPenDrawObj;
    fdmEraser: 
      FDrawObject := FEraDrawObj;
  end;
  
  Update;
end;

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
Exit;
  if FCanvasMode <> cmSelection then
    Exit;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    FDownPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
  end;
end;

procedure TShapeDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
Exit;
  if FCanvasMode <> cmSelection then
    Exit;

  if FMouseDowned then
  begin
    // Viewport to Local
    DebugMousePos('ShapeDraw', PointF(X, Y));

    FCurrPos := LayerCollection.ViewportToLocal(FloatPoint(X, Y), True);
    Update;
  end;

end;

procedure TShapeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
Exit;
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

{ TThBackgroundLayer }

function TThBackgroundLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

end.
