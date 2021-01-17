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

  GR32, GR32_Layers, GR32_Polygons, GR32_VectorUtils,
  clipper,

  ThTypes, ThClasses,
  ThDrawItem, ThDrawStyle, ThDrawObject;

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
    function ViewportToLocal(APoint: TFloatPoint): TFloatPoint; overload;
    function ViewportToLocal(AX, AY: TFloat): TFloatPoint; overload;
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
    [unsafe] FDrawObject: IThDrawObject; // 인터페이스 인스턴스 교체 시 자동해제 방지 

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
    
    FPenStyle: IThDrawStyle;
    FEraStyle: IThDrawStyle;

    FPenDrawObj: IThDrawObject;
    FEraDrawObj: IThDrawObject;
    
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
    FShapeDrawObj: TThShapeDrawObject;
    FSelectObj: TThShapeSelectObject;
    FDrawMode: TThShapeDrawMode;
    procedure SetDrawMode(const Value: TThShapeDrawMode);
  protected
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    property DrawMode: TThShapeDrawMode read FDrawMode write SetDrawMode;
    property Selection: TThShapeSelectObject read FSelectObj;
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
  ThUtils;


{ TThCustomViewLayer }

constructor TThCustomViewLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FDrawItems := TThDrawItems.Create(True);

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

function TThCustomViewLayer.ViewportToLocal(AX, AY: TFloat): TFloatPoint;
begin
  Result := FloatPoint(AX, AY);
end;

function TThCustomViewLayer.ViewportToLocal(APoint: TFloatPoint): TFloatPoint;
begin
  Result := LayerCollection.ViewportToLocal(APoint, True)
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
begin
  inherited;

  if Button = mbLeft then
  begin
    FMouseDowned := True;

    if Assigned(FDrawObject) then
      FDrawObject.Start(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDowned then
  begin
    if Assigned(FDrawObject) then
      FDrawObject.Move(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Item: TObject;
begin
  inherited;

  if FMouseDowned then
  begin
    FMouseDowned := False;
    Item := FDrawObject.CreateItem;
    if Assigned(Item) then
       FDrawItems.Add(Item as TThDrawItem);
    FDrawObject.Done(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;

  if FMouseDowned and Assigned(FDrawObject) then
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

  FPenStyle := TThPenStyle.Create;
  FPenDrawObj := TThPenDrawObject.Create(FPenStyle);
  FEraStyle := TThEraserStyle.Create;
  FEraDrawObj := TThObjErsDrawObject.Create(FEraStyle, FDrawItems); // 객체 지우개

  FDrawObject := FPenDrawObj;

  FMouseDowned := False;
end;

destructor TFreeDrawLayer.Destroy;
begin
  FPenDrawObj := nil;
  FPenStyle := nil;
  FEraDrawObj := nil;
  FEraStyle := nil;

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

  FSelectObj := TThShapeSelectObject.Create(FDrawItems);
  FShapeDrawObj := TThShapeDrawObject.Create(TThShapeStyle.Create);

  FDrawObject := FSelectObj;
end;

destructor TShapeDrawLayer.Destroy;
begin

  inherited;
end;

procedure TShapeDrawLayer.SetDrawMode(const Value: TThShapeDrawMode);
begin
  FDrawMode := Value;

  case Value of
    sdmSelect: FDrawObject := FSelectObj;
    sdmDraw: FDrawObject := FShapeDrawObj;
  end;
end;

{ TThBackgroundLayer }

function TThBackgroundLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

end.
