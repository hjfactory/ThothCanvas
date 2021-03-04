{
  Role
    Drawing shapes(Collaborate with ThDrawObject)
    Display drawn shapes(Collaborate with ThItemList)
}

unit ThCanvasLayers;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  Vcl.Controls,

  GR32, GR32_Layers, GR32_Polygons, GR32_VectorUtils,

  ThTypes, ThClasses,
  ThItemCollections,
  ThDrawObject, ThItemStyle;

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
    FItemList: TThItemList;

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
    FDrawMode: TThDrawMode;
    [unsafe] FDrawObject: IThDrawObject; // 인터페이스 인스턴스 교체 시 자동해제 방지

    procedure SetDrawMode(const Value: TThDrawMode); virtual;
    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    property DrawMode: TThDrawMode read FDrawMode write SetDrawMode;
  end;

  // 자유롭게 그리기(펜과 지우개) 위한 레이어
  TPenDrawLayer = class(TThCustomDrawLayer)
  private
    FPenDrawObj: TThPenDrawObject;
    FEraDrawObj: TThObjectEraserObject;
  protected
    procedure SetDrawMode(const Value: TThDrawMode); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function DoHitTest(X, Y: Integer): Boolean; override;

    property PenDrawObj: TThPenDrawObject read FPenDrawObj;
  end;

  // 도형을 추가해 그리는 레이어
  TShapeDrawLayer = class(TThCustomDrawLayer)
  private
    FShapeId: string;

    FSelectObj: TThSelectObject;
    FShapeDrawObj: TThShapeDrawObject;

    procedure SetShapetId(const Value: string);
  protected
    procedure SetDrawMode(const Value: TThDrawMode); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    property ShapeId: string read FShapeId write SetShapetId;
//    property Selection: TThShapeSelectObject read FSelectObj;
    procedure DeleteSelectedItems;
  end;

  // 배경 레이어
  TThBackgroundLayer = class(TBitmapLayer)
  public
    function DoHitTest(X, Y: Integer): Boolean; override;
  end;

implementation

uses
  ThUtils;

{ TThCustomViewLayer }

constructor TThCustomViewLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FItemList := TThItemList.Create;

  Scaled := True;
end;

destructor TThCustomViewLayer.Destroy;
begin
  FItemList.Clear;
  FItemList.Free;

  inherited;
end;

procedure TThCustomViewLayer.Clear;
begin
  FItemList.Clear;
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
  I: Integer;
  Item: IThItem;
  LScale, LOffset: TFloatPoint;
begin
  inherited;

  LScale := Scale;
  LOffset := Offset;

  Buffer.BeginUpdate;
  for I := 0 to FItemList.Count - 1 do
    FItemList[I].Draw(Buffer, LScale, LOffset);
  Buffer.EndUpdate;
end;

function TThCustomViewLayer.ViewportToLocal(AX, AY: TFloat): TFloatPoint;
begin
  Result := ViewportToLocal(FloatPoint(AX, AY));
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
  if Button = mbLeft then
  begin
    FMouseDowned := True;

    if Assigned(FDrawObject) then
      FDrawObject.MouseDown(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FDrawObject) then
  begin
    FDrawObject.MouseMove(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Item: IThItem;
begin
  if FMouseDowned then
  begin
    FMouseDowned := False;

    Item := FDrawObject.ItemInstance;

    if Assigned(Item) then
       FItemList.Add(Item);

    FDrawObject.MouseUp(ViewportToLocal(X, Y), Shift);
    Update;
  end;
end;

procedure TThCustomDrawLayer.Paint(Buffer: TBitmap32);
begin
  inherited;

  if FMouseDowned and Assigned(FDrawObject) then
    FDrawObject.Draw(Buffer, Scale, Offset);
end;

procedure TThCustomDrawLayer.SetDrawMode(const Value: TThDrawMode);
begin
  FDrawMode := Value;
end;

{ TPenLayer }

constructor TPenDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FPenDrawObj := TThPenDrawObject.Create(TThPenStyle.Create);
  FEraDrawObj := TThObjectEraserObject.Create(TThEraserStyle.Create, FItemList);

  FDrawObject := FPenDrawObj;

  FMouseDowned := False;
end;

destructor TPenDrawLayer.Destroy;
begin
  FPenDrawObj.Free;
  FEraDrawObj.Free;

  inherited;
end;

function TPenDrawLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := inherited;
  if Result then
    Result := Assigned(FDrawObject);
end;

procedure TPenDrawLayer.SetDrawMode(const Value: TThDrawMode);
begin
  inherited;

  case Value of
    dmPen:    FDrawObject := FPenDrawObj;
    dmEraser: FDrawObject := FEraDrawObj;
  end;

  Update;
end;

{ TThShapeDrawLayer }

constructor TShapeDrawLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FSelectObj := TThSelectObject.Create(FItemList);
  FShapeDrawObj := TThShapeDrawObject.Create(TThShapeStyle.Create);

  ShapeId := 'Rect';
  FDrawObject := FSelectObj;
end;

procedure TShapeDrawLayer.DeleteSelectedItems;
begin
  FSelectObj.DeleteSelectedItems;
  Update;
end;

destructor TShapeDrawLayer.Destroy;
begin
  FSelectObj.Free;
  FShapeDrawObj.Free;

  inherited;
end;

procedure TShapeDrawLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if FDrawMode = dmDraw then
    DrawMode := dmSelect;
end;

procedure TShapeDrawLayer.SetDrawMode(const Value: TThDrawMode);
begin
  inherited;

  case Value of
    dmSelect:  FDrawObject := FSelectObj;
    dmDraw:    FDrawObject := FShapeDrawObj;
  end;
end;

procedure TShapeDrawLayer.SetShapetId(const Value: string);
begin
  if FShapeId = Value then
    Exit;

  FShapeId := Value;

  FShapeDrawObj.ShapeId := FShapeId;
end;

{ TThBackgroundLayer }

function TThBackgroundLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

end.
