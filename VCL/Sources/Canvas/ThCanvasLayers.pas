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
    function ViewportToLocal(APoint: TFloatPoint): TFloatPoint;
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
    [unsafe] FDrawObject: IThDrawObject; // �������̽� �ν��Ͻ� ��ü �� �ڵ����� ���� 

    procedure SetDrawStyle(const Value: IThDrawStyle); virtual;

    procedure Paint(Buffer: TBitmap32); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property DrawStyle: IThDrawStyle read FDrawStyle write SetDrawStyle;
  end;

  // �����Ӱ� �׸���(��� ���찳) ���� ���̾�
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

  // ������ �߰��� �׸��� ���̾�
  TShapeDrawLayer = class(TThCustomDrawLayer)
  private
  protected
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  end;

  // ��� ���̾�
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
var
  P: TFloatPoint;
begin
  inherited;

  if Button = mbLeft then
  begin
    P := ViewportToLocal(FloatPoint(X, Y));

    FMouseDowned := True;

    if Assigned(FDrawObject) then
    begin
      FDrawObject.StartMove(P);
      FDrawObject.Move(P);
    end;
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
    P := ViewportToLocal(FloatPoint(X, Y));
    if Assigned(FDrawObject) then
      FDrawObject.Move(P);
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
    FDrawObject.DoneMove;
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
  FEraDrawObj := TThObjErsDrawObject.Create(FEraStyle, FDrawItems); // ��ü ���찳

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

  FDrawObject := TThShapeDrawObject.Create(TThShapeStyle.Create);
end;

destructor TShapeDrawLayer.Destroy;
begin

  inherited;
end;

{ TThBackgroundLayer }

function TThBackgroundLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

end.
