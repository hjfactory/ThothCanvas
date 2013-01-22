unit ThItemCommand;

interface

uses
  FMX.Types, System.Types, System.SysUtils,
  ThTypes, ThClasses, ThItem, System.Generics.Collections;

type
  TUpdateState = record
    Item: TThItem;
    Parent, BeforeParent: TFmxObject;
    Index, BeforeIndex: Integer;
  public
    constructor Create(AItem: TThItem);
  end;

  TThItemCommand = class(TInterfacedObject, IThCommand)
  private
    procedure UpdateState;
  protected
    FItems: TThItems;
    FUpdateStateItems: TList<TUpdateState>;
  public
    constructor Create(AItem: TThItem); overload;
    constructor Create(AItems: TThItems); overload;
    destructor Destroy; override;

    procedure Execute; virtual; abstract;   // BeforeParent로 처리
    procedure Rollback; virtual; abstract;  // ItemHistory에서 Parent 찾기
  end;

  TThCommandItemAdd = class(TThItemCommand)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItem: TThItem); overload;

    procedure Execute; override;
    procedure Rollback; override;

    property Items: TThItems read FItems;
  end;

  TThCommandItemDelete = class(TThItemCommand)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItems: TThItems); overload;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  TThCommandItemMove = class(TThItemCommand)
  private
    FDistance: TPointF;
  public
    constructor Create(AItems: TThItems; ADistance: TPointF); overload;
    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  TThCommandItemResize = class(TThItemCommand)
  private
    FBeforeRect,
    FAfterRect: TRectF;
  public
    constructor Create(AItem: TThItem; ABeforeRect: TRectF); overload;

    procedure Execute; override;
    procedure Rollback; override;
  end;

implementation

uses
  ThCanvasEditor;

{ TThAbstractCommandItem }

constructor TThItemCommand.Create(AItems: TThItems);
begin
  FItems := TThItems.Create(AItems);

  UpdateState;
end;

constructor TThItemCommand.Create(AItem: TThItem);
begin
  FItems := TThitems.Create;
  FItems.Add(AItem);

  UpdateState;
end;

destructor TThItemCommand.Destroy;
begin
  FItems.Free;
  if Assigned(FUpdateStateItems) then

  FUpdateStateItems.Free;

  inherited;
end;

procedure TThItemCommand.UpdateState;
var
  Item, Child: TThItem;
begin
  FUpdateStateItems := TList<TUpdateState>.Create;
  for Item in FItems do
  begin
    if Item.Parent <> Item.BeforeParent then
    begin
      FUpdateStateItems.Add(TUpdateState.Create(Item));
      Item.BeforeParent := Item.Parent;
    end;

    for Child in Item.LastContainItems do
    begin
      if Child.Parent <> Child.BeforeParent then
      begin
        FUpdateStateItems.Add(TUpdateState.Create(Child));
        Child.BeforeParent := Child.Parent;
      end;
    end;
  end;
end;

{ TThCommandItemAdd }

constructor TThCommandItemAdd.Create(AParent: TControl; AItem: TThItem);
begin
  inherited Create(AItem);

  FParent := AParent;
end;

procedure TThCommandItemAdd.Execute;
var
  Item: TThItem;
  UpdateState: TUpdateState;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.Parent.InsertObject(UpdateState.Index, UpdateState.Item);

  for Item in FItems do
    Item.Selected := True;
end;

procedure TThCommandItemAdd.Rollback;
var
  Item: TThItem;
  UpdateState: TUpdateState;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.BeforeParent.InsertObject(UpdateState.BeforeIndex, UpdateState.Item);

  for Item in FItems do
  begin
    Item.Selected := False;
    Item.Parent := nil;
  end;
end;

{ TThCommandItemDelete }

constructor TThCommandItemDelete.Create(AParent: TControl; AItems: TThItems);
begin
  inherited Create(AItems);

  FParent := AParent;
end;

procedure TThCommandItemDelete.Execute;
var
  Item: TThItem;
  UpdateState: TUpdateState;
begin
  for UpdateState in FUpdateStateItems do
  begin
    UpdateState.Item.Parent := UpdateState.Parent;
    UpdateState.Item.Index := UpdateState.Index;
  end;

  for Item in FItems do
  begin
    Item.Parent := nil;
    Item.Selected := False;
  end;
end;

procedure TThCommandItemDelete.Rollback;
var
  I: Integer;
  Item: TThItem;
  Canvas: TThCanvasEditor;
  UpdateState: TUpdateState;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.BeforeParent.InsertObject(UpdateState.BeforeIndex, UpdateState.Item);

  Canvas := TThCanvasEditor(FParent);
  Canvas.ClearSelection;
  Canvas.BeginSelect;
  try
    for Item in FItems do
      Item.Selected := True;
  finally
    Canvas.EndSelect;
  end;
end;

{ TThCommandItemMove }

constructor TThCommandItemMove.Create(AItems: TThItems; ADistance: TPointF);
begin
  inherited Create(AItems);

  FDistance := ADistance;
end;

destructor TThCommandItemMove.Destroy;
begin

  inherited;
end;

procedure TThCommandItemMove.Execute;
var
  Item: TThItem;
  UpdateState: TUpdateState;
begin
  for Item in FItems do
    Item.Position.Point := Item.Position.Point.Add(FDistance);

  for UpdateState in FUpdateStateItems do
    UpdateState.Parent.InsertObject(UpdateState.Index, UpdateState.Item);
end;

procedure TThCommandItemMove.Rollback;
var
  Item: TThItem;
  UpdateState: TUpdateState;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.BeforeParent.InsertObject(UpdateState.BeforeIndex, UpdateState.Item);

  for Item in FItems do
    Item.Position.Point := Item.Position.Point.Subtract(FDistance);
end;

{ TThCommandItemResize }

constructor TThCommandItemResize.Create(AItem: TThItem; ABeforeRect: TRectF);
begin
  inherited Create(AItem);

  FBeforeRect := ABeforeRect;
  FAfterRect := AItem.BoundsRect;
  FAfterRect.Offset(AItem.Position.Point);
end;

procedure TThCommandItemResize.Execute;
var
  UpdateState: TUpdateState;
  Item: TThItem;
  ItemContainer: IThItemContainer;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.Parent.InsertObject(UpdateState.Index, UpdateState.Item);

  for Item in FItems do
  begin
    Item.SetBounds(FAfterRect.Left, FAfterRect.Top, FAfterRect.Width, FAfterRect.Height);
    Item.RealignSpot;

    Item.ReleaseChildren;
    if Supports(Item.Parent, IThItemContainer, ItemContainer) then
      ItemContainer.ContainChildren(Item);
  end;
end;

procedure TThCommandItemResize.Rollback;
var
  UpdateState: TUpdateState;
  Item: TThItem;
  ItemContainer: IThItemContainer;
begin
  for UpdateState in FUpdateStateItems do
    UpdateState.BeforeParent.InsertObject(UpdateState.BeforeIndex, UpdateState.Item);

  for Item in FItems do
  begin
    Item.SetBounds(FBeforeRect.Left, FBeforeRect.Top, FBeforeRect.Width, FBeforeRect.Height);
    Item.RealignSpot;

    Item.ReleaseChildren;
    if Supports(Item.Parent, IThItemContainer, ItemContainer) then
      ItemContainer.ContainChildren(Item);
  end;
end;

{ TUpdateData }

constructor TUpdateState.Create(AItem: TThItem);
begin
  Item := AItem;
  BeforeParent := AItem.BeforeParent;
  Parent := AItem.Parent;
  BeforeIndex := AItem.BeforeIndex;
  Index := AItem.Index;
end;

end.
