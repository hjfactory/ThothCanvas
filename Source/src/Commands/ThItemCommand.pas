unit ThItemCommand;

interface

uses
  FMX.Types, System.Types, System.SysUtils,
  ThTypes, ThClasses, ThItem;

type
  TThItemCommand = class(TInterfacedObject, IThCommand)
  protected
    FItems: TThItems;
  public
    constructor Create(AItem: TThItem); overload;
    constructor Create(AItems: TThItems); overload;
    destructor Destroy; override;

    procedure Execute; virtual; abstract;
    procedure Rollback; virtual; abstract;
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
end;

constructor TThItemCommand.Create(AItem: TThItem);
begin
  FItems := TThitems.Create;
  FItems.Add(AItem);
end;

destructor TThItemCommand.Destroy;
begin
  FItems.Free;

  inherited;
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
begin
  Item := FItems[0];
  Item.Parent := Item.BeforeParent;
  Item.Visible := True;
  Item.Selected := True;
//  Item.Repaint;
end;

procedure TThCommandItemAdd.Rollback;
var
  Item: TThItem;
begin
  Item := FItems[0];
  Item.Selected := False;
  Item.BeforeParent := Item.Parent;
  Item.Parent := nil;
  Item.Visible := False;
end;

{ TThCommandItemDelete }

constructor TThCommandItemDelete.Create(AParent: TControl; AItems: TThItems);
begin
  inherited Create(AItems);

  FParent := AParent;
end;

procedure TThCommandItemDelete.Execute;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].BeforeParent := FItems[I].Parent;
    FItems[I].Parent := nil;
    FItems[I].Visible := False;
    FItems[I].Selected := False;
  end;
end;

procedure TThCommandItemDelete.Rollback;
var
  I: Integer;
  Canvas: TThCanvasEditor;
begin
  Canvas := TThCanvasEditor(FParent);

  Canvas.ClearSelection;
  Canvas.BeginSelect;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      FItems[I].Parent := FItems[I].BeforeParent;
      FItems[I].Index := FItems[I].BeforeIndex;
      FItems[I].Visible := True;
      FItems[I].Selected := True;
    end;
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

procedure TThCommandItemMove.Execute;
var
  I: Integer;
  Item: TThItem;
  CurrParent: TFmxObject;
  CurrIndex: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Item        := FItems[I];
    CurrParent  := Item.Parent;
    CurrIndex   := Item.Index;

    Item.Parent := Item.BeforeParent;
    Item.Index  := Item.BeforeIndex;
    Item.BeforeParent := CurrParent;
    Item.BeforeIndex  := CurrIndex;
    Item.Position.Point := Item.Position.Point.Add(FDistance);
    if Item.Parent is TThItem then
    Item.Position.Point := Item.Position.Point.Subtract(TThItem(Item.Parent).GetAbsolutePoint);
//    if Item.Parent is TThItem then
//      TThItem(Item.Parent).Contain(Item);
//    FItems[I].ParentCanvas.DoGrouping(FItems[I]);
  end;
end;

procedure TThCommandItemMove.Rollback;
var
  I: Integer;
  Item: TThItem;
  CurrParent: TFmxObject;
  CurrIndex: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Item        := FItems[I];
    CurrParent  := Item.Parent;
    CurrIndex   := Item.Index;

    if CurrParent is TThItem then
      Item.Position.Point := Item.GetAbsolutePoint;
    Item.Parent := Item.BeforeParent;
    Item.Index  := Item.BeforeIndex;
    Item.BeforeParent := CurrParent;
    Item.BeforeIndex  := CurrIndex;
    Item.Position.Point := Item.Position.Point.Subtract(FDistance);
//    FItems[I].ParentCanvas.DoGrouping(FItems[I]);
  end;
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
  Item: TThItem;
  CurrParent: TFmxObject;
  CurrIndex: Integer;
begin
  Item := FItems[0];

  CurrParent  := Item.Parent;
  CurrIndex   := Item.Index;

  Item.Parent := Item.BeforeParent;
  Item.Index  := Item.BeforeIndex;
  Item.BeforeParent := CurrParent;
  Item.BeforeIndex  := CurrIndex;
//
  Item.SetBounds(FAfterRect.Left, FAfterRect.Top, FAfterRect.Width, FAfterRect.Height);
  Item.RealignSpot;
  Item.ParentCanvas.DoGrouping(Item);
end;

procedure TThCommandItemResize.Rollback;
var
  Item: TThItem;
  CurrParent: TFmxObject;
  CurrIndex: Integer;
begin
  Item := FItems[0];

  CurrParent  := Item.Parent;
  CurrIndex   := Item.Index;

  Item.Parent := Item.BeforeParent;
  Item.Index  := Item.BeforeIndex;
  Item.BeforeParent := CurrParent;
  Item.BeforeIndex  := CurrIndex;

  Item.SetBounds(FBeforeRect.Left, FBeforeRect.Top, FBeforeRect.Width, FBeforeRect.Height);
  Item.RealignSpot;
  Item.ParentCanvas.DoGrouping(Item);
end;

end.
