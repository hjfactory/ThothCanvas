unit ThItemCommand;

interface

uses
  FMX.Types, System.Types,
  ThTypes, ThClasses, ThItem;

type
  TThAbstractCommandItem = class(TInterfacedObject, IThCommand)
  protected
    FItems: TThItems;
  public
    constructor Create(AItem: TThItem); overload;
    constructor Create(AItems: TThItems); overload;
    destructor Destroy; override;

    procedure Execute; virtual; abstract;
    procedure Rollback; virtual; abstract;
  end;

  TThCommandItemAdd = class(TThAbstractCommandItem)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItem: TThItem); overload;

    procedure Execute; override;
    procedure Rollback; override;

    property Items: TThItems read FItems;
  end;

  TThCommandItemDelete = class(TThAbstractCommandItem)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItems: TThItems); overload;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  TThCommandItemMove = class(TThAbstractCommandItem)
  private
    FDistance: TPointF;
  public
    constructor Create(AItems: TThItems; ADistance: TPointF); overload;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  TThCommandItemResize = class(TThAbstractCommandItem)
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
  CommonUtils, ThCanvasEditor;

{ TThAbstractCommandItem }

constructor TThAbstractCommandItem.Create(AItems: TThItems);
begin
  FItems := TThItems.Create(AItems);
end;

constructor TThAbstractCommandItem.Create(AItem: TThItem);
begin
  FItems := TThitems.Create;
  FItems.Add(AItem);
end;

destructor TThAbstractCommandItem.Destroy;
begin
  FItems.Free;

  Debug('Destroy Command');

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
  Item.Parent := FParent;
  Item.Visible := True;
//  Item.Repaint;
end;

procedure TThCommandItemAdd.Rollback;
var
  Item: TThItem;
begin
  Item := FItems[0];
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
      FItems[I].Parent := FParent;
      FItems[I].Index := FItems[I].Tag;
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
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].Position.Point := FItems[I].Position.Point.Add(FDistance);
end;

procedure TThCommandItemMove.Rollback;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].Position.Point := FItems[I].Position.Point.Subtract(FDistance);
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
begin
  Item := FItems[0];

  Item.SetBounds(FAfterRect.Left, FAfterRect.Top, FAfterRect.Width, FAfterRect.Height);
  Item.RealignSpot;
end;

procedure TThCommandItemResize.Rollback;
var
  Item: TThItem;
begin
  Item := FItems[0];

  Item.SetBounds(FBeforeRect.Left, FBeforeRect.Top, FBeforeRect.Width, FBeforeRect.Height);
  Item.RealignSpot;
end;

end.
