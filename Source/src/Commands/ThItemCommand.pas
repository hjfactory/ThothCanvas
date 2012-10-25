unit ThItemCommand;

interface

uses
  FMX.Types,
  ThTypes, ThClasses, ThItem;

type
  TThAbstractCommandItem = class(TInterfacedObject, IThCommand)
  protected
    FItems: TThItems;
  public
    constructor Create(AItem: TThItem); overload;
    constructor Create(AItems: TThItems); overload;
    destructor Destroy; override;

    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
  end;

  TThCommandItemAdd = class(TThAbstractCommandItem)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItem: TThItem); overload;

    procedure Undo; override;
    procedure Redo; override;
  end;

  TThCommandItemDelete = class(TThAbstractCommandItem)
  private
    FParent: TControl;
  public
    constructor Create(AParent: TControl; AItems: TThItems); overload;

    procedure Undo; override;
    procedure Redo; override;
  end;

implementation

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

  inherited;
end;

{ TThCommandItemAdd }

constructor TThCommandItemAdd.Create(AParent: TControl; AItem: TThItem);
begin
  inherited Create(AItem);

  FParent := AParent;
end;

procedure TThCommandItemAdd.Undo;
var
  Item: TThItem;
begin
  Item := FItems[0];
  Item.Parent := nil;
  Item.Visible := False;
end;

procedure TThCommandItemAdd.Redo;
var
  Item: TThItem;
begin
  Item := FItems[0];
  Item.Parent := FParent;
  Item.Visible := True;
//  Item.Repaint;
end;

{ TThCommandItemDelete }

constructor TThCommandItemDelete.Create(AParent: TControl; AItems: TThItems);
begin
  inherited Create(AItems);

  FParent := AParent;
end;

procedure TThCommandItemDelete.Undo;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].Parent := FParent;
    FItems[I].Visible := True;
  end;
end;

procedure TThCommandItemDelete.Redo;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[I].Parent := nil;
    FItems[I].Visible := False;
  end;
end;

end.
