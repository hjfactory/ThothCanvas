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
  end;

implementation

{ TThAbstractCommandItem }

constructor TThAbstractCommandItem.Create(AItems: TThItems);
begin
  FItems := TThItems.Create(AItems);
//  FItems.
//  FItems.Assign(AItems);
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
  FParent := AParent;

  inherited Create(AItem);
end;

procedure TThCommandItemAdd.Execute;
var
  Item: TThItem;
begin
  Item := FItems[0];
  Item.Parent := FParent;
  Item.Visible := True;
end;

procedure TThCommandItemAdd.Rollback;
var
  Item: TThItem;
begin
  Item := FItems[0];
  Item.Parent := nil;
  Item.Visible := False;
end;

end.
