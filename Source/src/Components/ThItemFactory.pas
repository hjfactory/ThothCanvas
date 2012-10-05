unit ThItemFactory;

interface

uses
  System.Classes,
  ThItem;

type
  TThItemData = class(TObject)
  private
    FID: Integer;
    FItemClass: TThItemClass;
  public
    constructor Create(ID: Integer; ItemClass: TThItemClass);

    property ID: Integer read FID;
    property ItemClass: TThItemClass read FItemClass;
  end;

  TThItems = class(TObject)
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(ID: Integer; ItemClass: TThItemClass);
    function GetItemClass(ID: Integer): TThItemClass;
  end;

  TThItemFactory = class(TObject)
  private
    FItems: TThItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(ID: Integer; ItemClass: TThItemClass);
    function Get(AID: Integer): TThItem;
  end;

  procedure RegisterItem(ID: Integer; ItemClass: TThItemClass);
  function ItemFactory: TThItemFactory;

implementation

var
  _Factory: TThItemFactory;

procedure RegisterItem(ID: Integer; ItemClass: TThItemClass);
begin
  if not Assigned(_Factory) then
    _Factory := TThItemFactory.Create;
  _Factory.AddItem(ID, ItemClass);
end;

function ItemFactory: TThItemFactory;
begin
  Assert(Assigned(_Factory), 'Not assigned item factory object.');

  Result := _Factory;
end;

{ TThItemData }

constructor TThItemData.Create(ID: Integer; ItemClass: TThItemClass);
begin
  FID := ID;
  FItemClass := ItemClass;
end;

{ TThItems }

constructor TThItems.Create;
begin
  FList := TList.Create;
end;

destructor TThItems.Destroy;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Free;

  inherited;
end;

function TThItems.GetItemClass(ID: Integer): TThItemClass;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FList.Count - 1 do
    if TThItemData(FList[I]).ID = ID then
    begin
      Result := TThItemData(FList[I]).ItemClass;
      Exit;
    end;
end;

procedure TThItems.AddItem(ID: Integer; ItemClass: TThItemClass);
begin
  FList.Add(TThItemData.Create(ID, ItemClass));
end;

{ TThItemFactory }

constructor TThItemFactory.Create;
begin
  FItems := TThItems.Create;
end;

destructor TThItemFactory.Destroy;
begin
  FItems.Free;

  inherited;
end;

procedure TThItemFactory.AddItem(ID: Integer; ItemClass: TThItemClass);
begin
  FItems.AddItem(ID, ItemClass);
end;

function TThItemFactory.Get(AID: Integer): TThItem;
var
  ItemClass: TThItemClass;
begin
  Result := nil;

  ItemClass := FItems.GetItemClass(AID);

  if Assigned(ItemClass) then
    Result := ItemClass.Create(nil);
end;

initialization

finalization
  if Assigned(_Factory) then
    _Factory.Free;

end.