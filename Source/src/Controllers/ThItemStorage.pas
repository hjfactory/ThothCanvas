unit ThItemStorage;

interface

uses
  ThTypes, ThClasses, ThItem;

type
  TThItemStorage = class(TThInterfacedObject, IThObserver)
  private
    FSubject: IThSubject;
    FItems: TThItems;
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    property ItemCount: Integer read GetItemCount;
  end;

implementation

uses
  ThItemCommand, ThSystemCommand;

{ TThItemStorage }

constructor TThItemStorage.Create;
begin
  FItems := TThItems.Create;
end;

destructor TThItemStorage.Destroy;
begin
  FSubject.UnregistObserver(Self);

  FItems.Free;

  inherited;
end;

function TThItemStorage.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TThItemStorage.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;
  FSubject.RegistObserver(Self);
end;

procedure TThItemStorage.Notifycation(ACommand: IThCommand);
var
  I: Integer;
  Item: TThItem;
begin
  if ACommand is TThCommandItemAdd then
  begin
    // 중복되는 일은 없겠지??
    FItems.AddRange(TThCommandItemAdd(ACommand).Items);
  end
  else if ACommand is TThCommandSystemItemDestroy then
  begin
    for I := 0 to TTHCommandSystemItemDestroy(ACommand).Items.Count - 1 do
    begin
      Item := TTHCommandSystemItemDestroy(ACommand).Items[I];
      if not Assigned(Item) then
        Continue;
      FItems.Remove(Item);
      Item.Free;
    end;
  end;
end;

end.
