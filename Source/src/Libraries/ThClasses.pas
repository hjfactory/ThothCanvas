unit ThClasses;

interface

uses
  System.Classes, ThTypes, ThItem;

type
  TThItems = class(TList)
  private
    function GetItems(Index: Integer): TThItem;
    procedure SetItems(Index: Integer; const Value: TThItem);
  public
    function Add(Item: TThItem): Integer;
    function Remove(Item: TThItem): Integer;

    property Items[Index: Integer]: TThItem read GetItems write SetItems; default;
  end;

implementation

{ TThItems }

function TThItems.Add(Item: TThItem): Integer;
begin
  Result := inherited Add(Item);
end;

function TThItems.GetItems(Index: Integer): TThItem;
begin
  Result := TThItem(inherited Items[Index]);
end;

function TThItems.Remove(Item: TThItem): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TThItems.SetItems(Index: Integer; const Value: TThItem);
begin
  inherited Items[Index] := Value;
end;

end.
