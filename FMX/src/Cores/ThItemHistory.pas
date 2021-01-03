unit ThItemHistory;

interface

uses
  System.Generics.Collections, FMX.Types,
  ThItem;

type
  TThItemHistoryData = record
    Parent: TFmxObject;
    Index: Integer;
    Children: TThItems;
  end;

  TThItemHistory = class(TList<TThItemHistoryData>)
  private
    FStackIndex: Integer;

    function GetBeforeIndex: Integer;
    function GetBeforeParent: TFmxObject;
  public
    constructor Crate;
    destructor Destroy; override;

    procedure Push(AParent: TFmxObject; AIndex: Integer; AChildren: TThItems);
    procedure Undo;
    procedure Redo;

    property BeforeParent: TFmxObject read GetBeforeParent;
    property BeforeIndex: Integer read GetBeforeIndex;
  end;

implementation

{ TThItemHistory }

constructor TThItemHistory.Crate;
begin
  inherited;

  FStackIndex := -1;
end;

destructor TThItemHistory.Destroy;
var
  Data: TThItemHistoryData;
begin
  for Data in List do
  begin
    if Assigned(Data.Children) then
      Data.Children.Free;
  end;

  inherited;
end;

function TThItemHistory.GetBeforeIndex: Integer;
begin
  Result := -1;
  if (FStackIndex > -1) and (Count > FStackIndex) then
    Result := List[FStackIndex].Index;
end;

function TThItemHistory.GetBeforeParent: TFmxObject;
begin
  Result := nil;
  if (FStackIndex > -1) and (Count > FStackIndex) then
    Result := List[FStackIndex].Parent;
end;

procedure TThItemHistory.Push(AParent: TFmxObject; AIndex: Integer;
  AChildren: TThItems);
var
  Data: TThItemHistoryData;
begin
  Data.Parent := AParent;
  Data.Index := AIndex;
  if Assigned(AChildren) then
    Data.Children := TThItems.Create(AChildren);

  Add(Data);
end;

procedure TThItemHistory.Undo;
begin

end;

procedure TThItemHistory.Redo;
begin

end;

end.
