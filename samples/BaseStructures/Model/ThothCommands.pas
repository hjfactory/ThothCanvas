unit ThothCommands;

interface

uses
  System.Classes,
  ThothTypes, ThothObjects;

type
///////////////////////////////////////////////////////
// Command
  TThCommand = class;
  TThCommand = class(TThInterfacedObject, IThCommand)
  end;

  TThShapeCommand = class(TThCommand)
  private
    FShape: TThShape;
    FShapeList: TList;
    function GetShapeCount: Integer;
    function Get(Index: Integer): TThShape;
  public
    constructor Create; overload;
    constructor Create(AShape: TThShape); overload;
    destructor Destroy; override;

    procedure AddShape(AShape: TThShape);

    property ShapeCount: Integer read GetShapeCount;

    property Items[Index: Integer]: TThShape read Get; default;
  end;

  TThInsertShapeCommand = class(TThShapeCommand)

  end;

  TThDeleteShapeCommand = class(TThShapeCommand)

  end;

  TThMoveShapeCommand = class(TThShapeCommand)

  end;

  TThScaleShapeCommand = class(TThShapeCommand)

  end;

  TThStyleCommand = class(TThCommand)

  end;


implementation

{ TThShapeCommand }

constructor TThShapeCommand.Create;
begin
end;

constructor TThShapeCommand.Create(AShape: TThShape);
begin
  Create;

  FShape := AShape;
end;

destructor TThShapeCommand.Destroy;
var
  I: Integer;
begin
  if Assigned(FShapeList) then
  begin
    for I := FShapeList.Count - 1 downto 0 do
      TObject(FShapeList[I]).Free;

    FShapeList.Free;
  end;

  inherited;
end;

function TThShapeCommand.Get(Index: Integer): TThShape;
begin
  Result := nil;
  if Assigned(FShape) and (Index = 0) then
    Result := FShape
  else if FShapeList.Count > Index then
    Result := TThShape(FShapeList[Index])
  ;
end;

function TThShapeCommand.GetShapeCount: Integer;
begin
  Result := 0;

  if Assigned(FShapeList) and (FShapeList.Count > 1) then
    Result := FShapeList.Count
  else if Assigned(FShape) then
    Result := 1
  ;
end;

procedure TThShapeCommand.AddShape(AShape: TThShape);
begin
  if not Assigned(FShapeList) then
    FShapeList := TList.Create;

  FShapeList.Add(AShape)
end;

end.
