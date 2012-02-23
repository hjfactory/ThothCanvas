unit ThothCommands;

interface

uses
  System.Classes, System.Types,
  ThothTypes, ThothObjects;

type
///////////////////////////////////////////////////////
// Command
  TThCommand = class;
  TThCommand = class(TInterfacedObject, IThCommand)
  end;

  TThShapeCommand = class(TThCommand)
  private
    FShapeList: TList;
    function GetShapeCount: Integer;
    function Get(Index: Integer): TThShape;
  public
    constructor Create; overload;
    constructor Create(AShape: TThShape); overload;
    constructor Create(AShapes: TList); overload;
    destructor Destroy; override;

    procedure AddShape(AShape: TThShape);

    property ShapeCount: Integer read GetShapeCount;

    property Items[Index: Integer]: TThShape read Get; default;

    property List: TList read FShapeList;
  end;

  TThInsertShapeCommand = class(TThShapeCommand);
  TThRestoreShapeCommand = class(TThShapeCommand);
  TThDeleteShapeCommand = class(TThShapeCommand);
  TThRemoveShapeCommand = class(TThShapeCommand);
  TThMoveShapeCommand = class(TThShapeCommand)
  private
    FBeforePos: TPointF;
    FAfterPos: TPointF;
  public
    constructor Create(AShapes: TList; ABefore, AAfter: TPointF); overload;

    property BeforePos: TPointF read FBeforePos write FBeforePos;
    property AfterPos: TPointF read FAfterPos write FAfterPos;
  end;
  TThScaleShapeCommand = class(TThShapeCommand);

  TThEditCommand = class(TThCommand)

  end;

  TThUndoCommand = class(TThEditCommand);
  TThRedoCommand = class(TThEditCommand);

  TThStyleCommand = class(TThCommand)

  end;


implementation

uses
  WinAPI.Windows;

{ TThShapeCommand }

constructor TThShapeCommand.Create;
begin
  FShapeList := TList.Create;
end;

constructor TThShapeCommand.Create(AShapes: TList);
begin
  Create;

  FShapeList.Assign(AShapes);
end;

constructor TThShapeCommand.Create(AShape: TThShape);
begin
  Create;

  FShapeList.Add(AShape);
end;

destructor TThShapeCommand.Destroy;
var
  I: Integer;
begin
  if Assigned(FShapeList) then
    FShapeList.Free;

  OutputDebugString(PChar('Command Destroy: ' + Self.ClassName));

  inherited;
end;

function TThShapeCommand.Get(Index: Integer): TThShape;
begin
  Result := nil;
  if FShapeList.Count > Index then
    Result := TThShape(FShapeList[Index]);
end;

function TThShapeCommand.GetShapeCount: Integer;
begin
  Result := FShapeList.Count;
end;

procedure TThShapeCommand.AddShape(AShape: TThShape);
begin
  FShapeList.Add(AShape)
end;

{ TThMoveShapeCommand }

constructor TThMoveShapeCommand.Create(AShapes: TList;
  ABefore, AAfter: TPointF);
begin
  Create(AShapes);

  FBeforePos := ABefore;
  FAfterPos := AAfter;
end;

end.
