unit ThCommand;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  ThInterface, ThTypes, ThShape;

type
///////////////////////////////////////////////////////
// Command
  TThCommand = class;
  TThCommand = class(TInterfacedObject, IThCommand)
  public
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
    FFromPos: TPointF;
    FToPos: TPointF;
  public
    constructor Create(AShapes: TList; const FromPos, ToPos: TPointF); overload;

    property FromPos: TPointF read FFromPos write FFromPos;
    property ToPos: TPointF read FToPos write FToPos;
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
  const FromPos, ToPos: TPointF);
begin
  Create(AShapes);

  FFromPos := FromPos;
  FToPos := ToPos;
end;

end.
