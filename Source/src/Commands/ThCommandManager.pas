unit ThCommandManager;

interface

uses
  ThTypes, ThClasses, System.Generics.Collections;

type
  TThCommandManager = class(TThInterfacedObject, IThObserver)
  type
    TThCommandStack = TStack<IThCommand>;
  private
    FSubject: IThSubject;

    FUndoStack: TThCommandStack;
    FRedoStack: TThCommandStack;
    function GetRedoCount: Integer;
    function GetUndoCount: Integer;

    procedure ClearRedoCommand;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure UndoAction;
    procedure RedoAction;

    property UndoCount: Integer read GetUndoCount;
    property RedoCount: Integer read GetRedoCount;
  end;

implementation

uses
  ThItemCommand, ThSystemCommand;

{ TThCommandHistory }

constructor TThCommandManager.Create;
begin
  FUndoStack := TThCommandStack.Create;
  FRedoStack := TThCommandStack.Create;
end;

destructor TThCommandManager.Destroy;
begin
  FSubject.UnregistObserver(Self);

  ClearRedoCommand;

  FUndoStack.Clear;
  FRedoStack.Clear;
  FUndoStack.Free;
  FRedoStack.Free;

  inherited;
end;

function TThCommandManager.GetRedoCount: Integer;
begin
  Result := FRedoStack.Count;
end;

function TThCommandManager.GetUndoCount: Integer;
begin
  Result := FUndoStack.Count;
end;

procedure TThCommandManager.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;
  FSubject.RegistObserver(Self);
end;

procedure TThCommandManager.Notifycation(ACommand: IThCommand);
begin
  FUndoStack.Push(ACommand);

  // Undo된 TThCommandItemAdd 커맨드의 Items는 신규 커맨드 요청 시 해제(Free)
  ClearRedoCommand;
end;

procedure TThCommandManager.UndoAction;
var
  Command: IThCommand;
begin
  if FUndoStack.Count = 0 then
    Exit;

  Command := FUndoStack.Pop;
  if not Assigned(Command) then
    Exit;

  Command.Rollback;
  FRedoStack.Push(Command);
end;

procedure TThCommandManager.RedoAction;
var
  Command: IThCommand;
begin
  if FRedoStack.Count = 0 then
    Exit;

  Command := FRedoStack.Pop;
  if not Assigned(Command) then
    Exit;

  Command.Execute;
  FUndoStack.Push(Command);
end;

procedure TThCommandManager.ClearRedoCommand;
var
  I: Integer;
  Command: IThCommand;
begin
  for I := 0 to FRedoStack.Count - 1 do
  begin
    Command := FRedoStack.Pop;
    if Command is TThCommandItemAdd then
      FSubject.Subject(Self, TThCommandSystemItemDestroy.Create(TThCommandItemAdd(Command).Items));
  end;

  // Undo된(FRedoStack에 위치한) 커맨드들은 새로운 커맨드 요청 시 Clear
  FRedoStack.Clear;
end;

end.

