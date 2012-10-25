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
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure UndoAction;
    procedure RedoAction;
  end;

implementation

{ TThCommandHistory }

constructor TThCommandManager.Create;
begin
  FUndoStack := TThCommandStack.Create;
  FRedoStack := TThCommandStack.Create;
end;

destructor TThCommandManager.Destroy;
begin
  FUndoStack.Clear;
  FRedoStack.Clear;
  FUndoStack.Free;
  FRedoStack.Free;

  inherited;
end;

procedure TThCommandManager.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;
  FSubject.RegistObserver(Self);
end;

procedure TThCommandManager.Notifycation(ACommand: IThCommand);
begin
  FUndoStack.Push(ACommand);
end;

procedure TThCommandManager.UndoAction;
var
  Command: IThCommand;
begin
  Command := FUndoStack.Pop;
  if not Assigned(Command) then
    Exit;

  Command.Undo;
  FRedoStack.Push(Command);
end;

procedure TThCommandManager.RedoAction;
var
  Command: IThCommand;
begin
  Command := FRedoStack.Pop;
  if not Assigned(Command) then
    Exit;

  Command.Redo;
  FUndoStack.Push(Command);
end;

end.
