{
  ItemStroage 및 CommandManager를 컨트롤
  Form등의 외부인터페이스에서 접근하여 처리 하는 클래스
}
unit ThothController;

interface

uses
  System.Classes,
  ThTypes, ThClasses, ThCommandManager, ThItemStorage;

type
  TThothController = class(TThInterfacedObject, IThSubject)
  private
    FObservers: TInterfaceList;

    FItemStorage: TThItemStorage;
    FCommandManager: TThCommandManager;

    function GetRedoCount: Integer;
    function GetUndoCount: Integer;
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);

    procedure Undo;
    procedure Redo;

    property UndoCount: Integer read GetUndoCount;
    property RedoCount: Integer read GetRedoCount;

    property ItemCount: Integer read GetItemCount;
  end;

implementation

{ TMainController }

constructor TThothController.Create;
begin
  FObservers := TInterfaceList.Create;

  FItemStorage := TThItemStorage.Create;
  FItemStorage.SetSubject(Self);

  FCommandManager := TThCommandManager.Create;
  FCommandManager.SetSubject(Self);
end;

destructor TThothController.Destroy;
begin
  FCommandManager.Free;

  FItemStorage.Free;

  FObservers.Clear;
  FObservers := nil;

  inherited;
end;

function TThothController.GetItemCount: Integer;
begin
  Result := FItemStorage.ItemCount;
end;

function TThothController.GetRedoCount: Integer;
begin
  Result := FCommandManager.RedoCount;
end;

function TThothController.GetUndoCount: Integer;
begin
  Result := FCommandManager.UndoCount;
end;

procedure TThothController.RegistObserver(AObserver: IThObserver);
begin
  FObservers.Add(AObserver);
end;

procedure TThothController.UnregistObserver(AObserver: IThObserver);
begin
  FObservers.Remove(AObserver);
end;

procedure TThothController.Subject(ASource: IThObserver; ACommand: IThCommand);
var
  I: Integer;
begin
  for I := 0 to FObservers.Count - 1  do
    if ASource <> IThObserver(FObservers[I]) then
      IThObserver(FObservers[I]).Notifycation(ACommand);
end;

procedure TThothController.Undo;
begin
  FCommandManager.UndoAction;
end;

procedure TThothController.Redo;
begin
  FCommandManager.RedoAction;
end;

end.
