unit ThothController;

interface

uses
  System.Classes,
  ThTypes;

type
  TThothController = class(TInterfacedObject, IThSubject)
  private
    FObservers: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);

    procedure Undo;
    procedure Redo;
  end;

implementation

{ TMainController }

constructor TThothController.Create;
begin
  FObservers := TInterfaceList.Create;
end;

destructor TThothController.Destroy;
begin
  FObservers.Clear;
  FObservers := nil;

  inherited;
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

end;

procedure TThothController.Redo;
begin

end;

end.
