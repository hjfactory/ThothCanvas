unit ThMainController;

//ThothManager

interface

uses
  System.Classes, System.SysUtils,
  ThTypes, ThInterface, ThObjectManager, ThCommandManager;

type
///////////////////////////////////////////////////////
// Manager
  TThMainController = class(TThInterfacedObject, IThSubject)
  private
    FObservers: TInterfaceList;

    FObjectManager: TThObjectManager;
    FCommandManager: TThCommandManager;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);

    procedure Redo;
    procedure Undo;

//    property CommandList: TThCommandManager read FCommandManager;

    procedure test;
  end;

implementation

uses
  ThCommand;

{ TThMainController }

constructor TThMainController.Create;
begin
  FObservers := TInterfaceList.Create;

  FObjectManager := TThObjectManager.Create;
  FObjectManager.SetSubject(Self);

  FCommandManager := TThCommandManager.Create;
  FCommandManager.SetSubject(Self);
end;

destructor TThMainController.Destroy;
begin
  Debug('TThMainController.Destroy;');

//  FObjectList := nil;
  FObservers.Clear;
  FObjectManager.Free;
//  FObservers.Free;

  FCommandManager.Free;

  inherited;
end;

procedure TThMainController.RegistObserver(AObserver: IThObserver);
begin
  FObservers.Add(AObserver);
end;

procedure TThMainController.Subject(ASource: IThObserver; ACommand: IThCommand);
var
  I: Integer;
begin
  if ACommand is TThMoveShapeCommand then
    Debug('TThothObjectManager.Subject.TThMoveShapeCommand(%f,%f) (%f, %f)', [
        TThMoveShapeCommand(ACommand).FromPos.X
      , TThMoveShapeCommand(ACommand).FromPos.Y
      , TThMoveShapeCommand(ACommand).ToPos.X
      , TThMoveShapeCommand(ACommand).ToPos.Y
    ]);

  for I := 0 to FObservers.Count - 1 do
  begin
    if FObservers[I] = ASource then
      Continue;

     IThObserver(FObservers[I]).Notifycation(ACommand);
  end;
end;

procedure TThMainController.test;
begin
  FObjectManager.test;
end;

procedure TThMainController.Undo;
begin
  FCommandManager.Undo;
end;

procedure TThMainController.Redo;
begin
  FCommandManager.Redo;
end;

procedure TThMainController.UnregistObserver(AObserver: IThObserver);
begin
  FObservers.Remove(AObserver);
end;

end.
