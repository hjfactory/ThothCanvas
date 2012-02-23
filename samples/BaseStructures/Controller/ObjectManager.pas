unit ObjectManager;

//ThothManager

interface

uses
  System.Classes, System.SysUtils,
  ThothTypes, ThothObjects, ObjectList, CommandList;

type
///////////////////////////////////////////////////////
// Manager
  TThothObjectManager = class(TThInterfacedObject, IThSubject)
  private
    FObservers: TInterfaceList;

    FObjectList: TThObjectList;
    FCommandList: TThCommandList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);

    procedure Redo;
    procedure Undo;

    property CommandList: TThCommandList read FCommandlist;

    procedure test;
  end;

implementation

uses
  WinAPI.windows, ThothCommands;

{ TThothObjectManager }

constructor TThothObjectManager.Create;
begin
  FObservers := TInterfaceList.Create;

  FObjectList := TThObjectList.Create;
  FObjectList.SetSubject(Self);

  FCommandList := TThCommandList.Create;
  FCommandList.SetSubject(Self);
//  RegistObserver(FCommandList);
//  FObjectList.SetSubject(Self);
end;

destructor TThothObjectManager.Destroy;
begin
  OutputDebugString(PChar('TThothObjectManager.Destroy;'));

//  FObjectList := nil;
  FObservers.Clear;
  FObjectList.Free;
//  FObservers.Free;

  inherited;
end;

procedure TThothObjectManager.RegistObserver(AObserver: IThObserver);
begin
  FObservers.Add(AObserver);
end;

procedure TThothObjectManager.Subject(ASource: IThObserver; ACommand: IThCommand);
var
  I: Integer;
  Observer: IThObserver;
begin
  if ACommand is TThMoveShapeCommand then
    OutputDebugString(PChar(Format('TThothObjectManager.Subject.TThMoveShapeCommand(%f,%f) (%f, %f)', [
        TThMoveShapeCommand(ACommand).BeforePos.X
      , TThMoveShapeCommand(ACommand).BeforePos.Y
      , TThMoveShapeCommand(ACommand).AfterPos.X
      , TThMoveShapeCommand(ACommand).AfterPos.Y
    ])));

  for I := 0 to FObservers.Count - 1 do
  begin
    if FObservers[I] = ASource then
      Continue;

     IThObserver(FObservers[I]).Notifycation(ACommand);
  end;
end;

procedure TThothObjectManager.test;
begin
  FObjectList.test;
end;

procedure TThothObjectManager.Undo;
begin
  FCommandList.Undo;
end;

procedure TThothObjectManager.Redo;
begin
  FCommandList.Redo;
end;

procedure TThothObjectManager.UnregistObserver(AObserver: IThObserver);
begin
  FObservers.Remove(AObserver);
end;

end.
