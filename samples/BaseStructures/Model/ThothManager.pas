unit ThothManager;

interface

uses
  ThothTypes;

type

///////////////////////////////////////////////////////
// ObjectList
  TThObjectList = class(TInterfacedObject, IThObserver)
  private
    procedure Notifycation(ACommand: IThCommand);
  end;

///////////////////////////////////////////////////////
// Command List
  TCommandList = class

  end;

///////////////////////////////////////////////////////
// Manager
  TThothManager = class(TInterfacedObject, IThSubject)
    procedure Report(ACommand: IThCommand);
    procedure AddObserver(AObserver: IThObserver);
  end;


implementation

{ TThObjectList }

procedure TThObjectList.Notifycation(ACommand: IThCommand);
begin

end;

{ TThothManager }

procedure TThothManager.AddObserver(AObserver: IThObserver);
begin

end;

procedure TThothManager.Report(ACommand: IThCommand);
begin
//  OutputDebugString(PChar('test'));
end;

end.
