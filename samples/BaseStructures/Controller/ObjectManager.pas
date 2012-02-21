unit ObjectManager;

//ThothManager

interface

uses
  System.Classes, System.SysUtils,
  ThothTypes, ThothObjects, ObjectList;

type
///////////////////////////////////////////////////////
// Manager
  TThothObjectManager = class(TThInterfacedObject, IThSubject)
  private
    FObjectList: TThObjectList;
    FObservers: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Report(ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);

    procedure test;
  end;

implementation

uses
  WinAPI.windows;

{ TThothObjectManager }

constructor TThothObjectManager.Create;
begin
  FObservers := TInterfaceList.Create;
  FObjectList := TThObjectList.Create;
  RegistObserver(FObjectList);
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

procedure TThothObjectManager.Report(ACommand: IThCommand);
var
  I: Integer;
  Observer: IThObserver;
begin
  for I := 0 to FObservers.Count - 1 do
    IThObserver(FObservers[I]).Notifycation(ACommand);
end;

procedure TThothObjectManager.test;
begin
  FObjectList.test;
end;

procedure TThothObjectManager.UnregistObserver(AObserver: IThObserver);
begin
  FObservers.Remove(AObserver);
end;

end.
