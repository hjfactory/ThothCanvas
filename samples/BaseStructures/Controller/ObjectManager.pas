unit ObjectManager;

interface

uses
  System.Classes, System.SysUtils,
  ThothTypes, ThothObjects, ObjectList;

type
///////////////////////////////////////////////////////
// Manager
  TThothObjectManager = class(TInterfacedObject, IThSubject)
  private
    FDatetime: TDatetime;
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
  FDatetime := now;

  FObservers := TInterfaceList.Create;
  FObjectList := TThObjectList.Create;
  FObjectList.SetSubject(Self);
end;

destructor TThothObjectManager.Destroy;
begin
  OutputDebugString(PChar('TThothObjectManager.Destroy;'));

  FObjectList.Free;
  FObservers.Free;

  inherited;
end;

procedure TThothObjectManager.RegistObserver(AObserver: IThObserver);
begin
  OutputDebugString(PChar(FormatDateTime('HH:NN:SS.ZZZ', FDatetime)));

  FObservers.Add(AObserver);
end;

procedure TThothObjectManager.Report(ACommand: IThCommand);
var
  I: Integer;
  Observer: IThObserver;
begin
  OutputDebugString(PChar(FormatDateTime('HH:NN:SS.ZZZ', FDatetime)));

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
