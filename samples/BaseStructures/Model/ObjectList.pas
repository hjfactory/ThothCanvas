unit ObjectList;

interface

uses
  System.Classes, System.SysUtils,
  ThothTypes, ThothObjects, ThothCommands;

type
///////////////////////////////////////////////////////
// ObjectList
  TThObjectList = class(TThInterfacedObject, IThObserver)
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure test;
  end;


implementation

uses
  WinAPI.Windows;

{ TThObjectList }

constructor TThObjectList.Create;
begin
  FList := TList.Create;
end;

destructor TThObjectList.Destroy;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TObject(FList[I]).Free;

//  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TThObjectList.Notifycation(ACommand: IThCommand);
var
  I: Integer;
  Cmd: TThShapeCommand;
begin
  if ACommand is TThInsertShapeCommand then
    OutputDebugSTring(PChar('TThInsertShapeCommand'));

  if ACommand is TThShapeCommand then
  begin
    Cmd := TThShapeCommand(ACommand);
    for I := 0 to Cmd.ShapeCount - 1 do
      FList.Add(Cmd[I]);
//    FList.Add(
  end;
end;

procedure TThObjectList.SetSubject(ASubject: IThSubject);
begin
  ASubject.RegistObserver(Self);
end;

procedure TThObjectList.test;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    OutputDebugString(PChar(Format('%d> %s', [I, TThShape(FList[I]).ClassName])));
end;

end.
