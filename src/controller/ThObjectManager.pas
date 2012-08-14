unit ThObjectManager;

interface

uses
  System.Classes, System.SysUtils,
  ThTypes, ThInterface, ThCommand, ThShape;

type
///////////////////////////////////////////////////////
// ObjectList
  TThObjectManager = class(TThInterfacedObject, IThObserver)
  private
    FList: TList;
    FBackup: TList;

    procedure InsertShapes(AShapes: TList);
    procedure DeleteShapes(AShapes: TList);
    procedure RestoreShapes(AShapes: TList);
    procedure RemoveShapes(AShapes: TList);
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

constructor TThObjectManager.Create;
begin
  FList := TList.Create;
  FBackup := TList.Create;
end;

destructor TThObjectManager.Destroy;
var
  I: Integer;
begin
// Canvas에서 처리?
//  for I := FList.Count - 1 downto 0 do
//    TObject(FList[I]).Free;
  FList.Free;

  for I := FBackup.Count - 1 downto 0 do
    TObject(FBackup[I]).Free;
  FBackup.Free;

  inherited;
end;

procedure TThObjectManager.InsertShapes(AShapes: TList);
begin
  FList.Assign(AShapes, TListAssignOp.laOr);
end;

procedure TThObjectManager.DeleteShapes(AShapes: TList);
var
  I: Integer;
  Shape: TThShape;
begin
  for I := 0 to AShapes.Count - 1 do
  begin
    Shape := TThShape(AShapes[I]);

//    Shape.Depth := FList.IndexOf(Shape);
//    if Shape.Depth > -1 then
//      FList.Delete(Shape.Depth);

    FBackup.Add(Shape);
  end;
end;

procedure TThObjectManager.RestoreShapes(AShapes: TList);
var
  I, Idx: Integer;
  Shape: TThShape;
begin
  for I := 0 to AShapes.Count - 1 do
  begin
    Shape := TThShape(AShapes[I]);
    FBackup.RemoveItem(Shape, TList.TDirection.FromEnd);
//    FList.Insert(Shape.Depth, Shape);
  end;
end;

procedure TThObjectManager.RemoveShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := FBackup.Count - 1 downto 0 do
    TObject(FBackup[I]).Free;
end;

procedure TThObjectManager.Notifycation(ACommand: IThCommand);
var
  I: Integer;
  Cmd: TThShapeCommand;
begin
  OutputDebugSTring(PChar('TThObjectList - ' + TThShapeCommand(ACommand).ClassName));

  if ACommand is TThInsertShapeCommand then
    InsertShapes(TThShapeCommand(ACommand).List)
    // FList에 추가
  else if ACommand is TThDeleteShapeCommand then
    DeleteShapes(TThShapeCommand(ACommand).List)
    // FList에서 FBackup으로 이동, Index추가
  else if ACommand is TThRestoreShapeCommand then
    RestoreShapes(TThShapeCommand(ACommand).List)
    // FBackup에서 FList로 이동(Insert)
  else if ACommand is TThRemoveShapeCommand then
    RemoveShapes(TThShapeCommand(ACommand).List)
    // FBackup에서 제거 후 객체해제
  ;
end;

procedure TThObjectManager.SetSubject(ASubject: IThSubject);
begin
  ASubject.RegistObserver(Self);
end;

procedure TThObjectManager.test;
var
  I: Integer;
begin
//  for I := 0 to FList.Count - 1 do
//    with TThShape(FList[I]) do
//      Debug('%d> %s(W:%f, H:%f, %f,%f / %f,%f)', [I, ClassName, Width, Height, StartPos.X, StartPos.Y, EndPos.X, EndPos.Y]);
end;

end.
