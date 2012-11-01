unit ThItemStorage;

interface

uses
  ThTypes, ThClasses, ThItem;

type
  TThItemStorage = class(TThInterfacedObject, IThObserver)
  private
    FSubject: IThSubject;
    FItems: TThItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

implementation

{ TThItemStorage }

constructor TThItemStorage.Create;
begin
  FItems := TThItems.Create;
end;

destructor TThItemStorage.Destroy;
begin
  FItems.Free;

  inherited;
end;

procedure TThItemStorage.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;
  FSubject.RegistObserver(Self);
end;

procedure TThItemStorage.Notifycation(ACommand: IThCommand);
begin

end;

end.
