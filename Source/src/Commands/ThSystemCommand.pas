unit ThSystemCommand;

interface

uses
  FMX.Types, System.Types,
  ThTypes, ThClasses, ThItem;

type
  TThAbstractCommandSystem = class(TInterfacedObject, IThCommand)
  public
    procedure Execute; virtual; abstract;
    procedure Rollback; virtual; abstract;
  end;

  TThCommandSystemItemDestroy = class(TThAbstractCommandSystem)
  private
    FItems: TThItems;
  public
    constructor Create(AItems: TThItems);
    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;

    property Items: TThItems read FItems;
  end;

implementation

uses
  CommonUtils;

{ TThCommandSystemItemDestroy }

constructor TThCommandSystemItemDestroy.Create(AItems: TThItems);
begin
  FItems := TThItems.Create(AItems);
end;

destructor TThCommandSystemItemDestroy.Destroy;
begin
  FItems.Free;

  inherited;
end;

procedure TThCommandSystemItemDestroy.Execute;
begin

end;

procedure TThCommandSystemItemDestroy.Rollback;
begin

end;

end.
