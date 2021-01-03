unit ThCanvasCommand;

interface

uses
  FMX.Types, System.Types, System.SysUtils,
  ThTypes, ThClasses, ThItem;

type
  TThCanvasCommand = class(TInterfacedObject, IThCommand)
  private
    procedure Execute; virtual;
    procedure Rollback; virtual;
  protected
    FItems: TThItems;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TThCommandCanvasZoom = class(TThCanvasCommand)
  end;

implementation

{ TThCanvasCommand }

constructor TThCanvasCommand.Create;
begin

end;

destructor TThCanvasCommand.Destroy;
begin

  inherited;
end;

procedure TThCanvasCommand.Execute;
begin
end;

procedure TThCanvasCommand.Rollback;
begin
end;

end.
