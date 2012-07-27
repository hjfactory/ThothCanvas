unit ThothCanvas;

interface

uses
  System.Classes, FMX.Types;

type
  TThCanvas = class(TStyledControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TThCanvas.Destroy;
begin

  inherited;
end;

end.
