unit ThothCanvas;

interface

uses
  System.Classes, FMX.Types;

type
  TThContent = class(TControl)

  end;

  TThCanvas = class(TStyledControl)
  private
    FContent: TThContent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;
  end;

implementation

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FContent := TThContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
end;

destructor TThCanvas.Destroy;
begin
  FContent.Free;
  FContent := nil;

  inherited;
end;

procedure TThCanvas.AddObject(AObject: TFmxObject);
begin
  if Assigned(FContent) and (AObject <> FContent) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.AddObject(AObject)
  end
  else
    inherited;
end;

end.
