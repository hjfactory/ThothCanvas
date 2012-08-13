unit ThCanvas;

interface

uses
  ThTypes, ThLayout;

type
  TThCanvas = class(TThContainer, IThCanvas, IThObserver)
  private
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;


implementation

{ TThCanvas }

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin

end;

procedure TThCanvas.SetSubject(ASubject: IThSubject);
begin

end;

end.
