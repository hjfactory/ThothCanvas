program InfiniteCanvas;

uses
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Unit5 in 'Unit5.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
