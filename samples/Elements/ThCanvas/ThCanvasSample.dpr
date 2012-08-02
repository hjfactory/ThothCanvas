program ThCanvasSample;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ThothCanvas in 'ThothCanvas.pas',
  Unit1 in 'Unit1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
