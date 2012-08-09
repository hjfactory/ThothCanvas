program ThothShapeSample;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ThothShape in 'ThothShape.pas',
  Debug in 'Debug.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
