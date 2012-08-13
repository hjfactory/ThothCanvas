program ThothShapeSample;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Debug in 'Debug.pas',
  ThShape in '..\..\..\src\lib\ThShape.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
