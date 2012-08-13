program ThCanvasSample;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Unit1 in 'Unit1.pas',
  ThLayout in '..\..\..\src\lib\ThLayout.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
