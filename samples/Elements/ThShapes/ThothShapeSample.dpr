program ThothShapeSample;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Debug in 'Debug.pas',
  ThShape in '..\..\..\src\component\ThShape.pas',
  ThTypes in '..\..\..\src\common\ThTypes.pas',
  ThInterface in '..\..\..\src\model\ThInterface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
