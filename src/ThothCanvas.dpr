program ThothCanvas;

uses
  FMX.Forms,
  ThLayout in 'lib\ThLayout.pas',
  ThShape in 'lib\ThShape.pas',
  MainForm in 'form\MainForm.pas' {Form1},
  ThCanvas in 'lib\ThCanvas.pas',
  ThTypes in 'common\ThTypes.pas',
  CommandFactory in 'model\CommandFactory.pas',
  ThCommand in 'common\ThCommand.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
