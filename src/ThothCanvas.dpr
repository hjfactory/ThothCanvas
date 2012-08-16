program ThothCanvas;

uses
  FMX.Forms,
  ThLayout in 'lib\ThLayout.pas',
  ThShape in 'component\ThShape.pas',
  MainForm in 'form\MainForm.pas' {Form1},
  ThCanvas in 'component\ThCanvas.pas',
  ThTypes in 'common\ThTypes.pas',
  ThCommand in 'model\ThCommand.pas',
  ThInterface in 'model\ThInterface.pas',
  ThCommandManager in 'controller\ThCommandManager.pas',
  ThMainController in 'controller\ThMainController.pas',
  ThObjectManager in 'controller\ThObjectManager.pas',
  ThItem in 'lib\ThItem.pas',
  ThControlPanel in 'component\ThControlPanel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
