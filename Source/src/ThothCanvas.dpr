program ThothCanvas;

uses
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {Form1},
  ThItem in 'Libraries\ThItem.pas',
  ThShape in 'Cores\ThShape.pas',
  ThCanvasEditor in 'Cores\ThCanvasEditor.pas',
  ThCanvas in 'Libraries\ThCanvas.pas',
  ThItemFactory in 'Components\ThItemFactory.pas',
  DebugUtils in 'Utils\DebugUtils.pas',
  ThItemHighlighter in 'Components\ThItemHighlighter.pas',
  ThConsts in 'Types\ThConsts.pas',
  ThItemResizer in 'Components\ThItemResizer.pas',
  ThTypes in 'Types\ThTypes.pas',
  SpotCornerUtils in 'Utils\SpotCornerUtils.pas',
  ThClasses in 'Libraries\ThClasses.pas',
  ThCanvasController in 'Controllers\ThCanvasController.pas',
  ThothController in 'Controllers\ThothController.pas',
  ThCommandManager in 'Commands\ThCommandManager.pas',
  ThItemCommand in 'Commands\ThItemCommand.pas',
  ThItemStorage in 'Controllers\ThItemStorage.pas',
  ThSystemCommand in 'Commands\ThSystemCommand.pas',
  ThZoomAnimation in 'Components\ThZoomAnimation.pas',
  ThResourceString in 'Types\ThResourceString.pas',
  ThAlertAnimation in 'Components\ThAlertAnimation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
