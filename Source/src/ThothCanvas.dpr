program ThothCanvas;

uses
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {Form1},
  ThItem in 'Libraries\ThItem.pas',
  ThShape in 'Components\ThShape.pas',
  ThCanvasEditor in 'Components\ThCanvasEditor.pas',
  ThCanvas in 'Libraries\ThCanvas.pas',
  ThItemFactory in 'Components\ThItemFactory.pas',
  DebugUtils in 'Utils\DebugUtils.pas',
  ThItemHighlighter in 'Components\ThItemHighlighter.pas',
  ThConsts in 'Libraries\ThConsts.pas',
  ThItemResizer in 'Components\ThItemResizer.pas',
  ThTypes in 'Libraries\ThTypes.pas',
  ResizeUtils in 'Utils\ResizeUtils.pas',
  ThClasses in 'Libraries\ThClasses.pas',
  ThCanvasController in 'Controllers\ThCanvasController.pas',
  ThothController in 'Controllers\ThothController.pas',
  ThCommandManager in 'Commands\ThCommandManager.pas',
  ThItemCommand in 'Commands\ThItemCommand.pas',
  ThItemStorage in 'Controllers\ThItemStorage.pas',
  ThSystemCommand in 'Commands\ThSystemCommand.pas',
  ThZoomAnimation in 'Components\ThZoomAnimation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
