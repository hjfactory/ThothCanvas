program ThothCanvas;

uses
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {frmMainDraft},
  ThItem in 'Cores\ThItem.pas',
  ThShapeItem in 'Cores\ThShapeItem.pas',
  ThCanvasEditor in 'Cores\ThCanvasEditor.pas',
  ThCanvas in 'Cores\ThCanvas.pas',
  ThItemFactory in 'Components\ThItemFactory.pas',
  DebugUtils in 'Utils\DebugUtils.pas',
  ThItemHighlighter in 'Components\ThItemHighlighter.pas',
  ThConsts in 'Types\ThConsts.pas',
  ThItemSelection in 'Components\ThItemSelection.pas',
  ThTypes in 'Types\ThTypes.pas',
  SpotCornerUtils in 'Utils\SpotCornerUtils.pas',
  ThClasses in 'Libraries\ThClasses.pas',
  ThCanvasController in 'Controllers\ThCanvasController.pas',
  ThothController in 'Controllers\ThothController.pas',
  ThCommandManager in 'Controllers\ThCommandManager.pas',
  ThItemCommand in 'Commands\ThItemCommand.pas',
  ThItemStorage in 'Controllers\ThItemStorage.pas',
  ThSystemCommand in 'Commands\ThSystemCommand.pas',
  ThZoomAnimation in 'Components\ThZoomAnimation.pas',
  ThResourceString in 'Types\ThResourceString.pas',
  ThAlertAnimation in 'Components\ThAlertAnimation.pas',
  ThImageItem in 'Cores\ThImageItem.pas',
  ThCanvasCommand in 'Commands\ThCanvasCommand.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainDraft, frmMainDraft);
  Application.Run;
end.
