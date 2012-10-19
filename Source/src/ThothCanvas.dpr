program ThothCanvas;

uses
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {Form1},
  ThItem in 'Libraries\ThItem.pas',
  ThShape in 'Components\ThShape.pas',
  ThCanvasEditor in 'Components\ThCanvasEditor.pas',
  ThContainer in 'Libraries\ThContainer.pas',
  ThItemFactory in 'Components\ThItemFactory.pas',
  CommonUtils in 'Utils\CommonUtils.pas',
  ThItemHighlighterIF in 'Interfaces\ThItemHighlighterIF.pas',
  ThItemHighlighter in 'Components\ThItemHighlighter.pas',
  ThConsts in 'Libraries\ThConsts.pas',
  ThItemResizerIF in 'Interfaces\ThItemResizerIF.pas',
  ThItemResizer in 'Components\ThItemResizer.pas',
  ThTypes in 'Libraries\ThTypes.pas',
  ResizeUtils in 'Utils\ResizeUtils.pas',
  ThItemIF in 'Interfaces\ThItemIF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
