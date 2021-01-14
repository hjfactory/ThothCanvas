program ThothCanvasVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas',
  ThCanvasLayers in 'Canvas\ThCanvasLayers.pas',
  clipper in '..\Libraries\clipper.pas',
  ThGraphicsUtils in 'Utils\ThGraphicsUtils.pas',
  DebugForm in 'Forms\DebugForm.pas' {frmDebug},
  ThDrawItem in 'Items\ThDrawItem.pas',
  ThDrawObject in 'Items\ThDrawObject.pas',
  ThDrawStyle in 'Items\ThDrawStyle.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.
