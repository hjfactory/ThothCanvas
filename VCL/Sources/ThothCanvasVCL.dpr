program ThothCanvasVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'Canvas\ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas',
  ThCanvasLayers in 'Canvas\ThCanvasLayers.pas',
  clipper in '..\Libraries\clipper.pas',
  ThUtils in 'Utils\ThUtils.pas',
  DebugForm in 'Forms\DebugForm.pas' {frmDebug},
  ThDrawItem in 'Items\ThDrawItem.pas',
  ThDrawObject in 'Items\ThDrawObject.pas',
  ThDrawStyle in 'Items\ThDrawStyle.pas',
  ThDrawObjectManager in 'Controller\ThDrawObjectManager.pas',
  ThClasses in 'Types\ThClasses.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.
