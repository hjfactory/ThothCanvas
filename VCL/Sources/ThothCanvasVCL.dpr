program ThothCanvasVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'Cores\ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas',
  ThCanvasLayers in 'Cores\ThCanvasLayers.pas',
  clipper in '..\Libraries\clipper.pas',
  ThGrahicsUtils in 'Utils\ThGrahicsUtils.pas',
  DebugForm in 'Forms\DebugForm.pas' {frmDebug},
  ThItem in 'Cores\ThItem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.
