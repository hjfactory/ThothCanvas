program ThothCanvasVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'Canvas\ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas',
  ThCanvasLayers in 'Canvas\ThCanvasLayers.pas',
  clipper in '..\Libraries\clipper.pas',
  ThUtils in 'Utils\ThUtils.pas',
  ThDrawItem in 'Items\ThDrawItem.pas',
  ThDrawObject in 'Items\ThDrawObject.pas',
  ThDrawStyle in 'Items\ThDrawStyle.pas',
  ThClasses in 'Types\ThClasses.pas',
  ThShapeItem in 'Items\ThShapeItem.pas',
  ThSelection in 'Items\ThSelection.pas',
  ThLinkPoint in 'Items\ThLinkPoint.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
