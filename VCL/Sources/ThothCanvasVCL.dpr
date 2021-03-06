program ThothCanvasVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'Canvas\ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas',
  ThCanvasLayers in 'Canvas\ThCanvasLayers.pas',
  clipper in '..\Libraries\clipper.pas',
  ThUtils in 'Utils\ThUtils.pas',
  ThItem in 'Items\ThItem.pas',
  ThDrawObject in 'Canvas\ThDrawObject.pas',
  ThItemStyle in 'Items\ThItemStyle.pas',
  ThClasses in 'Types\ThClasses.pas',
  ThShapeItem in 'Items\ThShapeItem.pas',
  ThItemSelection in 'Items\ThItemSelection.pas',
  ThItemConnection in 'Items\ThItemConnection.pas',
  ThItemHandle in 'Items\ThItemHandle.pas',
  ThItemCollections in 'Items\ThItemCollections.pas',
  ThCanvasEventProcessor in 'Canvas\ThCanvasEventProcessor.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
