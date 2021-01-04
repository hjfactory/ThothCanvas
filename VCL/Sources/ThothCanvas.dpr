program ThothCanvas;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ThCanvas in 'Cores\ThCanvas.pas',
  ThTypes in 'Types\ThTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
