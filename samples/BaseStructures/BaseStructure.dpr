program BaseStructure;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ThothObjects in 'Model\ThothObjects.pas',
  ThothCanvas in 'Canvas\ThothCanvas.pas',
  ThothTypes in 'Model\ThothTypes.pas',
  ThothCommands in 'Model\ThothCommands.pas',
  ObjectList in 'Model\ObjectList.pas',
  ObjectManager in 'Controller\ObjectManager.pas',
  CommandList in 'Model\CommandList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
