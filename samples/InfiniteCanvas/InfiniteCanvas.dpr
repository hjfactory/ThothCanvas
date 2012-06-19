program InfiniteCanvas;

uses
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Unit5 in 'Unit5.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
