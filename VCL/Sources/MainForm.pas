unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ThCanvas, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    pnlMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvas.Create(nil);
  FCanvas.Align := alClient;
  FCanvas.Parent := pnlMain;

  FCanvas.CreatePage(640, 480);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

end.
