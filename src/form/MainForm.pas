unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  ThCanvas;

type
  TForm1 = class(TForm)
    pnlMainMenu: TPanel;
    pnlMenu: TPanel;
    pnlMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Parent := pnlMain;
  FThCanvas.Align := TAlignLayout.alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;
end;

end.
