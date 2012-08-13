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
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ThShape;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FThCanvas.ShapeClass := TThRectangle;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FThCanvas.ShapeClass := TThLine;
end;

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
