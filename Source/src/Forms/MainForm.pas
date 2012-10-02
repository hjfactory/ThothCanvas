unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ThCanvasEditor, FMX.Layouts,
  FMX.Memo, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThCanvasEditor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(Format('%f, %f', [FCanvas.ContentPos.X, FCanvas.ContentPos.Y]));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCanvas.ItemID := 1100;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Shape: TShape;
begin
  FCanvas := TThCanvasEditor.Create(Self);
  FCanvas.Parent := Panel1;
  FCanvas.Align := TAlignLayout.alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

end.
