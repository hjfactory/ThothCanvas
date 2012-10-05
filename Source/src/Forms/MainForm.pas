unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ThCanvasEditor, FMX.Layouts,
  FMX.Memo, FMX.Objects;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThCanvasEditor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ThItem;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FCanvas.Repaint;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCanvas.ItemID := TButton(Sender).Tag;
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
