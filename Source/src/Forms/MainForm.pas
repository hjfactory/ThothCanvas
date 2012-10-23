{

}

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ThCanvasEditor, FMX.Layouts,
  FMX.Memo, FMX.Objects, FMX.ExtCtrls, FMX.Edit;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    CornerButton1: TCornerButton;
    CornerButton2: TCornerButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
  ThItem, WinAPI.Windows;

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCanvas.DrawItemID := TButton(Sender).Tag;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  I: Integer;
  Item: TThItem;
begin
  for I := 0 to 200 do
  begin
    Item := FCanvas.AddItem(1100);
    Item.Position.X := Random(Width);
    Item.Position.Y := Random(Height);
    Item.Height := 50 + Random(50);
    Item.Width := 50 + Random(50);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FCanvas.DeleteSelection;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvasEditor.Create(Self);
  FCanvas.Parent := Panel1;
  FCanvas.Align := TAlignLayout.alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  keybd_event(VK_SHIFT, 0, 0, 0);
end;

end.
