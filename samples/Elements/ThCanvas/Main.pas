unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,
  ThothCanvas, FMX.Layouts, FMX.Dialogs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ScrollBox1: TScrollBox;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;

    procedure ClickEvent(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TButton.Create(FThCanvas) do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(10, 10);
    Text := 'asdfsaf';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
//  ShowMessage(Format('%s', [FThCanvas.test]));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with TButton.Create(FThCanvas) do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(250, 150);
    Text := 'asdfsaf';
    Onclick := ClickEvent;
  end;
  FThCanvas.Realign;

  with TButton.Create(ScrollBox1) do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(150, 150);
    Text := 'asdfsaf';
    Onclick := ClickEvent;
  end;


  ScrollBox1.Realign
end;

procedure TForm1.ClickEvent(Sender: TObject);
begin
  ShowMessage(TButton(Sender).Parent.ClassName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Application.DefaultStyles

  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Align := TAlignLayout.alClient;
  FThCanvas.Parent := Panel1;
  FThCanvas.Position.Point := PointF(100, 50);
  FThCanvas.Width := 400;
  FThCanvas.Height := 400;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;
end;

end.
