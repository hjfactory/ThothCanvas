unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,
  ThLayout, FMX.Layouts, FMX.Dialogs, FMX.Objects;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ScrollBox1: TScrollBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Rectangle1: TRectangle;
    Button6: TButton;
    Button7: TButton;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Rectangle1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FA, FB: TButton;
    FThCanvas: TThContainer;

    procedure ClickEvent(Sender: TObject);
    procedure ResizeEvent(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit1;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin

  FA := TButton.Create(FThCanvas);
  with FA do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(-30, -10);
    Text := 'asdfsaf';
    Width := 100;
    Height := 100;
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FA.Position.Point := PointF(-10, 0);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FB := TButton.Create(FThCanvas);
  with FB do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(250, 150);
    Text := 'asdfsaf';
    Scale.X := 2;
    Onclick := ClickEvent;
    OnResize := ResizeEvent;
  end;

  with TButton.Create(ScrollBox1) do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(150, 150);
    Text := 'asdfsaf';
    Scale.X := 2;
    Onclick := ClickEvent;
  end;

  ScrollBox1.Realign
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FB.Position.X := 300;
  Debug('Move');
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  panel1.Width := 600;
  panel1.Height := 600;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FThCanvas.Center;

  ShowMessage(Format('%f', [Panel2.Position.X]));
end;

procedure TForm1.ClickEvent(Sender: TObject);
begin
//  ShowMessage(TButton(Sender).Parent.ClassName);
  TBUtton(Sender).Scale.X := 3;
  TBUtton(Sender).Scale.Y := 3;
  TButton(Sender).RotationAngle := 45;

  Debug('Change scale');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Application.DefaultStyles

  FThCanvas := TThContainer.Create(Self);
  FThCanvas.Align := TAlignLayout.alClient;
  FThCanvas.Parent := Panel1;
  FThCanvas.Position.Point := PointF(100, 50);
  FThCanvas.Width := 400;
  FThCanvas.Height := 400;

//  FTHCanvas.ClipChildren := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;
end;

procedure TForm1.Rectangle1Click(Sender: TObject);
begin
  TShape(Sender).Position.X := TShape(Sender).Position.X + 10;
end;

procedure TForm1.ResizeEvent(Sender: TObject);
begin
  Debug('');
end;

end.
