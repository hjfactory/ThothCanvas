unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,
  ThothCanvas, FMX.Layouts;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TButton.Create(ScrollBox1) do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(200, 10);
    Text := 'afasfdasfd';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Parent := Self;
  FThCanvas.Position.Point := PointF(100, 50);
  FThCanvas.Width := 400;
  FThCanvas.Height := 400;

  with TButton.Create(Self) do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(10, 10);
    Text := 'asdfsaf';
  end;
end;

end.
