unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  ThothShape;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FRectangle: TThRectangle;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FRectangle := TThRectangle.Create(Self);
  with FRectangle do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(10, 10);
    Width := 100;
    Height := 100;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FRectangle.Selected := not FRectangle.Selected;
end;

end.
