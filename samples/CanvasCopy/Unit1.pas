unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Rectangle1: TRectangle;
    Button1: TButton;
    Panel1: TPanel;
    Image1: TImage;
    FramedScrollBox1: TFramedScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure ScrollBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin


  with ScrollBox1.Canvas do
  begin

  end;
end;

procedure TForm1.ScrollBox2Click(Sender: TObject);
begin
//  with TRectangle.Create(ScrollBox2) do
//  begin
//    Position.X := 30;
//    Position.Y := 200;
//    Width := 200;
//    Height := 100;
//    Parent := ScrollBox2;
//  end;
end;

end.
