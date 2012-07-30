unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,
  ThothCanvas, FMX.Layouts;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TButton.Create(FThCanvas) do
  begin
    Parent := FThCanvas;
    Position.Point := PointF(10, 10);
    Text := 'asdfsaf';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
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
