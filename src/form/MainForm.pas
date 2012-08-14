unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  ThCanvas, FMX.Objects;

type
  TForm1 = class(TForm)
    pnlMainMenu: TPanel;
    pnlMenu: TPanel;
    pnlMain: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

procedure TForm1.Button3Click(Sender: TObject);
begin
  FThCanvas.ZoomOut;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FThCanvas.ZoomIn;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  with TThRectangle.Create(nil) do
  begin
    Parent := Rectangle1;
    Position.Point := PointF(10, -10);
    Width := 100;
    Height := 100;
  end;
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
