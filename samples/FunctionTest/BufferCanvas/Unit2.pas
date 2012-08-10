unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Memo;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TScrollBox;
    FBuffer: TScrollBox;

    R: TRectangle;

    procedure CP(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure BP(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.BP(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Memo1.Lines.Add('BP')
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  with TRectangle.Create(FCanvas) do
  begin
    Position.Point := PointF(100, 100);
    Width := 100;
    height := 100;
    Parent := FCanvas;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  R := TRectangle.Create(FCanvas);
  with R do
  begin
    Position.Point := PointF(150, 150);
    Width := 100;
    height := 100;
    Parent := FCanvas;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  R.Position.X := 300;
//  R.Position.Y := 300;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  R.Position.X := 150;
  R.Position.Y := 150;
end;

procedure TForm2.CP(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Memo1.Lines.Add('CP')
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FCanvas := TScrollBox.Create(Self);
//  FCanvas.Position.Point := TPointF(0, 0);
  FCanvas.Align := TAlignLayout.alClient;
  FCanvas.Parent := Self;
  FCanvas.OnPaint := CP;

  FBuffer := TSCrollBox.Create(FCanvas);
  FBuffer.Align := TAlignLayOut.alClient;
  FBuffer.Parent := FCanvas;
  FBuffer.OnPaint := BP;
end;

procedure TForm2.ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//  Memo1.Lines.Add('SP')
end;

end.
