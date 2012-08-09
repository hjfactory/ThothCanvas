unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  ThothShape, FMX.Edit, FMX.Memo;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Button2: TButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FRectangle: TThRectangle;
    FLine: TThLine;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Assigned(FRectangle) then
    Exit;

  FRectangle := TThRectangle.Create(Self);
  with FRectangle do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(10, 10);
    ShadowSize := 3;
    Width := 100;
    Height := 100;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not Assigned(FRectangle) then
    Exit;

  FRectangle.GripSize := StrToIntDef(Edit1.Text, 3);
  FRectangle.ShadowSize := StrToIntDef(Edit2.Text, 3);
  FRectangle.StrokeThickness := StrToIntDef(Edit3.Text, 1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(FLine) then
    Exit;
  FLine := TThLine.Create(Self);
  with FLine do
  begin
    Parent := ScrollBox1;
    Position.Point := PointF(10, 10);
    ShadowSize := 3;
    Width := 100;
    Height := 100;
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if not Assigned(FRectangle) then
    Exit;

  FRectangle.Opacity := TTrackBar(Sender).Value;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  if not Assigned(FRectangle) then
    Exit;

  FRectangle.Scale.X := TTrackBar(Sender).Value;
  FRectangle.Scale.Y := TTrackBar(Sender).Value;

//  Memo1.Lines.Add(Format('X: %f, Y: %f, L: %f', [FRectangle.Position.X, FRectangle.Position.Y, FRectangle.ShapeRect.Left]));
end;

end.
