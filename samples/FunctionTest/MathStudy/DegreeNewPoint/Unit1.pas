unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.Objects, FMX.Edit, System.UIConsts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Button1: TButton;
    edtH: TEdit;
    edtW: TEdit;
    Label2: TLabel;
    Edit5: TEdit;
    Label3: TLabel;
    Button2: TButton;
    ScrollBox1: TScrollBox;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FX, FY, FW, FH: Single;
    FCP: TPointF;
    FRot: Single;
    procedure DrawPoint(const AP: TPointF; AC: TAlphaColor);
    procedure DrawLine(const ASP, AEP: TPointF; AC: TAlphaColor);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FX := StrToFloatDef(Edit1.Text, 0.0);
  FY := StrToFloatDef(Edit2.Text, 0.0);
  FW := StrToFloatDef(edtW.Text, 0.0);
  FH := StrToFloatDef(edtH.Text, 0.0);
  FCP := PointF(FX + FW, FY + FH);

  DrawPoint(PointF(FX, FY), claRed);
  DrawPoint(FCP, claBlue);
  DrawLine(PointF(FX, FY), FCP, claBlack);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CP: TPointF;
  Deg, Rad: Single;
  X, Y, D, R1: Single;
begin
  FRot := StrToFloatDef(Edit5.Text, 0.0);

  Rad := ArcTan(FH/FW);
  Deg := RadToDeg(Rad);

  // º±±Ê¿Ã
  D := FW / Cos(Rad);

  Memo1.Lines.Add(Format('%f, %f, %f', [Rad, Deg, D]));

  R1 := DegToRad(90) - Rad + DegToRad(FRot);

  X := FCP.X - Sin(R1) * D;
  Y := FCP.Y - Cos(R1) * D;
  DrawPoint(PointF(X, Y), claGreen);
  DrawLine(FCP, PointF(X, Y), claBlack);
  Memo1.Lines.Add(Format('%f, %f, %f, %f', [R1, RadToDeg(R1), X, Y]));
end;

procedure TForm1.DrawLine(const ASP, AEP: TPointF; AC: TAlphaColor);
begin
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := AC;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawLine(ASP, AEP, 1);
  Image1.Bitmap.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.DrawPoint(const AP: TPointF; AC: TAlphaColor);
begin
  Image1.Bitmap.Canvas.BeginScene;
//  Image1.Bitmap.Canvas.Fill.Color := claWhite;
//  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := AC;
  Image1.Bitmap.Canvas.StrokeThickness := 5;
  Image1.Bitmap.Canvas.DrawEllipse(RectF(AP.X-1,AP.Y-1,AP.X+1,AP.Y+1), 1);
  Image1.Bitmap.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Bitmap.Create(Round(Image1.Width), Round(Image1.Height));
//  Image1.Bitmap.FillColor(claWhite);
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Fill.Color := claWhite;
  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;
end;

end.
