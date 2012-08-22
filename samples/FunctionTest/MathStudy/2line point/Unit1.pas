unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.Memo, FMX.Objects;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    Image1: TImage;
    Edit2: TEdit;
    Button3: TButton;
    Edit3: TEdit;
    Button4: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }

    FIndex: Integer;
    FPos1, FPos2, FPD1, FPD2: TPointF;
    procedure Log(str: string);
    procedure DrawPoint(const AP: TPointF; AC: TAlphaColor);
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
var
  rad: Double;
begin
  // Degrees  : 각도
  // Radians  : 라디안

  // 0.1 deg = 0.001745 rad     rad = deg / PI / 180
  // 0.1 rad = 5.7 deg          deg = rad * PI * 180

  // rad = deg / PI

  // PI rad = 180 deg

  Log(Format('%f, %f', [Cos(DegToRad(60)), Cos(PI / 3)]));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  rad: single;
begin
  Log(Format('%f', [DegToRad(90)]));
  rad := Tan(PI/3);
  Log(Format('%f', [rad]));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  X, Y, W, H, D, D2, Rad: Single;

begin
  W := Abs(FPos1.X - FPos2.X);
  H := Abs(FPos1.Y - FPos2.Y);
  Log(Format('W: %f, H: %f', [W, H]));
  Log(format('distance: %f', [sqrt(Power(W, 2) + Power(H, 2))]));

  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawLine(FPos1, FPos2, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;

  Rad := ArcTan(W/H);
  Log(Format('Radians: %f, Degree: %f', [Rad, RadToDeg(Rad)]));

  D := StrToFloatDef(Edit3.Text, 200);
  D2 := D / 2;

  Y := Sin(Rad) * D2;
  X := Sin(DegToRad(90) - Rad) * D2;

  FPD1.X := IfThen(FPos1.X > FPos2.X, FPos2.X - X, FPos2.X + X);
  FPD1.Y := IfThen(FPos1.Y > FPos2.Y, FPos2.Y + Y, FPos2.Y - Y);
  DrawPoint(FPD1, claGreen);
  Log(Format('D1: %f, %f', [FPD1.X, FPD1.Y]));

  FPD2.X := IfThen(FPos1.X > FPos2.X, FPos2.X + X, FPos2.X - X);
  FPD2.Y := IfThen(FPos1.Y > FPos2.Y, FPos2.Y - Y, FPos2.Y + Y);
  DrawPoint(FPD2, claGreen);
  Log(Format('D2: %f, %f', [FPD2.X, FPD2.Y]));

  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawLine(FPD1, FPD2, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
//  Log(Format('%f, %f', [X, Y]));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FIndex := 0;

  Image1.Bitmap.Create(Round(Image1.Width), Round(Image1.Height));
//  Image1.Bitmap.FillColor(claWhite);
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Fill.Color := claWhite;
  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
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
  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIndex := 0;

  Image1.Bitmap.Create(Round(Image1.Width), Round(Image1.Height));
//  Image1.Bitmap.FillColor(claWhite);
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Fill.Color := claWhite;
  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  C: TAlphaColor;
begin
  case FIndex of
  0:
    begin
      FPos1 := PointF(X, Y);
      Edit1.Text := Format('%f, %f', [X, Y]);
      C := claBlue;
    end;
  1:
    begin
      FPos2 := PointF(X, Y);
      Edit2.Text := Format('%f, %f', [X, Y]);
      C := claRed;
    end;
  else
    Exit;
  end;

  DrawPoint(PointF(X, Y), C);

  Inc(FIndex);
end;

procedure TForm1.Log(str: string);
begin
  Memo1.Lines.Add(Str)
end;

end.
