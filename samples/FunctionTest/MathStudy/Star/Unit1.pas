unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Edit,
  FMX.Layouts, FMX.Memo;

type
  TStarRec = record
    P: array[0..4] of TPointF;
    D: array[0..4] of TPointF;
  end;

  TForm1 = class(TForm)
    Image1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    FPoints: TPolygon;

    procedure DrawPoint(const AP: TPointF; AC: TAlphaColor);
    procedure DrawLine(const AP, AP2: TPointF; AC: TAlphaColor = claBlack);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.fmx}

procedure TForm1.DrawLine(const AP, AP2: TPointF; AC: TAlphaColor);
begin
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := AC;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawLine(AP, Ap2, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.DrawPoint(const AP: TPointF; AC: TAlphaColor);
begin
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := AC;
  Image1.Bitmap.Canvas.StrokeThickness := 5;
  Image1.Bitmap.Canvas.DrawEllipse(RectF(AP.X-1,AP.Y-1,AP.X+1,AP.Y+1), 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
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
  Image1.Bitmap.BitmapChanged;

  SetLength(FPoints, 11);
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
const
  D_P = 360 / 5;    // Degree Point
  D_D = 360 / 10;   // Degree Deep
var
  R, D: Single;
  Deg: Single;
  I: Integer;
begin
  DrawPoint(PointF(X, Y), claRed);

  R := StrToIntDef(Edit1.Text, 100);
  D := StrToIntDef(Edit2.Text, 40);

  for I := 0 to 4 do
  begin
    // 꼭지점 튀어나온 부분(0, 72, 144, 216, 288, 360)
    Deg := I * D_P;
    FPoints[I*2].X := X + (sin(DegToRad(Deg)) * R);
    FPoints[I*2].Y := Y + R - (cos(DegToRad(Deg)) * R);

    // 꼭지점 들어간 부분(36, 108, 180, 252, 324, 396)
    Deg := Deg + D_D;
    FPoints[I*2 + 1].X := X + (sin(DegToRad(Deg)) * (R-D));
    FPoints[I*2 + 1].Y := Y + R - (cos(DegToRad(Deg)) * (R-D));
  end;
  FPoints[10] := PointF(X, Y);

  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawPolygon(FPoints, 1);
  Image1.Bitmap.Canvas.EndScene;
  Image1.Bitmap.BitmapChanged;
end;

end.
