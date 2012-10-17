unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.Memo, FMX.Objects, System.UIConsts;

type
  TForm1 = class(TForm)
    Button3: TButton;
    edtWidth: TEdit;
    Label1: TLabel;
    edtHeight: TEdit;
    Label2: TLabel;
    Image1: TImage;
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
begin
//  Image1.Bitmap.Canvas.BeginScene;
//  Image1.Bitmap.Canvas.StrokeThickness := 7;
//  Image1.Bitmap.Canvas.DrawLine(PointF(1, 10), PointF(Image1.Width, 11), 1);
//  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
//  Image1.Scale.Y := 10;
//  Image1.Scale.X := 10;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  X, Y, W, H: Single;
  SP, SP1, SP2, EP, EP1, EP2: TPointF;
  D, R: Single;
  SER, EER: TRectF;
begin
  W := StrToFloatDef(edtWidth.Text, 100);
  H := StrToFloatDef(edtHeight.Text, 80);

  D := 30;
  R := ArcTan(H/W);

  SP  := PointF(100, 100);
  SP1 := SP.Add(PointF(Sin(R) * D, - Cos(R) * D));
  SP2 := SP.Add(PointF(- Sin(R) * D, Cos(R) * D));
  EP  := SP.Add(PointF(W, H));
  EP1 := EP.Add(PointF(PointF(Sin(R) * D, - Cos(R) * D)));
  EP2 := EP.Add(PointF(- Sin(R) * D, Cos(R) * D));
  SER := RectF(SP.X, SP.Y, SP.X, Sp.Y);
  InflateRect(SER, D, D);
  EER := RectF(EP.X, EP.Y, EP.X, Ep.Y);
  InflateRect(EER, D, D);

  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.DrawLine(SP, EP, 1);
  Image1.Bitmap.Canvas.DrawRect(RectF(Sp.X, SP.Y, EP.X, EP.Y), 9, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.DrawEllipse(SER, 1);
  Image1.Bitmap.Canvas.DrawEllipse(EER, 1);
//  Image1.Bitmap.Canvas.DrawArc(SP.X, D, (DegToRad(90) - R), 180, 1);
  Image1.Bitmap.Canvas.DrawLine(SP1, EP1, 1);
  Image1.Bitmap.Canvas.DrawLine(SP2, EP2, 1);
  Image1.Bitmap.Canvas.DrawLine(SP1, SP2, 1);
  Image1.Bitmap.Canvas.DrawLine(EP1, EP2, 1);
  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FIndex := 0;

//  Image1.Bitmap.Create(Round(Image1.Width), Round(Image1.Height));
////  Image1.Bitmap.FillColor(claWhite);
//  Image1.Bitmap.Canvas.BeginScene;
//  Image1.Bitmap.Canvas.Fill.Color := claWhite;
//  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
//  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
//  Image1.Bitmap.Canvas.StrokeThickness := 1;
//  Image1.Bitmap.Canvas.DrawRect(Image1.ClipRect, 0, 0, AllCorners, 1);
//  Image1.Bitmap.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.DrawPoint(const AP: TPointF; AC: TAlphaColor);
begin
//  Image1.Bitmap.Canvas.BeginScene;
////  Image1.Bitmap.Canvas.Fill.Color := claWhite;
////  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
//  Image1.Bitmap.Canvas.Stroke.Color := AC;
//  Image1.Bitmap.Canvas.StrokeThickness := 5;
//  Image1.Bitmap.Canvas.DrawEllipse(RectF(AP.X-1,AP.Y-1,AP.X+1,AP.Y+1), 1);
//  Image1.Bitmap.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;
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
//  Image1.Bitmap.BitmapChanged;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  C: TAlphaColor;
begin
//  case FIndex of
//  0:
//    begin
//      FPos1 := PointF(X, Y);
//      Edit1.Text := Format('%f, %f', [X, Y]);
//      C := claBlue;
//    end;
//  1:
//    begin
//      FPos2 := PointF(X, Y);
//      Edit2.Text := Format('%f, %f', [X, Y]);
//      C := claRed;
//    end;
//  else
//    Exit;
//  end;
//
//  DrawPoint(PointF(X, Y), C);
//
//  Inc(FIndex);
end;

procedure TForm1.Log(str: string);
begin
//  Memo1.Lines.Add(Str)
end;

end.
