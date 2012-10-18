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
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    edtDepth: TEdit;
    Button2: TButton;
    Edit4: TEdit;
    Button4: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    FIndex: Integer;
    FPos1, FPos2, FPD1, FPD2: TPointF;
    procedure Log(str: string);
    procedure DrawPoint(const AP: TPointF; AC: TAlphaColor); overload;
    procedure DrawPoint(X, Y: Single; AC: TAlphaColor); overload;

    procedure DrawPt(Tag: Integer);
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
  X, Y: Single;
begin
  X := StrToFloatDef(Edit1.Text, 100);
  Y := StrToFloatDef(Edit2.Text, 80);
  DrawPoint(X, Y, claRed);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Fill.Color := claWhite;
  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.DrawRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  X, Y, W, H: Single;
  SP, SP1, SP2, EP, EP1, EP2, P: TPointF;
  D, R: Single;
  SER, EER: TRectF;
  Zoom: Single;
begin
  Zoom := StrToFloatDef(Edit4.Text, 5);

  W := StrToFloatDef(edtWidth.Text, 100) * Zoom;
  H := StrToFloatDef(edtHeight.Text, 80) * Zoom;

  D := StrToFloatDef(edtDepth.Text, 30) * Zoom;
  R := ArcTan(H/W);

  SP  := PointF(100, 100);
  EP  := SP.Add(PointF(W, H));

  P := PointF(Sin(R) * D, Cos(R) * D);
  if P.X > 0 then
  begin

  end;

  SP1 := SP.Add(PointF(Sin(R) * D, -Cos(R) * D));
  SP2 := SP.Add(PointF(-Sin(R) * D, Cos(R) * D));
  EP1 := EP.Add(PointF(Sin(R) * D, -Cos(R) * D));
  EP2 := EP.Add(PointF(-Sin(R) * D, Cos(R) * D));

  SER := RectF(SP.X, SP.Y, SP.X, Sp.Y);
  InflateRect(SER, D, D);
  EER := RectF(EP.X, EP.Y, EP.X, Ep.Y);
  InflateRect(EER, D, D);

  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.StrokeThickness := 1;
  Image1.Bitmap.Canvas.Stroke.Color := claRed;
  Image1.Bitmap.Canvas.DrawLine(SP, EP, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claGray;
  Image1.Bitmap.Canvas.DrawRect(RectF(Sp.X, SP.Y, EP.X, EP.Y), 9, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := claBlack;
  Image1.Bitmap.Canvas.DrawEllipse(SER, 1);
  Image1.Bitmap.Canvas.DrawEllipse(EER, 1);
//  Image1.Bitmap.Canvas.DrawArc(SP.X, D, (DegToRad(90) - R), 180, 1);
  Image1.Bitmap.Canvas.DrawLine(SP1, EP1, 1);
  Image1.Bitmap.Canvas.DrawLine(SP2, EP2, 1);
  Image1.Bitmap.Canvas.DrawLine(SP1, SP2, 1);
  Image1.Bitmap.Canvas.DrawLine(EP1, EP2, 1);

  Image1.Bitmap.Canvas.Stroke.Color := claGreen;
  Image1.Bitmap.Canvas.DrawRect(RectF(Sp2.X, SP1.Y, EP1.X, EP2.Y), 9, 0, AllCorners, 1);

  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.DrawPoint(X, Y: Single; AC: TAlphaColor);
begin
  DrawPoint(PointF(X, Y), AC);
end;

procedure TForm1.DrawPt(Tag: Integer);
var
  Rect: TRectF;
  P, P2, B, DP: TPointF;
  W, H, D, R, Zoom: Single;
begin
  Zoom := StrToFloatDef(Edit4.Text, 5);

  W := StrToFloatDef(edtWidth.Text, 100) * Zoom;
  H := StrToFloatDef(edtHeight.Text, 80) * Zoom;
  Rect.TopLeft := PointF(100, 100);
  Rect.BottomRight := PointF(W, H);


  D := StrToFloatDef(edtDepth.Text, 3);
  P := Rect.CenterPoint;
  R := ArcTan(Rect.Height/Rect.Width);

  B := PointF(Sin(R) * D, Cos(R) * D);
  DP := PointF(B.X, -B.Y);end;

procedure TForm1.DrawPoint(const AP: TPointF; AC: TAlphaColor);
var
  SP, P: TPointF;
  X, Y: Single;
  R: TRectF;
  Zoom: Single;
begin
  Zoom := StrToFloatDef(Edit4.Text, 5);
  SP  := PointF(100, 100);
  X := AP.X * Zoom;
  Y := AP.Y * Zoom;
  P := PointF(X, Y).Add(SP);
  R := RectF(P.X,P.Y,P.X,P.Y);
  R.Inflate(1, 1);

  Image1.Bitmap.Canvas.BeginScene;
//  Image1.Bitmap.Canvas.Fill.Color := claWhite;
//  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.Stroke.Color := AC;
  Image1.Bitmap.Canvas.StrokeThickness := 5;
  Image1.Bitmap.Canvas.DrawEllipse(R, 1);
  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIndex := 0;

  Width := 1000;
  Height := 1000;
  Image1.Width := 800;
  Image1.Height := 800;

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

procedure TForm1.FormShow(Sender: TObject);
begin
  edtWidth.Text := '90';
  edtHeight.Text := '90';
  edtDepth.Text := '3';
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
