unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.Objects, FMX.Edit;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
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

{$R *.fmx}

uses
  System.Math;

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
  Rad, W, H: Single;
  R: TRectF;
  Range: Integer;

  X0, Y0, Y1, Y2: Single;
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
      Range := StrToIntDef(Edit3.Text, 3);

      Image1.Bitmap.Canvas.BeginScene;
      Image1.Bitmap.Canvas.Stroke.Color := claBlack;
      Image1.Bitmap.Canvas.DrawLine(FPos1, FPos2, 1);
      Image1.Bitmap.Canvas.Stroke.Color := claGray;
      Image1.Bitmap.Canvas.StrokeDash := TStrokeDash.sdDashDotDot;
      Image1.Bitmap.Canvas.DrawLine(PointF(FPos1.X - Range, FPos1.Y), PointF(FPos2.X, FPos2.Y - Range), 1);
      Image1.Bitmap.Canvas.DrawLine(PointF(FPos1.X + Range, FPos1.Y), PointF(FPos2.X, FPos2.Y + Range), 1);
      Image1.Bitmap.Canvas.EndScene;
      Image1.Bitmap.BitmapChanged;
    end;
  else
    C := claGreen;
    Range := StrToIntDef(Edit3.Text, 3);

    R.Left := Min(FPos1.X, FPos2.X);
    R.Right := Max(FPos1.X, FPos2.X);
    R.Top := Min(FPos1.Y, FPos2.Y);
    R.Bottom := Max(FPos1.Y, FPos2.Y);

    W := Abs(FPos1.X - FPos2.X);
    H := Abs(FPos1.Y - FPos2.Y);

    if W < Range * 2 then
    begin
      R.Left := R.Left - (Range * 2 - W) / 2;
      R.Right := R.Right + (Range * 2 - W) / 2;
    end;

    if H < Range * 2 then
    begin
      R.Top := R.Top - (Range * 2 - H) / 2;
      R.Bottom := R.Bottom + (Range * 2 - H) / 2;
    end;

    if not PtInRect(R, PointF(X, Y)) then
      Exit;

    Log(Format('W: %f, H: %f', [W, H]));

    if W = 0 then
    begin

    end
    else if H = 0 then
    end
    else
    begin
      Rad := ArcTan(H/W);

      X0 := Abs(X - Min(FPos1.X, FPos2.X));
      Y0 := Abs(Y - Max(FPos1.Y, FPos2.Y));

      Log(Format('X: %f, Y: %f', [X0, Y0]));
      Y1 := tan(Rad) * (X0 - Range);
      Y2 := tan(Rad) * (X0 + Range);
      Log(Format('Calc Y0: %f, Y1: %f, Y2: %f', [Tan(Rad) * (X0), Y1, Y2]));

      if (Y1 <= Y0) and (Y2 >= Y0) then
        Log('correct point.');
    end;

  end;

  DrawPoint(PointF(X, Y), C);

  Inc(FIndex);
end;

procedure TForm1.Log(str: string);
begin
  Memo1.Lines.Add(Str)
end;

end.
