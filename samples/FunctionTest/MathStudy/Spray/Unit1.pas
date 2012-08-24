{
  Spray   : 마우스 움직임에 맞춰 일정주기로 원안에 임의의 점을 반복하여 찍어
  Pen     :
}

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Edit;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    edtRadius: TEdit;
    Label2: TLabel;
    edtSprayInterval: TEdit;
    Label3: TLabel;
    edtSprayCount: TEdit;
    Button1: TButton;
    Timer1: TTimer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FDOwn: Boolean;
    FPos: TPointF;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

  Image1.Bitmap := TBitmap.Create(Round(Image1.Width), Round(Image1.Height));
  Image1.Bitmap.Canvas.BeginScene;
  Image1.Bitmap.Canvas.Fill.Color := claWhite;
  Image1.Bitmap.Canvas.FillRect(Image1.ClipRect, 0, 0, AllCorners, 1);
  Image1.Bitmap.Canvas.EndScene;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not (ssLeft in Shift) then
    Exit;

  FDown := True;

  FPos := PointF(X, Y);

  Timer1.Interval := StrToIntDef(edtSprayInterval.Text, 10);
  Timer1.Enabled := True;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FDown then
    FPos := PointF(X, Y);
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FDown := False;

  Timer1.Enabled := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
  procedure DrawPixel(X, Y: Integer);
  begin
    Image1.Bitmap.Pixels[X, Y] := claRed;
    Image1.Bitmap.BitmapChanged;
  end;

var
  I,
  C, R: Integer;
  X, Y: Integer;

  Deg, Rad: Integer;
begin
  if not FDown then Exit;

  R := StrToIntDef(edtRadius.Text, 15);
  C := StrToIntDef(edtSprayCount.Text, 15);

  for I := 0 to C - 1 do
  begin
    Deg := Random(360);
    Rad := Random(R);

    X := Round(FPos.X + Cos(DegToRad(Deg)) * Rad);
    Y := Round(FPos.Y + Sin(DegToRad(Deg)) * Rad);

    DrawPixel(X, Y);
  end;
end;

end.
