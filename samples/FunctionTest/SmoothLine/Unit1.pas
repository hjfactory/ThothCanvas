unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDownPos: TPointF;
    FImage: TImage;

    procedure DoImageResize(X, Y: Integer);

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ImageResize(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoImageResize(X, Y: Integer);
begin
  FImage.Bitmap.Canvas.BeginScene;
  FImage.Bitmap.SetSize(X, Y);
  FImage.Bitmap.Canvas.Fill.DefaultColor := claBlack;
  FImage.Bitmap.Canvas.Fill.Kind := TBrushKind.bkSolid;
//  FImage.Bitmap.Canvas.Fill.Color := claBlack;
  FImage.Bitmap.Canvas.FillRect(FImage.ClipRect, 0, 0, AllCorners, 1);
  FImage.Bitmap.Canvas.EndScene;
  FImage.Bitmap.BitmapChanged;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align := TAlignLayout.alClient;
  FImage.OnMouseDown := MouseDown;
  FImage.OnMouseMove := MouseMove;
  FImage.OnResize := ImageResize;
  FImage.Bitmap.Create(0, 0);
  DoImageResize(Round(FImage.Width), Round(FImage.Height));
  FImage.Visible := True;

end;

procedure TForm1.ImageResize(Sender: TObject);
begin
  DoImageResize(Round(TImage(Sender).Width), Round(TImage(Sender).Height));
end;

procedure TForm1.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
    FDownPos := PointF(X, Y);
end;

procedure TForm1.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    with FImage.Bitmap.Canvas do
    begin
      BeginScene;
      Stroke.Color := claRed;
      StrokeThickness := 3;
      DrawLine(FDownPos, PointF(X, Y), 1);
      EndScene;
    end;
    FImage.Bitmap.BitmapChanged;

    FDownPos := PointF(X, Y);
  end;
end;

end.
