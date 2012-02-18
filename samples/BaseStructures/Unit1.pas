{
 이 샘플은 TShape를 이용해서 Draw하는 test coding이다.
 향후 Canvas에 직접 그리는 방식으로 검토 필요
}
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, Winapi.Windows, ThothCanvas, ThothObjects;

type
///////////////////////////////////////////////////////
// Main form
  TForm1 = class(TForm)
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Path1: TPath;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
//  Text1.Text := 'asdfasdfasfd';
//  fsPascal1.
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Bmp: TBitmap;
begin
//  ScrollBox1.SaveToStream();
//  Image1.Position.X := ScrollBox1.Position.X;
//  Image1.Position.Y := ScrollBox1.Position.Y;
//  Image1.Width := SCrollBox1.Width;
//  Image1.Height := SCrollBox1.Height;

//  Bmp.Canvas.
//  Image1.Bitmap.Canvas.

//  Image1.
//  Image1.Canvas.BeginScene;
//  ScrollBox1.PaintTo(Image1.Bitmap.Canvas, RectF(0, 0, 100, 100));
//  Image1.PaintTo(ScrollBox1.Canvas, RectF(0, 0, 100, 100));
//  Image1.Bitmap.Canvas.d
//  Image1.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;


end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThLine;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThRectangle;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Parent := Self;
  FThCanvas.Position.Point := PointF(10, 40);
  FThCanvas.Width := 700;
  FThCanvas.Height := 580;
//  FThCanvas.OnMouseDown := ScrollBox1MouseDown;
//  FThCanvas.OnMouseUp := ScrollBox1MouseUp;
//  FThCanvas.OnMouseMove := ScrollBox1MouseMove;
//  FThCanvas.parent

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;
end;


end.
