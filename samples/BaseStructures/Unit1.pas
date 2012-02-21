{
 이 샘플은 TShape를 이용해서 Draw하는 test coding이다.
 향후 Canvas에 직접 그리는 방식으로 검토 필요
}
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, ThothCanvas, ThothObjects, ObjectManager;

type
///////////////////////////////////////////////////////
// Main form
  TForm1 = class(TForm)
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button7: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FObjectManager: TThothObjectManager;
    FThCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FObjectManager := TThothObjectManager.Create;

  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.SetSubject(FObjectManager);
  FThCanvas.Parent := Self;
  FThCanvas.Position.Point := PointF(10, 40);
  FThCanvas.Width := 530;
  FThCanvas.Height := 450;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FObjectManager.Free;
  FThCanvas.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FThCanvas.DrawClass := nil;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FObjectManager.test;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThLine;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  P: TPointF;
begin
  Randomize;
  FThCanvas.BeginUpdate;
  for I := 0 to 500 do
  begin
    P := PointF(Random(Trunc(FThCanvas.Width - 200)), Random(Trunc(FThCanvas.Height - 200)));

    FThCanvas.DrawShape(TThRectangle,
      P,
      PointF(P.X + Random(150) + 30, P.Y + Random(150) + 30)
    );
  end;
  FThCanvas.EndUpdate;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThRectangle;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThCircle;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  bmp: TBitmap;
  Stream: TMemoryStream;
begin
  bmp := TBitmap.Create(Trunc(FThCanvas.Width), Trunc(FThCanvas.Height));

  Stream := TMemoryStream.Create;

  FThCanvas.SaveToStream(Stream);

  bmp.LoadFromStream(Stream);
//  bmp.Canvas.
  bmp.BitmapChanged;

  Image1.Bitmap.Assign(bmp);

  Stream.Free;
end;

end.
