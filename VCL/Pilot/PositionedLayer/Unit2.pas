unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons,
  GR32_Clipper,
  GR32_VectorUtils, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TForm2 = class(TForm)
    Image32: TImage32;
    Button1: TButton;
    Button2: TButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FPenColor: TColor32;
    FThickness: Integer;

    FDrawLayer: TBitmapLayer;
    FPath: TArray<TFloatPoint>;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.LoadFromFile('D:\Works\ThothCanvas\VCL\Pilot\PositionedLayer\test.dmp');
  SetLength(FPath, Stream.Size div SizeOf(TFloatPoint));
  Stream.Position := 0;
  Stream.Read(PByte(FPath)[0], Stream.Size);// 828

  SpinEdit2.Value := Length(FPath)-1;

  Stream.Free;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  P, LP: TFloatPoint;
  I, S, E: Integer;
  Poly: TArrayOfFloatPoint;
  C: TColor32;
begin
  FDrawLayer.Bitmap.Clear(clWhite32);

  S := SpinEdit1.Value;
  E := SpinEdit2.Value;

  LP := FPath[S];
  for I := S+1 to E do
  begin
    P := FPath[I];
    Poly := BuildPolyline([LP, P], FThickness, jsRound, esRound);
    PolygonFS(FDrawLayer.Bitmap, Poly, clYellow32);
    PolylineFS(FDrawLayer.Bitmap, Circle(P, 1), clBlack32);
    LP := P;
  end;
  C := clBlack32;
  for I := S+1 to E do
  begin
    P := FPath[I];
    if I = S+1 then
      PolygonFS(FDrawLayer.Bitmap, Circle(P, 2), clRed32)
    else if I = E then
      PolygonFS(FDrawLayer.Bitmap, Circle(P, 2), clBlue32)
    else
      PolygonFS(FDrawLayer.Bitmap, Circle(P, 2), C);
    C := C + $00010101;
  end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FThickness := 12;
  FPenColor := clRed32;

  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
//  Image32.Bitmap.Clear(clGreen32);

  FDrawLayer := TBitmapLayer.Create(Image32.Layers);
  FDrawLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
  FDrawLayer.Bitmap.SetSize(Image32.Bitmap.Width, Image32.Bitmap.Height);
  FDrawLayer.Bitmap.Clear(clWhite32);

end;

end.
