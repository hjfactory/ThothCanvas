unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,

  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons,
//  GR32_Clipper,
  GR32_VectorUtils, Vcl.Samples.Spin;

type
  TThPath = TList<TFloatPoint>;

  TDrawData = class
  private
    FPath: TThPath;
    FThickness: Integer;
    FColor: TColor32;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
  public
    constructor Create(APath: TThPath; AThickness: Integer; AColor: TColor32);
    destructor Destroy; override;

    property Path: TThPath read FPath;
    property Thickness: Integer read FThickness;
    property Color: TColor32 read FColor;
    property PolyPoly: TArrayOfArrayOfFloatPoint read FPolyPoly;
  end;

  TForm1 = class(TForm)
    Image32: TImage32;
    Button1: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    Button4: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Button8: TButton;
    SpinEdit3: TSpinEdit;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SpinEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button8Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }

    // Live layer
    FPath: TThPath;
    FPenColor: TColor32;
    FThickness: Integer;
    FLastPoint: TFloatPoint;

    FMouseDowned: Boolean;

    // Draw layer
    FDrawDatas: TList<TDrawData>;
//    FPolyPoly: TArrayOfArrayOfFloatPoint;

    FBgLayer,
    FLiveLayer: TBitmapLayer;

    FDrawLayer: TPositionedLayer;
//    FClipper: TClipper;
//    FRenderer: TPolygonRenderer32VPR;

    FPLPaintCount: Integer;
    FDP: TFloatPoint;

    procedure ClearPath;
    procedure AddPoint(X, Y: Single);

    procedure AddPathToDrawLayer;

    function CreateLiveLayer: TBitmapLayer;
    procedure ClearDrawDatas;

    procedure PaintDrawHandler(Sender: TObject; Buffer: TBitmap32);
    procedure PaintDrawHandler2(Sender: TObject; Buffer: TBitmap32);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2,
  System.Math,
  clipper;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMouseDowned := False;
  FPath := TThPath.Create;

  FThickness := 12;
  FPenColor := clRed32;

  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
//  Image32.Bitmap.Clear(clGreen32);

  FBgLayer := TBitmapLayer.Create(Image32.Layers);
  FBgLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
  FBgLayer.Bitmap.SetSize(Image32.Bitmap.Width, Image32.Bitmap.Height);
  FBgLayer.Bitmap.Clear(clWhite32);


  FDrawDatas := TList<TDrawData>.Create;

  FDrawLayer := TPositionedLayer.Create(Image32.Layers);
  FDrawLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
//  FDrawLayer.Bitmap.DrawMode := dmTransparent;
  FDrawLayer.MouseEvents := True;
  FDrawLayer.OnPaint := PaintDrawHandler;

//  FClipper := TClipper.Create;

  FPLPaintCount := 0;

  SpinEdit1.Value := 1;
  SpinEdit2.Value := 98;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  C, P: Integer;
  Stream: TMemoryStream;
  Path: TArray<TFloatPoint>;
  FP: TFloatPoint;
begin
  Button1Click(nil);

  // 670 / 745 이상하게 표시
  C := SpinEdit2.Value;
  P := SpinEdit1.Value;

  Stream := TMemoryStream.Create;
  Stream.LoadFromFile('D:\Works\ThothCanvas\VCL\Pilot\PositionedLayer\test.dmp');

  if (C = 0) or (C > Stream.Size div SizeOf(TFloatPoint)) then
  begin
//    P := 0;
    C := Stream.Size div SizeOf(TFloatPoint);
  end;
  Caption := Format('%d / %d', [P, C]);

  SetLength(Path, C-P);
  Stream.Position := SizeOf(TFloatPoint) * P;
  Stream.Read(PByte(Path)[0], SizeOf(TFloatPoint) * (C-P));// 828
  Stream.Free;

  FPath.Clear;
  FPath.AddRange(Path);

  AddPathToDrawLayer;
  FDrawLayer.Update;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearDrawDatas;
  FDrawDatas.Free;
  FPath.Free;

//  FClipper.Free;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Button3Click(nil);
end;

procedure TForm1.ClearDrawDatas;
var
  Item: TDrawData;
begin
  for Item in FDrawDatas do
  begin
//    Item.Path.Free;
    Item.Free;
  end;
  FDrawDatas.Clear;
end;

procedure TForm1.ClearPath;
begin
  FPath.Clear;
//  FClipper.Clear;
//  FPolyPoly := nil;

  CreateLiveLayer;
end;

function TForm1.CreateLiveLayer: TBitmapLayer;
begin
  FLiveLayer :=  TBitmapLayer.Create(Image32.Layers);
  FLiveLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
  FLiveLayer.Bitmap.SetSize(Image32.Bitmap.Width, Image32.Bitmap.Height);
//  FLiveLayer.Bitmap.DrawMode := dmTransparent;
  FLiveLayer.Bitmap.DrawMode := dmBlend;
  FLiveLayer.MouseEvents := True;
//  FLiveLayer.LayerOptions
end;

procedure TForm1.AddPathToDrawLayer;
var
  Data: TDrawData;
begin
  Data := TDrawData.Create(FPath, FThickness, clRed32);
  FDrawDatas.Add(Data);
end;

procedure TForm1.AddPoint(X, Y: Single);
var
  P: TFloatPoint;
  Poly: TArrayOfFloatPoint;
begin
  P := FloatPoint(X, Y);

  if FPath.Count = 0 then
    Poly := Circle(P, FThickness / 2)
  else
    Poly := BuildPolyline([FLastPoint, P], FThickness, jsRound, esRound);

  PolygonFS(FLiveLayer.Bitmap, Poly, clYellow32);

  FLastPoint := P;
  FPath.Add(P);

  Label3.Caption := FPath.Count.ToString;
end;

procedure TForm1.Image32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if not Assigned(Layer) then
    Exit;

  FMouseDowned := True;
  ClearPath;
  AddPoint(X, Y);
  Layer.Update;
end;

procedure TForm1.Image32MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  if FMouseDowned then
  begin
    AddPoint(X, Y);
    Layer.Update;
  end;
end;

procedure TForm1.Image32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FMouseDowned then
  begin
    FMouseDowned := False;
    FLiveLayer.Free;
    FLiveLayer := nil;

    AddPathToDrawLayer;
    FDrawLayer.Update;
  end;
//  FLiveLayer.Bitmap.Clear;
//  FLiveLayer.Bitmap.DrawMode := dmTransparent;
//  FLiveLayer.Bitmap.CombineMode := cmMerge;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearDrawDatas;
  FDrawLayer.Update;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.Write(PByte(FPath.ToArray)[0], SizeOf(TFloatPoint) * FPath.Count);
  Stream.SaveToFile('D:\Works\ThothCanvas\VCL\Pilot\PositionedLayer\test.dmp');
  Stream.Free;

//  FPath.
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  I: Integer;
  P: TFloatPoint;
begin
  Memo1.Lines.Clear;

  I := 0;
  for P in FPath do
  begin
    Memo1.Lines.Add(Format('%d > %fx%f', [I, P.X, P.Y]));
    Inc(I);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  n: Integer;
begin
  n := SpinEdit3.Value;
  if n < FPath.Count then
  begin
    FDP := FPath[n];
    FDrawLayer.Update;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := TPositionedLayer(Image32.Layers[0]);

  L.Update;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
//  Button4.Caption := Image32.Layers.Count.ToString;
  form2.Show;
end;

procedure TForm1.PaintDrawHandler(Sender: TObject; Buffer: TBitmap32);
const
  COLORS: array[0..13] of TColor32 = (
    clRed32, clOrange32, clYellow32, clGreen32, clBlue32, clNavy32, clPurple32,
    clBrown32, clLime32, clOlive32, clDarkMagenta32, clCoral32, clDarkCyan32, clDeepPink32
  );
var
  I: Integer;
  Data: TDrawData;
  Poly: TArrayOfFloatPoint;
begin
  if CheckBox1.Checked then
  begin
    for Data in FDrawDatas do
    begin
      PolyPolygonFS(Buffer, Data.PolyPoly, Data.Color, pfWinding);
    end;
  end
  else
  begin
    for Data in FDrawDatas do
    begin
      Memo1.Lines.Clear;
      for I := 0 to Length(Data.PolyPoly) - 1 do
      begin
        PolygonFS(Buffer, Data.PolyPoly[I], COLORS[I mod Length(COLORS)]);
        Memo1.Lines.Add(Format('%d - %d', [I, Length(Data.PolyPoly[I])]));
      end;
    end;
  end;

  PolygonFS(Buffer, Circle(FDP, FThickness / 2), clBlack32);
//  PolyPolygonFS(Buffer, PolyPoly, FPenColor);
end;

procedure TForm1.PaintDrawHandler2(Sender: TObject; Buffer: TBitmap32);
var
  Poly: TArrayOfFloatPoint;
begin
//  Poly := BuildPolyline([FloatPoint(110, 10), FloatPoint(200, 200)], 10, jsRound, esRound);
  Poly := BuildPolyline([FloatPoint(239, 30), FloatPoint(247, 36)], 12, jsRound, esRound);
  PolygonFS(Buffer, Poly, clYellow32);
end;

procedure TForm1.SpinEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
    Button5Click(nil);
  end;
end;

{ TDrawData }
//------------------------------------------------------------------------------

function AAFloatPoint2AAPoint(const a: TArrayOfArrayOfFloatPoint;
  decimals: integer = 0): TPaths; overload;
var
  i,j,decScale: integer;
begin
  decScale := round(power(10,decimals));
  setlength(result, length(a));
  for i := 0 to high(a) do
  begin
    setlength(result[i], length(a[i]));
    for j := 0 to high(a[i]) do
    begin
      result[i][j].X := round(a[i][j].X *decScale);
      result[i][j].Y := round(a[i][j].Y *decScale);
    end;
  end;
end;

function AAFloatPoint2AAPoint(const a: TArrayOfFloatPoint;
  decimals: integer = 0): TPath; overload;
var
  i,j,decScale: integer;
begin
  decScale := round(power(10,decimals));
  setlength(result, length(a));
  for i := 0 to high(a) do
  begin
    result[i].X := round(a[i].X *decScale);
    result[i].Y := round(a[i].Y *decScale);
  end;
end;

//------------------------------------------------------------------------------

function AAPoint2AAFloatPoint(const a: TPaths;
  decimals: integer = 0): TArrayOfArrayOfFloatPoint;
var
  i,j,decScale: integer;
begin
  decScale := round(power(10,decimals));
  setlength(result, length(a));
  for i := 0 to high(a) do
  begin
    setlength(result[i], length(a[i]));
    for j := 0 to high(a[i]) do
    begin
      result[i][j].X := a[i][j].X /decScale;
      result[i][j].Y := a[i][j].Y /decScale;
    end;
  end;
end;
//------------------------------------------------------------------------------

constructor TDrawData.Create(APath: TThPath; AThickness: Integer;
  AColor: TColor32);
var
  I: Integer;
  LP, CP: TFloatPoint;
  Clipper: TClipper;
  Poly: TArrayOfFloatPoint;
  PolyPath: TPath;
  PolyPolyPaths: TPaths;
begin
  FPath := TThPath.Create;
  FPath.AddRange(APath);
  FThickness := AThickness;
  FColor := AColor;


  // GR32 Clipper는 일부 선이 겹치는 경우 오류 발생
    // Clipper libarary(Libraries\clipper.pas)로 대체
  Clipper := TClipper.Create;

  LP := FPath[0];
  for I := 1 to FPath.Count - 1 do
  begin
    CP := FPath[I];
    Poly := BuildPolyline([LP, CP], FThickness, jsRound, esRound);
    PolyPath := AAFloatPoint2AAPoint(Poly);
    LP := CP;
    if I = 1 then
      Clipper.AddPath(PolyPath, ptSubject, True)
    else
      Clipper.AddPath(PolyPath, ptClip, True);
  end;
  Clipper.Execute(ctUnion, PolyPolyPaths, pftNonZero);
  FPolyPoly := AAPoint2AAFloatPoint(PolyPolyPaths);
  Clipper.Free;

end;

destructor TDrawData.Destroy;
begin
  FPath.Free;
  inherited;
end;

end.
