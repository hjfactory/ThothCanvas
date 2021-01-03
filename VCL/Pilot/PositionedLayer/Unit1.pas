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
    Memo1: TMemo;
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
    procedure Button7Click(Sender: TObject);
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

    procedure ClearPath;
    procedure AddPoint(X, Y: Single);

    procedure AddPathToDrawLayer;

    function CreateLiveLayer: TBitmapLayer;
    procedure ClearDrawDatas;

    procedure PaintDrawHandler(Sender: TObject; Buffer: TBitmap32);
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

  FThickness := 4;
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

procedure TForm1.Button3Click(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := TPositionedLayer(Image32.Layers[0]);

  L.Update;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Button4.Caption := Image32.Layers.Count.ToString;
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
  for Data in FDrawDatas do
  begin
    PolyPolygonFS(Buffer, Data.PolyPoly, Data.Color, pfWinding);
  end;
end;

{ TDrawData }
//------------------------------------------------------------------------------

function AAFloatPoint2AAPoint(const APolyPoly: TArrayOfArrayOfFloatPoint;
  Decimals: Integer = 0): TPaths; overload;
var
  I, J, DecScale: integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APolyPoly));
  for I := 0 to high(APolyPoly) do
  begin
    Setlength(Result[I], Length(APolyPoly[I]));
    for J := 0 to High(APolyPoly[I]) do
    begin
      Result[I][J].X := Round(APolyPoly[I][J].X * DecScale);
      Result[I][J].Y := Round(APolyPoly[I][J].Y * DecScale);
    end;
  end;
end;

function AAFloatPoint2AAPoint(const APoly: TArrayOfFloatPoint;
  Decimals: Integer = 0): TPath; overload;
var
  I, DecScale: Integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APoly));
  for I := 0 to High(APoly) do
  begin
    Result[I].X := Round(APoly[I].X * DecScale);
    Result[I].Y := Round(APoly[I].Y * DecScale);

    if Round(APoly[I].X) <> APoly[I].X then
      OutputDebugString(PChar(Format('%d : %d = %f', [I, Round(APoly[I].X), APoly[I].X])));
  end;
end;

//------------------------------------------------------------------------------

function AAPoint2AAFloatPoint(const APaths: TPaths;
  Decimals: Integer = 0): TArrayOfArrayOfFloatPoint;
var
  I, J, DecScale: Integer;
begin
  DecScale := Round(Power(10, Decimals));
  Setlength(Result, Length(APaths));
  for I := 0 to High(APaths) do
  begin
    Setlength(Result[i], Length(APaths[i]));
    for J := 0 to High(APaths[I]) do
    begin
      Result[I][J].X := APaths[I][J].X / DecScale;
      Result[I][J].Y := APaths[I][J].Y / DecScale;
    end;
  end;
end;
//------------------------------------------------------------------------------

constructor TDrawData.Create(APath: TThPath; AThickness: Integer;
  AColor: TColor32);
var
  I: Integer;
  LastPoint, Point: TFloatPoint;
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
  with TClipper.Create do
  try
    StrictlySimple := True;

    LastPoint := FPath[0];
    for I := 1 to FPath.Count - 1 do
    begin
      Point := FPath[I];
      Poly := BuildPolyline([LastPoint, Point], FThickness, jsRound, esRound);
      PolyPath := AAFloatPoint2AAPoint(Poly);
      LastPoint := Point;
      if I = 1 then
        AddPath(PolyPath, ptSubject, True)
      else
        AddPath(PolyPath, ptClip, True);
    end;
//    Execute(ctUnion, PolyPolyPaths, pftEvenOdd);
    Execute(ctUnion, PolyPolyPaths, pftNonZero);
    FPolyPoly := AAPoint2AAFloatPoint(PolyPolyPaths);
  finally
    Free;
  end;
end;

destructor TDrawData.Destroy;
begin
  FPath.Free;
  inherited;
end;

end.
