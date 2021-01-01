unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,

  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons,
  GR32_Clipper,
  GR32_VectorUtils;

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


    FLiveLayer: TBitmapLayer;
    FDrawLayer: TPositionedLayer;
    FClipper: TClipper;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMouseDowned := False;
  FPath := TList<TFloatPoint>.Create;

  FThickness := 12;
  FPenColor := clRed32;

  Image32.Bitmap.SetSize(640, 480);
  Image32.Bitmap.Clear(clWhite32);

  FDrawDatas := TList<TDrawData>.Create;

  FDrawLayer := TPositionedLayer.Create(Image32.Layers);
  FDrawLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
//  FDrawLayer.Bitmap.DrawMode := dmTransparent;
  FDrawLayer.MouseEvents := True;
  FDrawLayer.OnPaint := PaintDrawHandler;

  FClipper := TClipper.Create;

  FPLPaintCount := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearDrawDatas;
  FDrawDatas.Free;
  FPath.Free;

  FClipper.Free;
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
  FClipper.Clear;
//  FPolyPoly := nil;

  CreateLiveLayer;
end;

function TForm1.CreateLiveLayer: TBitmapLayer;
begin
  FLiveLayer :=  TBitmapLayer.Create(Image32.Layers);
  FLiveLayer.Location := FloatRect(0, 0, Image32.Bitmap.Width, Image32.Bitmap.Height);
  FLiveLayer.Bitmap.SetSize(Image32.Bitmap.Width, Image32.Bitmap.Height);
  FLiveLayer.Bitmap.DrawMode := dmTransparent;
  FLiveLayer.MouseEvents := True;
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
  FMouseDowned := False;
  FLiveLayer.Free;
  FLiveLayer := nil;

  AddPathToDrawLayer;
  FDrawLayer.Update;
//  FLiveLayer.Bitmap.Clear;
//  FLiveLayer.Bitmap.DrawMode := dmTransparent;
//  FLiveLayer.Bitmap.CombineMode := cmMerge;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearDrawDatas;
  FDrawLayer.Update;
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
var
  Data: TDrawData;
begin
  for Data in FDrawDatas do
  begin
    PolyPolygonFS(Buffer, Data.PolyPoly, Data.Color);
  end;
//  PolyPolygonFS(Buffer, PolyPoly, FPenColor);
end;

{ TDrawData }

constructor TDrawData.Create(APath: TThPath; AThickness: Integer;
  AColor: TColor32);
var
  I: Integer;
  LP, CP: TFloatPoint;
  Clipper: TClipper;
  Poly: TArrayOfFloatPoint;
begin
  FPath := TThPath.Create;
  FPath.AddRange(APath);
//  FPath.  := APath;
  FThickness := AThickness;
  FColor := AColor;

  Clipper := TClipper.Create;

  LP := FPath[0];
  for I := 1 to FPath.Count - 1 do
  begin
    CP := FPath[I];
    Poly := BuildPolyline([LP, CP], FThickness, jsRound, esRound);
    LP := CP;
    Clipper.AddPath(Poly, ptClip);
  end;
  Clipper.Execute(ctUnion, frNonZero, FPolyPoly);
  Clipper.Free;
end;

destructor TDrawData.Destroy;
begin
  FPath.Free;
  inherited;
end;

end.
