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
    btnClear: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    ImgView: TImgView32;
    Button1: TButton;
    Button5: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ImgViewResize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }

    // Live layer
    FPath: TThPath;
    FPenColor: TColor32;
    FThickness: Integer;
    FLastPoint: TFloatPoint;

    FMouseDowned: Boolean;

    // Draw layer
    FDrawDatas: TObjectList<TDrawData>;
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

    procedure CreateLiveLayer;
    procedure RecreateLiveLayer;

    procedure PaintDrawHandler(Sender: TObject; Buffer: TBitmap32);

    procedure LiveLayerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LiveLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LiveLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2,
  System.Math,
  clipper, ThGrahicsUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMouseDowned := False;
  FPath := TThPath.Create;

  FThickness := 4;
  FPenColor := clRed32;

  ImgView.Bitmap.SetSize(ImgView.Width, ImgView.Height);
  ImgView.Bitmap.Clear(clGreen32);
//  Image32.scalem

  FBgLayer := TBitmapLayer.Create(ImgView.Layers);
  FBgLayer.Location := FloatRect(0, 0, ImgView.Bitmap.Width, ImgView.Bitmap.Height);
  FBgLayer.Bitmap.SetSize(ImgView.Bitmap.Width, ImgView.Bitmap.Height);
  FBgLayer.Bitmap.Clear(clGreen32);
  FBgLayer.Scaled := True;


  FDrawDatas := TObjectList<TDrawData>.Create;

  FDrawLayer := TPositionedLayer.Create(ImgView.Layers);
  FDrawLayer.Location := FloatRect(0, 0, ImgView.Bitmap.Width, ImgView.Bitmap.Height);
//  FDrawLayer.Bitmap.DrawMode := dmTransparent;
  FDrawLayer.MouseEvents := True;
  FDrawLayer.OnPaint := PaintDrawHandler;
  FDrawLayer.Scaled := True;


  CreateLiveLayer;


//  FClipper := TClipper.Create;

  FPLPaintCount := 0;
end;

procedure TForm1.CreateLiveLayer;
begin
  FLiveLayer :=  TBitmapLayer.Create(ImgView.Layers);
//  FLiveLayer.
  FLiveLayer.Location := FloatRect(0, 0, ImgView.Bitmap.Width, ImgView.Bitmap.Height);
  FLiveLayer.Bitmap.SetSize(ImgView.Bitmap.Width, ImgView.Bitmap.Height);
  FLiveLayer.Bitmap.DrawMode := dmBlend;
  FLiveLayer.MouseEvents := True;
  FLiveLayer.Scaled := True;
//  FLiveLayer.Bitmap.Clear(clYellow32);

  FLiveLayer.OnMouseDown := LiveLayerMouseDown;
  FLiveLayer.OnMouseMove := LiveLayerMouseMove;
  FLiveLayer.OnMouseUp := LiveLayerMouseUp;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDrawDatas.Clear;
  FDrawDatas.Free;
  FPath.Free;

//  FClipper.Free;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Button3Click(nil);
end;

procedure TForm1.ClearPath;
begin
  FPath.Clear;
end;

procedure TForm1.AddPathToDrawLayer;
var
  Data: TDrawData;
begin
  Data := TDrawData.Create(FPath, FThickness, clRed32);
  FDrawDatas.Add(Data);
  FDrawLayer.Update;
end;

procedure TForm1.AddPoint(X, Y: Single);
var
  P: TFloatPoint;
  Poly: TArrayOfFloatPoint;
begin
  P := FloatPoint(X, Y);
  P := P - FLiveLayer.GetAdjustedLocation.TopLeft;

  Label3.Caption := Format('%fx%f', [X, Y]);

  if FPath.Count = 0 then
    Poly := Circle(P, FThickness / 2)
  else
    Poly := BuildPolyline([FLastPoint, P], FThickness, jsRound, esRound);

  PolygonFS(FLiveLayer.Bitmap, Poly, clYellow32);

  FLastPoint := P;
  FPath.Add(P);

  Label1.Caption := FPath.Count.ToString;
end;

procedure TForm1.ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  s: Single;
begin
  s := ImgView.Scale * 1.1;
  if s > 20 then s := 20;
  ImgView.Scale := s;
end;

procedure TForm1.ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  s: Single;
begin
  s := ImgView.Scale / 1.1;
  if s < 0.2 then s := 0.2;
  ImgView.Scale := s;
end;

procedure TForm1.ImgViewResize(Sender: TObject);
begin
  FDrawLayer.Update;
end;

procedure TForm1.LiveLayerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDowned := True;
  ClearPath;
  AddPoint(X, Y);
  FLiveLayer.Update;
end;

procedure TForm1.LiveLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FMouseDowned then
  begin
    AddPoint(X, Y);
    FLiveLayer.Update;
  end;
end;

procedure TForm1.LiveLayerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDowned then
  begin
    FMouseDowned := False;

    RecreateLiveLayer;

    AddPathToDrawLayer;
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  FDrawDatas.Clear;
  FDrawLayer.Update;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ImgView.Scale := ImgView.Scale + 0.2;

  Label2.Caption := ImgView.Scale.ToString;
  Label3.Caption := Format('%f x %f', [FDrawLayer.Location.Right, FDrawLayer.Location.Bottom]);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FDrawLayer.Update;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
begin
  Button4.Caption := ImgView.Layers.Count.ToString;

  for I := 0 to ImgView.Layers.Count - 1 do
  begin
    Memo1.Lines.Add(ImgView.Layers[I].ClassName);
    Memo1.Lines.Add(Format('%fx%f %fx%f', [
      TPositionedLayer(ImgView.Layers[I]).Location.Left,
      TPositionedLayer(ImgView.Layers[I]).Location.Top,
      TPositionedLayer(ImgView.Layers[I]).Location.RIght,
      TPositionedLayer(ImgView.Layers[I]).Location.Bottom
    ]));

    Memo1.Lines.Add(Format('%fx%f %fx%f', [
      TPositionedLayer(ImgView.Layers[I]).GetAdjustedLocation.Left,
      TPositionedLayer(ImgView.Layers[I]).GetAdjustedLocation.Top,
      TPositionedLayer(ImgView.Layers[I]).GetAdjustedLocation.RIght,
      TPositionedLayer(ImgView.Layers[I]).GetAdjustedLocation.Bottom
    ]));
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ImgView.Scale := ImgView.Scale - 0.2;

  Label2.Caption := ImgView.Scale.ToString;
  Label3.Caption := Format('%f x %f', [FDrawLayer.Location.Right, FDrawLayer.Location.Bottom]);
end;

procedure TForm1.PaintDrawHandler(Sender: TObject; Buffer: TBitmap32);
const
  COLORS: array[0..13] of TColor32 = (
    clRed32, clOrange32, clYellow32, clGreen32, clBlue32, clNavy32, clPurple32,
    clBrown32, clLime32, clOlive32, clDarkMagenta32, clCoral32, clDarkCyan32, clDeepPink32
  );
var
  OffsetX, OffsetY: Single;
  Data: TDrawData;
  PolyPoly: TArrayOfArrayOfFloatPoint;
begin
  OffsetX := FDrawLayer.GetAdjustedLocation.Left;
  OffsetY := FDrawLayer.GetAdjustedLocation.Top;
  for Data in FDrawDatas do
  begin
    PolyPoly := Data.PolyPoly;
    PolyPoly := ScalePolyPolygon(PolyPoly, ImgView.Scale, ImgView.Scale);
    PolyPoly := TranslatePolyPolygon(PolyPoly, OffsetX, OffsetY);

    Buffer.ClipRect := FDrawLayer.GetAdjustedLocation.Rect;

    PolyPolygonFS(Buffer, PolyPoly, Data.Color, pfWinding);
  end;
end;

procedure TForm1.RecreateLiveLayer;
begin
  if Assigned(FLiveLayer) then
  begin
    FLiveLayer.Free;
    FLiveLayer := nil;
  end;
  CreateLiveLayer;
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

//    LastPoint := FPath[0] + Form1.FDrawLayer.GetAdjustedLocation.TopLeft;
    LastPoint := FPath[0];
    for I := 1 to FPath.Count - 1 do
    begin
      Point := FPath[I];
//      Point := Point + Form1.FDrawLayer.GetAdjustedLocation.TopLeft;

      Poly := BuildPolyline([LastPoint, Point], FThickness, jsRound, esRound);
      PolyPath := AAFloatPoint2AAPoint(Poly, 1); // TFloatPoint to TIntPoint
      LastPoint := Point;
      if I = 1 then
        AddPath(PolyPath, ptSubject, True)
      else
        AddPath(PolyPath, ptClip, True);
    end;
    Execute(ctUnion, PolyPolyPaths, pftNonZero);

    FPolyPoly := AAPoint2AAFloatPoint(PolyPolyPaths, 1);  // TIntPoint to TFloatPoint
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
