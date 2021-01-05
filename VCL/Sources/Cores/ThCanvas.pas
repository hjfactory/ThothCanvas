unit ThCanvas;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Graphics,

  GR32,
  GR32_Image,
  GR32_Layers;

type
  TScaleChangeEvent = procedure(Sender: TObject; Scale: Single) of object;

  TThCustomCanvas = class(TCustomControl)
  private
    FImgView: TImgView32;

    FBackgroundLayer: TBitmapLayer;
    FFreeDrawLayer,
    FShapeDrawLayer: TPositionedLayer;
    FLIveLayer: TBitmapLayer;

    FOnScaleChange: TScaleChangeEvent;

    procedure DoScaleChage(Scale: Single);

    procedure ImgViewWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SetScale(const Value: Single);
    function GetScale: Single;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreatePage(AWidth, AHeight: Integer);

    property Scale: Single read GetScale write SetScale;
    property OnScaleChange: TScaleChangeEvent read FOnScaleChange write FOnScaleChange;
  end;

  TThCanvas = class(TThCustomCanvas)
  end;

implementation

{ TThCanvas }

uses ThTypes;

constructor TThCustomCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImgView := TImgView32.Create(nil);
  FImgView.Parent := Self;
  FImgView.Align := alClient;
  FImgView.Color := clSilver;

  FImgView.OnMouseWheelUp := ImgViewWheelUp;
  FImgView.OnMouseWheelDown := ImgViewWheelDown;
end;

procedure TThCustomCanvas.CreatePage(AWidth, AHeight: Integer);
begin
  FImgView.Bitmap.SetSize(AWidth, AHeight);
  FImgView.Bitmap.Clear(clWhite32);
  FImgView.Bitmap.FrameRectTS(FImgView.Bitmap.BoundsRect, clGray32);

  FBackgroundLayer := TBitmapLayer.Create(FImgView.Layers);
  FBackgroundLayer.Location := FloatRect(0, 0, FImgView.Bitmap.Width, FImgView.Bitmap.Height);
  FBackgroundLayer.Bitmap.SetSize(FImgView.Bitmap.Width, FImgView.Bitmap.Height);
  FBackgroundLayer.Bitmap.Clear(clWhite32);
  FBackgroundLayer.Scaled := True;


  FFreeDrawLayer := TPositionedLayer.Create(FImgView.Layers);
  FFreeDrawLayer.Location := FloatRect(0, 0, FImgView.Bitmap.Width, FImgView.Bitmap.Height);
//  FFreeDrawLayer.Bitmap.DrawMode := dmTransparent;
  FFreeDrawLayer.MouseEvents := True;
  FFreeDrawLayer.Scaled := True;

end;

procedure TThCustomCanvas.CreateWnd;
begin
  inherited;

  FImgView.ScrollBars.Visibility := svAuto;
end;

destructor TThCustomCanvas.Destroy;
begin
  FImgView.Free;

  inherited;
end;

procedure TThCustomCanvas.DoScaleChage(Scale: Single);
begin
  if Assigned(FOnScaleChange) then
    FOnScaleChange(Self, Scale);
end;

function TThCustomCanvas.GetScale: Single;
begin
  Result := FImgView.Scale;
end;

procedure TThCustomCanvas.SetScale(const Value: Single);
var
  LScale: Single;
begin
  if Value < TH_SCALE_MIN then
    LScale := TH_SCALE_MIN
  else if Value > TH_SCALE_MAX then
    LScale := TH_SCALE_MAX
  else
    LScale := Value
  ;
  if GetScale = LScale then
    Exit;

  FImgView.Scale := LScale;
  DoScaleChage(LScale);
end;

procedure TThCustomCanvas.ImgViewWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Scale := Scale + 0.1;
end;

procedure TThCustomCanvas.ImgViewWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Scale := Scale - 0.1;
end;

end.
