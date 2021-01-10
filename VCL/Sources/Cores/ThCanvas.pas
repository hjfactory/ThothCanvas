unit ThCanvas;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Graphics,

  GR32,
  GR32_Image,
  GR32_Layers,

  ThTypes,
  ThCanvasLayers;

type
  TScaleChangeEvent = procedure(Sender: TObject; Scale: Single) of object;

  TThCustomCanvas = class(TCustomControl)
  private
    FImgView: TImgView32;

    FMouseDowned: Boolean;
    FMouseDownPoint: TPoint;

    FBackgroundLayer: TBitmapLayer;
    FFreeDrawLayer: TFreeDrawLayer;
//    FShapeDrawLayer: TPositionedLayer;

    FOnScaleChange: TScaleChangeEvent;
    FPenColor: TColor;
    FPenSize: Integer;
    FPenOpacity: TThPercent;
    FCanvasMode: TThCanvasMode;

    procedure DoScaleChage(Scale: Single);

    procedure SetScale(const Value: Single);
    function GetScale: Single;

    ////////////////////////////
    // Event
    procedure ImgViewResize(Sender: TObjecT);

    // Mouse
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    // Wheel
    procedure ImgViewMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);

    procedure SetPenColor(const Value: TColor);
    procedure SetPenSize(const Value: Integer);
    procedure SetPenOpacity(const Value: TThPercent);
    procedure SetCanvasMode(const Value: TThCanvasMode);
    function GetPenDrawMode: TThFreeDrawMode;
    procedure SetPenDrawMode(const Value: TThFreeDrawMode);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreatePage(AWidth: Integer = 0; AHeight: Integer = 0);

    property PenColor: TColor read FPenColor write SetPenColor;
    property PenSize: Integer read FPenSize write SetPenSize;
    property PenOpacity: TThPercent read FPenOpacity write SetPenOpacity;
    property PenDrawMode: TThFreeDrawMode read GetPenDrawMode write SetPenDrawMode;

    property Scale: Single read GetScale write SetScale;
    property OnScaleChange: TScaleChangeEvent read FOnScaleChange write FOnScaleChange;

    procedure Move(AX, AY: Integer);
    procedure Test;

    procedure Clear;

    property CanvasMode: TThCanvasMode read FCanvasMode write SetCanvasMode;
  end;

  TThCanvas = class(TThCustomCanvas)
  public
    property OnScaleChange;
  end;

implementation

{ TThCanvas }

procedure TThCustomCanvas.Clear;
begin
  FFreeDrawLayer.Clear;
end;

constructor TThCustomCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImgView := TImgView32.Create(nil);
  FImgView.Parent := Self;
  FImgView.Align := alClient;

  FImgView.Color := clSilver;
  FImgView.Centered := False;

  FImgView.OnResize := ImgViewResize;

  FImgView.OnMouseDown := ImgViewMouseDown;
  FImgView.OnMouseMove := ImgViewMouseMove;
  FImgView.OnMouseUp := ImgViewMouseUp;

  FImgView.OnMouseWheelUp := ImgViewMouseWheelUp;
  FImgView.OnMouseWheel := ImgViewMouseWheel;
  FImgView.OnMouseWheelDown := ImgViewMouseWheelDown;
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

procedure TThCustomCanvas.CreatePage(AWidth, AHeight: Integer);
begin
  if (AWidth = 0) or (AWidth = 0) then
    FImgView.Bitmap.SetSize(2480, 3508)
  else
    FImgView.Bitmap.SetSize(AWidth, AHeight);
  FImgView.Bitmap.Clear(clGray32);

  FBackgroundLayer := TBitmapLayer.Create(FImgView.Layers);
  FBackgroundLayer.Location := FloatRect(0, 0, FImgView.Bitmap.Width-0, FImgView.Bitmap.Height-0);
  FBackgroundLayer.Bitmap.SetSize(FImgView.Bitmap.Width, FImgView.Bitmap.Height);
  FBackgroundLayer.Bitmap.Clear(clWhite32);
  FBackgroundLayer.Scaled := True;

  FFreeDrawLayer := TFreeDrawLayer.Create(FImgView.Layers);
  FFreeDrawLayer.Location := FloatRect(0, 0, FImgView.Bitmap.Width, FImgView.Bitmap.Height);
//  FFreeDrawLayer.Location := FloatRect(0, 0, 300, 300);
end;

procedure TThCustomCanvas.DoScaleChage(Scale: Single);
begin
  if Assigned(FOnScaleChange) then
    FOnScaleChange(Self, Scale);
end;

function TThCustomCanvas.GetPenDrawMode: TThFreeDrawMode;
begin
  Result := FFreeDrawLayer.DrawMode;
end;

function TThCustomCanvas.GetScale: Single;
begin
  Result := FImgView.Scale;
end;

procedure TThCustomCanvas.SetCanvasMode(const Value: TThCanvasMode);
begin
  FCanvasMode := Value;

  FFreeDrawLayer.SetCanvasMode(Value);
end;

procedure TThCustomCanvas.SetPenColor(const Value: TColor);
begin
  FPenColor := Value;

  FFreeDrawLayer.PenColor := Color32(FPenColor);
end;

procedure TThCustomCanvas.SetPenDrawMode(const Value: TThFreeDrawMode);
begin
  FFreeDrawLayer.DrawMode := Value;
end;

procedure TThCustomCanvas.SetPenOpacity(const Value: TThPercent);
begin
  FPenOpacity := Value;

  FFreeDrawLayer.PenAlpha := Round(FPenOpacity / 100 * 255);;
end;

procedure TThCustomCanvas.SetPenSize(const Value: Integer);
begin
  FPenSize := Value;
  FFreeDrawLayer.Thickness := FPenSize;
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

procedure TThCustomCanvas.Test;
begin
  FImgView.Bitmap.SaveToFile('D:\test.jpg');
end;

procedure TThCustomCanvas.ImgViewResize(Sender: TObjecT);
begin
//  FImgView.Bitmap.SetSize(Width, Height);
//  FImgView.Bitmap.Clear(clWhite32);
end;

procedure TThCustomCanvas.ImgViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FCanvasMode = cmSelection then
  begin
    FMouseDowned := True;

    FMouseDownPoint := Point(X, Y);

    Cursor := crDrag;
  end;
end;

procedure TThCustomCanvas.ImgViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
  if FCanvasMode = cmSelection then
  begin
//    FMouseDownPoint
    if FMouseDowned then
    begin
      Move(FMouseDownPoint.X - X, FMouseDownPoint.Y - Y);
      FMouseDownPoint := Point(X, Y);
    end;
  end;
end;

procedure TThCustomCanvas.ImgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FCanvasMode = cmSelection then
  begin
    FMouseDowned := False;
    Cursor := crDefault;
  end;
end;

procedure TThCustomCanvas.ImgViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FImgView.Scroll(0, -WheelDelta);
  FImgView.Update;
end;

procedure TThCustomCanvas.ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Scale := Scale + 0.1;
end;

procedure TThCustomCanvas.ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Scale := Scale - 0.1;
end;

procedure TThCustomCanvas.Move(AX, AY: Integer);
begin
  FImgView.Scroll(AX, AY);
end;

end.
