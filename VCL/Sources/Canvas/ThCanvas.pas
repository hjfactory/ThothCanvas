{
  Role
    Interaction
    Layers management
      Freedraw
      Shapes
      Viewer(Display received datas)
}

unit ThCanvas;

interface

uses
  System.Classes, System.Types,
  Vcl.Controls, Vcl.Graphics,

  GR32,
  GR32_Image,
  GR32_Layers,

  ThTypes,
  ThItemStyle,
  ThCanvasLayers;

type
  TScaleChangeEvent = procedure(Sender: TObject; Scale: Single) of object;

  TThCustomCanvas = class(TCustomControl)
  private
    FImgView: TImgView32;

    FPenLayer: TPenDrawLayer;
    FShapeLayer: TShapeDrawLayer;
    FBackgroundLayer: TThBackgroundLayer;

    FPenStyle: TThPenStyle;

    FOnScaleChange: TScaleChangeEvent;

    FDrawMode: TThDrawMode;
    FShapeId: string;

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

    procedure SetDrawMode(const Value: TThDrawMode);

    procedure SetShapeId(const Value: string);
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreatePage(AWidth: Integer = 0; AHeight: Integer = 0);
    property DrawMode: TThDrawMode read FDrawMode write SetDrawMode;

    // Link PenStyleChange
    property PenStyle: TThPenStyle read FPenStyle;
    property ShapeId: string read FShapeId write SetShapeId;

    property Scale: Single read GetScale write SetScale;
    property OnScaleChange: TScaleChangeEvent read FOnScaleChange write FOnScaleChange;

    procedure Clear;
    procedure DeleteSelected;
  end;

  TThCanvas = class(TThCustomCanvas)
  public
    property OnScaleChange;
  end;

implementation

{ TThCanvas }

procedure TThCustomCanvas.Clear;
begin
  FPenLayer.Clear;
end;

constructor TThCustomCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImgView := TImgView32.Create(nil);
  FImgView.Name := 'ThImgView';
  FImgView.Parent := Self;
  FImgView.Align := alClient;

//  FImgView.Cursor := crNone;

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
end;

procedure TThCustomCanvas.DeleteSelected;
begin
  FShapeLayer.DeleteSelectedItems;
end;

destructor TThCustomCanvas.Destroy;
begin
  FImgView.Free;

  inherited;
end;

procedure TThCustomCanvas.CreatePage(AWidth, AHeight: Integer);
begin
  if (AWidth = 0) or (AWidth = 0) then
  begin
    AWidth := 2480;
    AHeight := 3508;
  end;

  FImgView.Bitmap.SetSize(AWidth, AHeight);
  FImgView.Bitmap.Clear(clGray32);

  FBackgroundLayer := TThBackgroundLayer.Create(FImgView.Layers);
  FBackgroundLayer.Location := FloatRect(0, 0, AWidth, AHeight);
  FBackgroundLayer.Bitmap.SetSize(AWidth, AHeight);
  FBackgroundLayer.Bitmap.Clear(clWhite32);
  FBackgroundLayer.Scaled := True;

  FShapeLayer := TShapeDrawLayer.Create(FImgView.Layers);
  FShapeLayer.Location := FloatRect(0, 0, AWidth, AHeight);
  FShapeLayer.HitTest := False;
  FShapeLayer.Scaled := True;
  FShapeLayer.MouseEvents := True;

  FPenLayer := TPenDrawLayer.Create(FImgView.Layers);
  FPenLayer.Location := FloatRect(0, 0, AWidth, AHeight);
  FPenLayer.Scaled := True;
  FPenLayer.HitTest := True;

  FImgView.Layers.MouseListener := FPenLayer;

  FPenStyle := FPenLayer.PenDrawObj.DrawStyle as TThPenStyle;
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

procedure TThCustomCanvas.SetShapeId(const Value: string);
begin
  FShapeId := Value;

  FShapeLayer.ShapeId := Value;
end;

procedure TThCustomCanvas.SetDrawMode(const Value: TThDrawMode);
begin
  case Value of
    dmSelect, dmDraw:
      begin
        FPenLayer.HitTest  := False;
        FShapeLayer.HitTest := True;
        FShapeLayer.DrawMode := Value;

        FImgView.Layers.MouseListener := FShapeLayer;
      end;
    dmPen, dmEraser:
      begin
        FPenLayer.HitTest  := True;
        FShapeLayer.HitTest := False;
        FPenLayer.DrawMode := Value;

        FImgView.Layers.MouseListener := FPenLayer;
      end;
  end;
  FDrawMode := Value;
end;

procedure TThCustomCanvas.SetScale(const Value: Single);
var
  LScale: Single;
begin
  if      Value < TH_SCALE_MIN then LScale := TH_SCALE_MIN
  else if Value > TH_SCALE_MAX then LScale := TH_SCALE_MAX
  else                              LScale := Value;

  if GetScale = LScale then
    Exit;

  FImgView.Scale := LScale;
  FImgView.Invalidate;

//  FShapeDrawLayer.Scale := FloatPoint(LScale, LScale);
  DoScaleChage(LScale);
end;

procedure TThCustomCanvas.ImgViewResize(Sender: TObjecT);
begin

end;

procedure TThCustomCanvas.Loaded;
begin
  inherited;

  if HandleAllocated then
    FImgView.ScrollBars.Visibility := svAuto;
end;

procedure TThCustomCanvas.ImgViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin

end;

procedure TThCustomCanvas.ImgViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin

end;

procedure TThCustomCanvas.ImgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin

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
//  if ssCtrl in Shift then
    Scale := Scale + 0.1;
end;

procedure TThCustomCanvas.ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
//  if ssCtrl in Shift then
    Scale := Scale - 0.1;
end;

end.
