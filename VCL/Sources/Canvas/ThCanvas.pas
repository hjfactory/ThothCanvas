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
  ThDrawStyle,
  ThCanvasLayers;

type
  TScaleChangeEvent = procedure(Sender: TObject; Scale: Single) of object;

  TThCustomCanvas = class(TCustomControl)
  private
    FImgView: TImgView32;

    FFreeDrawLayer: TFreeDrawLayer;
    FShapeDrawLayer: TShapeDrawLayer;
    FBackgroundLayer: TThBackgroundLayer;

    FCanvasMode: TThCanvasMode;

    FPenStyle: TThPenStyle;

    FOnScaleChange: TScaleChangeEvent;
    FFreeDrawMode: TThFreeDrawMode;
    FShapeDrawMode: TThShapeDrawMode;
    FDrawObjId: Integer;

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

    procedure SetCanvasMode(const Value: TThCanvasMode);
    procedure SetFreeDrawMode(const Value: TThFreeDrawMode);
    procedure SetShapeDrawMode(const Value: TThShapeDrawMode);

    procedure PenStyleChange(Sender: TObject);
    procedure SetDrawObjId(const Value: Integer);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreatePage(AWidth: Integer = 0; AHeight: Integer = 0);

    // FreeDraw, ShapeDraw
    property CanvasMode: TThCanvasMode read FCanvasMode write SetCanvasMode;
    // Pen, Eraser
    property FreeDrawMode: TThFreeDrawMode read FFreeDrawMode write SetFreeDrawMode;
    // Rect, Round, ...
    property ShapeDrawMode: TThShapeDrawMode read FShapeDrawMode write SetShapeDrawMode;

//    property DrawShapeId: Integer

    // Link PenStyleChange
    property PenStyle: TThPenStyle read FPenStyle;
    property DrawObjId: Integer read FDrawObjId write SetDrawObjId;

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

uses ThDrawObjectManager;

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

  FPenStyle := TThPenStyle.Create;
  FPenStyle.OnChange := PenStyleChange;
end;

procedure TThCustomCanvas.CreateWnd;
begin
  inherited;

  FImgView.ScrollBars.Visibility := svAuto;
end;

procedure TThCustomCanvas.DeleteSelected;
begin
  FShapeDrawLayer.DeleteSelectedItems;
end;

destructor TThCustomCanvas.Destroy;
begin
  FPenStyle.Free;
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

//  FImgView.Bitmap.SetSize(AWidth, AHeight);
  FImgView.Bitmap.Clear(clGray32);

  FBackgroundLayer := TThBackgroundLayer.Create(FImgView.Layers);
  FBackgroundLayer.Location := FloatRect(0, 0, AWidth, AHeight);
  FBackgroundLayer.Bitmap.SetSize(AWidth, AHeight);
  FBackgroundLayer.Bitmap.Clear(clWhite32);
  FBackgroundLayer.Scaled := True;

  FShapeDrawLayer := TShapeDrawLayer.Create(FImgView.Layers);
  FShapeDrawLayer.HitTest := False;
  FShapeDrawLayer.Location := FloatRect(0, 0, AWidth, AHeight);

  FFreeDrawLayer := TFreeDrawLayer.Create(FImgView.Layers);
  FFreeDrawLayer.HitTest := True;
//  FFreeDrawLayer.Location := FloatRect(0, 0, AWidth-2000, AHeight-2000);
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

procedure TThCustomCanvas.SetCanvasMode(const Value: TThCanvasMode);
begin
  if FCanvasMode = Value then
    Exit;

  FCanvasMode := Value;

  FFreeDrawLayer.HitTest  := Value = TThCanvasMode.cmFreeDraw;
  FShapeDrawLayer.HitTest := Value = TThCanvasMode.cmShapeDraw;
end;

procedure TThCustomCanvas.SetDrawObjId(const Value: Integer);
begin
  FDrawObjId := Value;

  FShapeDrawLayer.DrawObjectId := Value;
end;

procedure TThCustomCanvas.SetFreeDrawMode(const Value: TThFreeDrawMode);
begin
  SetCanvasMode(cmFreeDraw);
  FFreeDrawMode := Value;

  FFreeDrawLayer.DrawMode := Value;
end;

procedure TThCustomCanvas.SetShapeDrawMode(const Value: TThShapeDrawMode);
begin
  SetCanvasMode(cmShapeDraw);
  FShapeDrawMode := Value;

  FShapeDrawLayer.DrawMode := Value;
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
  DoScaleChage(LScale);
end;

procedure TThCustomCanvas.ImgViewResize(Sender: TObjecT);
begin
//  FImgView.Bitmap.SetSize(Width, Height);
//  FImgView.Bitmap.Clear(clWhite32);
end;

procedure TThCustomCanvas.ImgViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
//  if FCanvasMode = cmSelection then
//  begin
//    FMouseDowned := True;
//
//    FMouseDownPoint := GR32.Point(X, Y);
//
//    Cursor := crDrag;
//  end;
end;

procedure TThCustomCanvas.ImgViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
//  if FCanvasMode = cmSelection then
//  begin
////    FMouseDownPoint
//    if FMouseDowned then
//    begin
//      Move(FMouseDownPoint.X - X, FMouseDownPoint.Y - Y);
//      FMouseDownPoint := GR32.Point(X, Y);
//    end;
//  end;
end;

procedure TThCustomCanvas.ImgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
//  if FCanvasMode = cmSelection then
//  begin
//    FMouseDowned := False;
//    Cursor := crDefault;
//  end;
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

procedure TThCustomCanvas.PenStyleChange(Sender: TObject);
begin
  DOMgr.PenDrawObj.DrawStyle := FPenStyle;
//  FFreeDrawLayer.DrawStyle := FPenStyle;
end;

end.
