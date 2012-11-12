unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani,
  FMX.Menus;

type
  TZoomAni = class(TControl)
  private
    FZoomAni: TFloatAnimation;
    FDiffuse: Single;

    procedure SetDiffuse(const Value: Single);
    procedure FinishAni(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ZoomIn(ACenterPos: TPointF);
    procedure ZoomOut(ACenterPos: TPointF);

    property Diffuse: Single read FDiffuse write SetDiffuse;
  end;

  TZoomCircleAni = class(TZoomAni)
  protected
    procedure Paint; override;
  end;

  TForm1 = class(TForm)
    FloatAnimation1: TFloatAnimation;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    StyleBook2: TStyleBook;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
    FTest: TZoomAni;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.UIConsts, Unit2;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FTest.ZoomIn(PointF(200, 200));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FTest.ZoomOut(PointF(100, 100));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTest := TZoomCircleAni.Create(Self);
  FTest.Parent := Panel1;
  FTest.Width := 100;
  FTest.Height := 100;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTest.Free;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

{ TTest }

constructor TZoomAni.Create(AOwner: TComponent);
begin
  inherited;

  FZoomAni := TFloatAnimation.Create(Self);
  FZoomAni.Parent := Self;
  FZoomAni.AnimationType := TAnimationType.atOut;
  FZoomAni.Interpolation := TInterpolationType.itQuadratic;
  FZoomAni.PropertyName := 'Diffuse';
  FZoomAni.StartFromCurrent := False;
  FZoomAni.Delay := 0;
  FZoomAni.Duration := 0.3;
  FZoomAni.OnFinish := FinishAni;

  FWidth := 100;
  FHeight := 100;
end;

destructor TZoomAni.Destroy;
begin
  FZoomAni.Free;

  inherited;
end;

procedure TZoomAni.FinishAni(Sender: TObject);
begin
  Visible := False;
end;

procedure TZoomAni.Paint;
begin
  inherited;

end;

procedure TZoomAni.SetDiffuse(const Value: Single);
begin
  FDiffuse := Value;

  Repaint;
end;

procedure TZoomAni.ZoomIn(ACenterPos: TPointF);
begin
  Position.Point := ACenterPos.Subtract(PointF(Width / 2, Height / 2));

  Visible := True;
  FZoomAni.StartValue := 100;
  FZoomAni.StopValue := 10;

  if FZoomAni.Running then
    FZoomAni.Stop;
  FZoomAni.Start;
end;

procedure TZoomAni.ZoomOut(ACenterPos: TPointF);
begin
  Visible := True;
  FZoomAni.StartValue := 10;
  FZoomAni.StopValue := 100;

  if FZoomAni.Running then
    FZoomAni.Stop;
  FZoomAni.Start;
end;

{ TZoomCircleAni }

procedure TZoomCircleAni.Paint;
var
  Radius: Single;
  R, R2: TRectF;
  P: TPointF;
begin
  inherited;

  Radius := (Width / 2) * (FDiffuse / 100);
  P := ClipRect.CenterPoint;
  R := RectF(P.X, P.Y, P.X, P.Y);
  R2 := RectF(P.X, P.Y, P.X, P.Y);

  InflateRect(R, Radius, Radius);

  if Radius > (Width / 4) then
  begin
    InflateRect(R2, Radius / 2, Radius / 2);
  end;

  Canvas.StrokeThickness := 3;
  Canvas.StrokeDash := TStrokeDash.sdDot;
  Canvas.Stroke.Color := claDarkGray;
  Canvas.DrawEllipse(R, 1);
  Canvas.DrawEllipse(R2, 1);
end;

end.
