unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects;

type
  TPieControlPanel = class(TControl)
  private
    FGapSize: Single;
    FMouseOutColor: TAlphaColor;
    FMouseOverColor: TAlphaColor;
    FStrokeThickness: Single;
    FFill: TBrush;
    FStroke: TBrush;
    FOverFill1: TBrush;
    procedure SetStrokeThickness(const Value: Single);
  protected
    function PointInObject(X, Y: Single): Boolean; override;
    procedure Paint; override;
    procedure Painting; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property MouseOverColor: TAlphaColor read FMouseOverColor write FMouseOverColor;
    property MouseOutColor: TAlphaColor read FMouseOutColor write FMouseOutColor;

    property Fill: TBrush read FFill;
    property OverFill: TBrush read FOverFill1 write FOverFill1;
    property Stroke: TBrush read FStroke;
    property StrokeThickness: Single read FStrokeThickness write SetStrokeThickness;
  end;

  TForm1 = class(TForm)
    Pie1: TPie;
    procedure Pie1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCP: TPieControlPanel;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCP := TPieControlPanel.Create(Self);
  FCP.Position.Point := PointF(100, 100);
  FCP.Width := 120;
  FCP.Height := 120;
  FCP.Parent := Self;
end;

procedure TForm1.Pie1Click(Sender: TObject);
begin
  ShowMessage(TControl(SEnder).Name);
end;

{ TPieControlPanel }

constructor TPieControlPanel.Create(Owner: TComponent);
begin
  inherited;

  FOverFill1 := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
  FOverFill1.Kind := TBrushKind.bkGradient;
  FOverFill1.Gradient.Color1 := $FF55FF55;
  FOverFill1.Gradient.Color := $FF99FF99;
  FOverFill1.Gradient.Style := TGradientStyle.gsRadial;
//  FOverFill1.Color := claGray;

  FFill := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
  FFIll.Kind := TBrushKind.bkGradient;
  FFill.Gradient.Color1 := $FF55FF55;
  FFill.Gradient.Color := $FF99FF99;
  FFill.Gradient.Style := TGradientStyle.gsRadial;
  FFill.Color := claGreen;

  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF000000);
  FStroke.Color := $FF000000;
  FStrokeThickness := 1;
end;

destructor TPieControlPanel.Destroy;
begin
  FStroke.Free;
  FOverFill1.Free;
  FFill.Free;

  inherited;
end;

procedure TPieControlPanel.Paint;
var
  P: TPathData;
begin
//  Canvas.Fill.Color := claGray;

  P := TPathData.Create;
  P.Clear;
  Canvas.Fill.Assign(FOverFill1);
  P.MoveTo(PointF(Width / 2 + 4, Height / 2 + 2));
  P.AddArc(PointF(Width / 2 + 4, Height / 2 + 2), PointF((Width - StrokeThickness) / 2, (Height - StrokeThickness) / 2), -45, 90);
  P.LineTo(PointF(Width / 2 + 4, Height / 2 + 2));
  P.ClosePath;
  Canvas.FillPath(P, AbsoluteOpacity);
  Canvas.DrawPath(P, AbsoluteOpacity);
  P.Free;

  P := TPathData.Create;
  Canvas.Fill.Assign(FFill);
  P.Clear;
  P.MoveTo(PointF(Width / 2 + 2, Height / 2 + 4));
  P.AddArc(PointF(Width / 2 + 2, Height / 2 + 4), PointF((Width - StrokeThickness) / 2, (Height - StrokeThickness) / 2), 45, 90);
  P.LineTo(PointF(Width / 2 + 2, Height / 2 + 4));

  P.MoveTo(PointF(Width / 2, Height / 2 + 2));
  P.AddArc(PointF(Width / 2, Height / 2 + 2), PointF((Width - StrokeThickness) / 2, (Height - StrokeThickness) / 2), 135, 90);
  P.LineTo(PointF(Width / 2, Height / 2 + 2));

  P.MoveTo(PointF(Width / 2 + 2, Height / 2));
  P.AddArc(PointF(Width / 2 + 2, Height / 2), PointF((Width - StrokeThickness) / 2, (Height - StrokeThickness) / 2), 225, 90);
  P.LineTo(PointF(Width / 2 + 2, Height / 2));
  P.ClosePath;
  Canvas.FillPath(P, AbsoluteOpacity);
  Canvas.DrawPath(P, AbsoluteOpacity);

  P.Free;
end;

procedure TPieControlPanel.Painting;
begin
  inherited;

//  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);
  Canvas.StrokeThickness := FStrokeThickness;
end;

function TPieControlPanel.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if Width * Height = 0 then
    Exit;
  if (Sqr((P.X * 2 - Width) / Width) + Sqr((P.Y * 2 - Height) / Height) <= 1)
  then
  begin
    Result := True;
  end;
end;

procedure TPieControlPanel.SetStrokeThickness(const Value: Single);
begin
  FStrokeThickness := Value;
end;

end.
