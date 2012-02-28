{
  초기에 ScrollBox의 H(V)Scollbar의 Min/Max를 일정하게 둔다
  캔버스 이동 시 Min / Max가 가까워 지면 증가한다

이슈
  대상이 그려지지 않으면 MouseTracking이 되지 않음
  (스크롤이 생기지 않음)
}

unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.Objects,
  Unit5;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ScrollBox1: TScrollBox;
    Panel3: TPanel;
    Button4: TButton;
    StyleBook1: TStyleBook;
    procedure ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThothCanvas;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  with TRectangle.Create(ScrollBox1) do
  begin
    Parent := ScrollBox1;
    Width := 100;
    height := 100;
    Position.Point := PointF(10, 10);
  end;
//
//  with TRectangle.Create(ScrollBox1) do
//  begin
//    Parent := ScrollBox1;
//    Width := 100;
//    height := 100;
//    Position.Point := PointF(-50, 50);
//  end;

  ScrollBox1.Repaint;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin

  ScrollBox1.Repaint;

//  ScrollBox1.InViewRect(RectF(990, 990, 1000, 1000));
end;

procedure TForm4.Button3Click(Sender: TObject);
begin

//Memo1.Lines.Add(Format('%f, %f', [
//  ScrollBox1.HScrollBar.ViewportSize,
//  ScrollBox1.VScrollBar.ViewportSize]));
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  with TRectangle.Create(FCanvas) do
  begin
    Parent := FCanvas;
    Width := 100;
    height := 100;
    Position.Point := PointF(10, 10);
  end;

  FCanvas.Realign;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
//  ScrollBox1.con

//  ScrollBox1.MouseTracking := True;
  ScrollBox1.HScrollBar.Value := 10000;
//
//  ScrollBox1.HScrollBar.Min := -1000;
//  ScrollBox1.VScrollBar.Min := -1000;
//
//  ScrollBox1.HScrollBar.Max := 1000;
//  ScrollBox1.VScrollBar.Max := 1000;

//  ScrollBox1.AutoHide := FAlse;

//  ScrollBox1.ShowSizeGrip := True;

  FCanvas := TThothCanvas.Create(Panel3);
  FCanvas.Align := TAlignLayout.alClient;
  FCanvas.Parent := Panel3;

//  FCanvas.StyleName := 'TScrollBoxstyle';
  FCanvas.StyleLookup := 'ScrollBoxstyle';

  FCanvas.HScrollBar.Visible := True;
end;

procedure TForm4.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
//  Memo1.Lines.Add(Format('%d', [WheelDelta]));
end;

procedure TForm4.ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  I: Integer;
begin
  for I := 0 to (Trunc(ScrollBox1.Width-1) div 100) do
    Canvas.DrawLine(PointF(100 * I, 0), PointF(100 * I, ScrollBox1.Height), 1);

  for I := 0 to (Trunc(ScrollBox1.Height-1) div 100) do
    Canvas.DrawLine(PointF(0, 100 * I), PointF(ScrollBox1.Width, 100 * I), 1);

//  Memo1.Lines.Add(Format('%d, %d', [Canvas.Width, Canvas.Height]));
//  Memo1.Lines.Add(Format('%f, %f', [ScrollBox1.HScrollBar.Value, ScrollBox1.VScrollBar.Value]));
end;

end.
