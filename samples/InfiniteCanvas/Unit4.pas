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
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThothCanvas;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  WinAPI.Windows;

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

  ScrollBox1.Repaint;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  OutputDebugString(PChar(Format('%f %f', [FCanvas.HScrollBar.Value, FCanvas.VScrollBar.Value])));
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
  FCanvas.Center;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ScrollBox1.HScrollBar.Value := 10000;

  FCanvas := TThothCanvas.Create(Panel3);
  FCanvas.Align := TAlignLayout.alClient;
  FCanvas.Parent := Panel3;

//  FCanvas.StyleName := 'TScrollBoxstyle';
  FCanvas.StyleLookup := 'ScrollBoxstyle';

  FCanvas.HScrollBar.Min := -1000;
  FCanvas.HScrollBar.Max := 1000;
  FCanvas.VScrollBar.Min := -1000;
  FCanvas.VScrollBar.Max := 1000;

  FCanvas.HScrollBar.Visible := True;
  FCanvas.VScrollBar.Visible := True;
  FCanvas.Realign;
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
end;

end.
