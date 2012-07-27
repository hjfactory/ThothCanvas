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
  Unit5, Unit1;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    StyleBook1: TStyleBook;
    Rectangle1: TRectangle;
    Panel2: TPanel;
    Button1: TButton;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Rectangle6: TRectangle;
    Rectangle7: TRectangle;
    Rectangle8: TRectangle;
    Rectangle9: TRectangle;
    Rectangle10: TRectangle;
    procedure ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button1Click(Sender: TObject);
    procedure ScrollBox1Painting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    { Private declarations }
    FOffSet, FPos: TPointF;
    FDown: Boolean;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

//uses
//  WinAPI.Windows;

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  Form1.Show;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FOffSet := PointF(0, 0);

//  ScrollBox1.HScrollBar.Value := 10000;
end;

procedure TForm4.ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  // 바닥선택(객체 선택안함)
  if Button = TMouseButton.mbLeft then
  begin
    FPos := PointF(X, Y);
    FDown := True;
  end;
end;

procedure TForm4.ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  I: Integer;
  MovePos: TPointF;
  s: TShape;
begin
//  Exit;
  if FDown and (ssLeft in Shift) then
  begin
//    OutputDebugString(PChar(Format('FPos: %f, %f', [FPos.X, FPos.Y])));
//    OutputDebugString(PChar(Format('X/Y: %f, %f', [X, Y])));
    MovePos.SetLocation(X-FPos.X, Y-FPos.Y);
    FOffSet.Offset(MovePos);
    FPos := PointF(X, Y);
//    OutputDebugString(PChar(Format('OffSet: %f, %f', [FOffSet.X, FOffSet.Y])));

//    OutputDebugString(PChar(Format('B: %f, %f', [Rectangle1.Position.X, Rectangle1.Position.Y])));

//    Rectangle1.Position.Point.Offset(FOffSet);
    for I := 0 to ScrollBox1.Children[1].ChildrenCount - 1 do
    begin
      if ScrollBox1.Children[1].Children[I] is TShape then
      begin

        s := TShape(ScrollBox1.Children[1].Children[I]);
        s.Position.X := s.Position.X + MovePos.X;
        s.Position.Y := s.Position.Y + MovePos.Y;
      end;
    end;

//    Rectangle1.Position.X := Rectangle1.Position.X + MovePos.X;
//    Rectangle1.Position.Y := Rectangle1.Position.Y + MovePos.Y;

//    OutputDebugString(PChar(Format('A: %f, %f', [Rectangle1.Position.X, Rectangle1.Position.Y])));
//    for I := 0 to ScrollBox1.ChildrenCount - 1 do
//    begin
//      TShape(ScrollBox1.Children[I]).Position.Point.Offset(FOffSet);
//    end;

    ScrollBox1.Repaint;
  end;
end;

procedure TForm4.ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  MovePos: TPointF;
begin
  Exit;
  if FDown then
  begin
//    OutputDebugString(PChar(Format('FPos: %f, %f', [FPos.X, FPos.Y])));
//    OutputDebugString(PChar(Format('X/Y: %f, %f', [X, Y])));
    MovePos.SetLocation(X-FPos.X, Y-FPos.Y);
    FOffSet.Offset(MovePos);
    FPos := PointF(X, Y);
//    OutputDebugString(PChar(Format('OffSet: %f, %f', [FOffSet.X, FOffSet.Y])));

//    OutputDebugString(PChar(Format('B: %f, %f', [Rectangle1.Position.X, Rectangle1.Position.Y])));

//    scrollBox1.

//    Rectangle1.Position.Point.Offset(FOffSet);
    Rectangle1.Position.X := Rectangle1.Position.X + MovePos.X;
    Rectangle1.Position.Y := Rectangle1.Position.Y + MovePos.Y;

//    OutputDebugString(PChar(Format('A: %f, %f', [Rectangle1.Position.X, Rectangle1.Position.Y])));
//    for I := 0 to ScrollBox1.ChildrenCount - 1 do
//    begin
//      TShape(ScrollBox1.Children[I]).Position.Point.Offset(FOffSet);
//    end;

    ScrollBox1.Repaint;
  end;
end;

procedure TForm4.ScrollBox1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  I: Integer;
  X, Y: single;
begin
  X := Trunc(FOffSet.X) mod 100;
  Y := Round(FOffSet.Y) mod 100;
Exit;
  for I := 0 to (Trunc(ScrollBox1.Width-1) div 100) do
    Canvas.DrawLine(PointF(X + 100 * I, 0), PointF(X + 100 * I, ScrollBox1.Height), 1);

  for I := 0 to (Trunc(ScrollBox1.Height-1) div 100) do
    Canvas.DrawLine(PointF(0, Y + 100 * I), PointF(ScrollBox1.Width, Y + 100 * I), 1);
end;

procedure TForm4.ScrollBox1Painting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  I: Integer;
  X, Y: single;
begin
  X := Trunc(FOffSet.X) mod 100;
  Y := Round(FOffSet.Y) mod 100;

  for I := 0 to (Trunc(ScrollBox1.Width-1) div 100) do
    Canvas.DrawLine(PointF(X + 100 * I, 0), PointF(X + 100 * I, ScrollBox1.Height), 1);

  for I := 0 to (Trunc(ScrollBox1.Height-1) div 100) do
    Canvas.DrawLine(PointF(0, Y + 100 * I), PointF(ScrollBox1.Width, Y + 100 * I), 1);
end;

end.
