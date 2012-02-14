unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo;

type
  TThShape = class(TShape)
  private
    FThCanvas: TScrollBox;
    //
  public
    constructor Create(AOwner: TComponent); override;

    property ThCanvas: TScrollBox read FThCanvas;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

  TThLine = class(TThShape)
    // implement
      // Selection point
      //
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FPosition: TPosition;
    FDraw: Boolean;

    procedure LineMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure LineMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TThLine }

constructor TThLine.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TThLine.Paint;
begin
  Canvas.DrawLine(GetShapeRect.TopLeft, GetShapeRect.BottomRight,
    AbsoluteOpacity);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
//  Text1.Text := 'asdfasdfasfd';
//  fsPascal1.
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  FDraw := TCheckBox(Sender).IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  FPosition := TPosition.Create(PointF(0, 0));
  FDraw := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FPosition) then
    FPosition.Free;
end;

procedure TForm1.LineMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  P := TLine(Sender).LocalToAbsolute(PointF(X, Y));

  Memo1.Lines.Add(Format('Line Down: %f, %f, %f, %f', [X, Y, P.X, P.Y]));

//  TLine(Sender).Parent.do
//TLine(Sender).Parent)
//  ScrollBox1MouseDown(TLine(Sender).Parent, Button, Shift, P.X, P.Y);
//  TThShape(Sender).ThCanvas.MouseDown(Button, Shift, P.X, P.Y);
end;

procedure TForm1.LineMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  P := TLine(Sender).LocalToAbsolute(PointF(X, Y));

  Memo1.Lines.Add(Format('Line Up: %f, %f, %f, %f', [X, Y, P.X, P.Y]));

//  TLine(Sender).Parent.do
//  TThShape(Sender).ThCanvas.MouseUp(Button, Shift, P.X, P.Y);
//  ScrollBox1MouseUp(TLine(Sender).Parent, Button, Shift, P.X, P.Y);
end;

procedure TForm1.ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
//  Local

  Memo1.Lines.Add(Format('ScrollBox Down: %f, %f', [X, Y]));

  if FDraw then
  begin
    if Assigned(FPosition) then
    begin
      FPosition.Free;
    end;

    FPosition := TPosition.Create(PointF(X, Y));
    FPosition.X := X;
    FPosition.Y := y;
  end;
end;

procedure TForm1.ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Line: TThLine;
begin
  if FDraw then
  begin
    if not Assigned(FPosition) then
      Exit;

    Line := TThLine.Create(ScrollBox1);
    Line.Position.X := FPosition.X;
    Line.OnMouseDown := LineMouseDown;
    Line.OnMouseUp := LineMouseUp;
    Line.Position.Y := FPosition.Y;
    Line.Width := X - FPosition.X;
    Line.Height := Y - FPosition.Y;
    Line.Parent := ScrollBox1;

    FPosition.Free;
    FPosition := nil;
  end;

  Memo1.Lines.Add(Format('Up: %f, %f', [X, Y]));
end;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FThCanvas := TScrollBox(AOwner);
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToAbsolute(PointF(X, Y));
  FThCanvas.MouseDown(Button, Shift, P.X, P.Y);
end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToAbsolute(PointF(X, Y));
  FThCanvas.MouseUp(Button, Shift, P.X, P.Y);
end;

end.
