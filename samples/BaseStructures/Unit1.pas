{
 이 샘플은 TShape를 이용해서 Draw하는 test coding이다.
 향후 Canvas에 직접 그리는 방식으로 검토 필요
}
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, Winapi.Windows;

type
  TThCommand = class;
  IThCommand = interface
  end;

  IThObserver = interface;
  IThSubject = interface
    procedure Report(ACommand: IThCommand);
    procedure AddObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: TThCommand);
  end;

///////////////////////////////////////////////////////
// ObjectList
  public
  TThObjectList = class(TInterfacedObject, IThObserver)
  private
    procedure Notifycation(ACommand: TThCommand);
  end;

///////////////////////////////////////////////////////
// Command List
  TCommandList = class

  end;

///////////////////////////////////////////////////////
// Manager
  TThothManager = class(TInterfacedObject, IThSubject)
    procedure Report(ACommand: IThCommand);
    procedure AddObserver(AObserver: IThObserver);
  end;

///////////////////////////////////////////////////////
// Canvas
  TThCanvas = class(TFramedScrollBox, IThObserver)
  public
    procedure Notifycation(ACommand: TThCommand); virtual;
  end;

///////////////////////////////////////////////////////
// Shape
  TThShape = class;
  TThShapeClass = class of TThShape;

  TThShape = class(TShape)
  private
    FThCanvas: TThCanvas;
    //
    function LocalToParent(P: TPointF): TPointF;
  public
    constructor Create(AOwner: TComponent); override;

    property ThCanvas: TThCanvas read FThCanvas;

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

///////////////////////////////////////////////////////
// Command
  TThCommand = class(TInterfacedObject, IThCommand)
  protected
    procedure Execute; virtual; abstract;
    procedure Rollback; virtual; abstract;
  end;

  TThShapeCommand = class(TThCommand)
  public
//    procedure Execute; override;
//    procedure Rollback; override;
  end;

  TThInsertShapeCommand = class(TThShapeCommand)

  end;

  TThDeleteShapeCommand = class(TThShapeCommand)

  end;

  TThMoveShapeCommand = class(TThShapeCommand)

  end;

  TThResizeShapeCommand = class(TThShapeCommand)

  end;

  TThStyleCommand = class(TThCommand)

  end;

///////////////////////////////////////////////////////
// Main form
  TForm1 = class(TForm)
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Path1: TPath;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;
    FPosition: TPosition;
    FDraw: Boolean;
    FLine: TThLine;

    procedure ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);

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

procedure TForm1.Button2Click(Sender: TObject);
var
  Bmp: TBitmap;
begin
//  ScrollBox1.SaveToStream();
//  Image1.Position.X := ScrollBox1.Position.X;
//  Image1.Position.Y := ScrollBox1.Position.Y;
//  Image1.Width := SCrollBox1.Width;
//  Image1.Height := SCrollBox1.Height;

//  Bmp.Canvas.
//  Image1.Bitmap.Canvas.

//  Image1.
//  Image1.Canvas.BeginScene;
//  ScrollBox1.PaintTo(Image1.Bitmap.Canvas, RectF(0, 0, 100, 100));
//  Image1.PaintTo(ScrollBox1.Canvas, RectF(0, 0, 100, 100));
//  Image1.Bitmap.Canvas.d
//  Image1.Canvas.EndScene;
//  Image1.Bitmap.BitmapChanged;


end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  FDraw := TCheckBox(Sender).IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  FPosition := TPosition.Create(PointF(0, 0));
  FDraw := True;

//  Image1.Bitmap.LoadFromFile('F:\Temp\새 폴더\test.bmp');
  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Parent := Self;
  FThCanvas.Position.Point := PointF(10, 40);
  FThCanvas.Width := 700;
  FThCanvas.Height := 580;
  FThCanvas.OnMouseDown := ScrollBox1MouseDown;
  FThCanvas.OnMouseUp := ScrollBox1MouseUp;
  FThCanvas.OnMouseMove := ScrollBox1MouseMove;
//  FThCanvas.parent

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;

  if Assigned(FPosition) then
    FPosition.Free;
end;

procedure TForm1.LineMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  P := TThLine(Sender).LocalToParent(PointF(X, Y));

  Memo1.Lines.Add(Format('Line Down: X:%f, Y:%f, %f, %f', [X, Y, P.X, P.Y]));

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
  P := TThLine(Sender).LocalToParent(PointF(X, Y));

  Memo1.Lines.Add(Format('Line Up: X:%f, Y:%f, %f, %f', [X, Y, P.X, P.Y]));

//  TLine(Sender).Parent.do
//  TThShape(Sender).ThCanvas.MouseUp(Button, Shift, P.X, P.Y);
//  ScrollBox1MouseUp(TLine(Sender).Parent, Button, Shift, P.X, P.Y);
end;

procedure TForm1.ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
//  Local
  Memo1.Lines.Add(Format('ScrollBox Down: X:%f, Y:%f', [X, Y]));

  if FDraw then
  begin
    if Assigned(FPosition) then
    begin
      FPosition.Free;
    end;

    FPosition := TPosition.Create(PointF(X, Y));
    FPosition.X := X;
    FPosition.Y := y;

    FLine := TThLine.Create(FThCanvas);
    FLine.Position.X := FPosition.X;
    FLine.Position.Y := FPosition.Y;
    FLine.OnMouseDown := LineMouseDown;
    FLine.OnMouseUp := LineMouseUp;
  end;
end;

procedure TForm1.ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if not Assigned(FPosition) then
    Exit;


  FLine.Width := X - FPosition.X;
  FLine.Height := Y - FPosition.Y;
  FLine.Parent := FThCanvas;
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

    FLine.Width := X - FPosition.X;
    FLine.Height := Y - FPosition.Y;
    FLine.Parent := FThCanvas;

    FPosition.Free;
    FPosition := nil;
  end;

  Memo1.Lines.Add(Format('Up: X:%f, Y:%f', [X, Y]));
end;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  FThCanvas := TThCanvas(AOwner);
end;

function TThShape.LocalToParent(P: TPointF): TPointF;
begin
//  FOrm1.Memo1.Lines.Add(Format('LocalToParent', []));
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(Format('LocalToParent', [])));
{$ENDIF}

  Result.X := Position.X + P.X;
  Result.Y := Position.Y + P.Y;
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToParent(PointF(X, Y));
  FThCanvas.MouseDown(Button, Shift, P.X, P.Y);
end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  P := LocalToParent(PointF(X, Y));
  FThCanvas.MouseUp(Button, Shift, P.X, P.Y);
end;

{ TThCanvas }

procedure TThCanvas.Notifycation;
begin
  inherited;

end;

{ TThObjectList }

procedure TThObjectList.Notifycation(ACommand: TThCommand);
begin

end;

{ TThothManager }

procedure TThothManager.AddObserver(AObserver: IThObserver);
begin

end;

procedure TThothManager.Report(ACommand: IThCommand);
begin
  OutputDebugString(PChar('test'));
end;

end.
