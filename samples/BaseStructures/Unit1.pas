{
 이 샘플은 TShape를 이용해서 Draw하는 test coding이다.
 향후 Canvas에 직접 그리는 방식으로 검토 필요
}
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, ThothCanvas, ThothObjects, ObjectManager, ThothTypes;

type
///////////////////////////////////////////////////////
// Main form
  TForm1 = class(TForm, IThObserver)  // 걍 여기다가 함
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button7: TButton;
    Panel1: TPanel;
    Button8: TButton;
    Panel2: TPanel;
    Button9: TButton;
    btnUndo: TButton;
    btnRedo: TButton;
    trbScale: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    Button10: TButton;
    Button11: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure trbScaleChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
    FObjectManager: TThothObjectManager;
    FThCanvas: TThCanvas;
    FThCanvas2: TThCanvas;

    FTest: TRectangle;

    procedure DoScale(Value: Single);
  public
    { Public declarations }
    procedure Notifycation(ACommand: IThCommand); virtual;
    procedure SetSubject(ASubject: IThSubject);
  end;

var
  Form1: TForm1;

implementation

uses
  WinAPI.Windows, ThothCommands;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FObjectManager := TThothObjectManager.Create;
  SetSubject(FObjectManager);
//  FObjectManager.RegistObserver(Self);

  FThCanvas := TThCanvas.Create(Panel2);
  FThCanvas.SetSubject(FObjectManager);
  FThCanvas.Parent := Panel2;
//  FThCanvas.Position.Point := PointF(10, 40);
  FThCanvas.Align := TAlignLayout.alClient;
//  FThCanvas.Width := 530;
//  FThCanvas.Height := 450;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FObjectManager.Free;
  FThCanvas.Free;
end;

procedure TForm1.Notifycation(ACommand: IThCommand);
begin
  OutputDebugSTring(PChar('TForm1 - ' + TThShapeCommand(ACommand).ClassName));

  btnUndo.Enabled := FObjectManager.CommandList.HasUndo;
  btnRedo.Enabled := FObjectManager.CommandList.HasRedo;
end;

procedure TForm1.SetSubject(ASubject: IThSubject);
begin
  ASubject.RegistObserver(Self);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  FThCanvas.SelectionRoate(TTrackBar(Sender).Value);
end;

procedure TForm1.trbScaleChange(Sender: TObject);
begin
//  FThCanvas.SelectionScale(TTrackBar(Sender).Value);
  FThCanvas.Scale.X := TTrackBar(Sender).Value;
  FThCanvas.Scale.Y := TTrackBar(Sender).Value;
end;

procedure TForm1.btnRedoClick(Sender: TObject);
begin
  FObjectManager.Redo;
end;

procedure TForm1.btnUndoClick(Sender: TObject);
begin
  FObjectManager.Undo;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  trbScale.Value := 1;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  TrackBar1.Value := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FThCanvas.DrawClass := nil;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  a: WORD;
begin
//  FTest := TRectangle.Create(Self);
//
//  with FTest do
//  begin
//    Position.Point := PointF(100, 100);
//    Width := 100;
//    Height:= 100;
//    Parent := Self;
//  end;
//
  FObjectManager.test;

//  FThCanvas.Centre;
//  ShowMessage(FloatToStr(FThCanvas.VScrollBar.Value));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThLine;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  P: TPointF;
begin
  Randomize;
  FThCanvas.BeginUpdate;
  for I := 0 to 100 do
  begin
    P := PointF(Random(Trunc(FThCanvas.Width - 200)), Random(Trunc(FThCanvas.Height - 200)));

    FThCanvas.DrawShape(TThRectangle,
      P,
      PointF(P.X + Random(150) + 30, P.Y + Random(150) + 30)
    );
  end;
  FThCanvas.EndUpdate;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThRectangle;
  if Assigned(FThCanvas2) then
    FThCanvas2.DrawClass := TThRectangle;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FThCanvas.DrawClass := TThCircle;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FThCanvas2 := TThCanvas.Create(nil);
  FThCanvas2.SetSubject(FObjectManager);
  FThCanvas2.Parent := FThCanvas;
//  FThCanvas2.Align := TAlignLayout.alClient;
  FThCanvas2.Position.Point := PointF(100, 100);
  FThCanvas2.Width := 530;
  FThCanvas2.Height := 450;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  FTest.Free;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  FThCanvas.DeleteSelectedShapes;
end;

procedure TForm1.DoScale(Value: Single);
begin
end;

end.
