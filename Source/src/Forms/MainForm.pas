{

}

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ThCanvasEditor,
  FMX.Objects, FMX.ExtCtrls, ThothController, ThCanvasController, ThTypes,
  ThItemCommand, FMX.Ani;

type
  TForm1 = class(TForm, IThObserver)
    Panel1: TPanel;
    Button2: TButton;
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    btnRedo: TCornerButton;
    btnUndo: TCornerButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    FController: TThothController;
    FCanvas: TThCanvasEditor;
    FCanvasController: TThCanvasEditorController;

    // test
  FHorzTrackAni : TFloatAnimation;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ThItem;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvasEditor.Create(Self);
  FCanvas.Parent := Panel1;
  FCanvas.Align := TAlignLayout.alClient;

  FController := TThothController.Create;
  FController.RegistObserver(Self);

  FCanvasController := TThCanvasEditorController.Create;
  FCanvasController.SetSubject(FController);
  FCanvasController.SetThCanvas(FCanvas);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TForm1.Notifycation(ACommand: IThCommand);
begin
  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TForm1.SetSubject(ASubject: IThSubject);
begin
  FController.RegistObserver(Self);
end;

procedure TForm1.btnRedoClick(Sender: TObject);
begin
  FController.Redo;

  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TForm1.btnUndoClick(Sender: TObject);
begin
  FController.Undo;

  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCanvas.DrawItemID := TButton(Sender).Tag;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  I: Integer;
  Item: TThItem;
begin
  for I := 0 to 200 do
  begin
    Item := FCanvas.AddItem(1100);
    Item.Position.X := Random(Width);
    Item.Position.Y := Random(Height);
    Item.Height := 50 + Random(50);
    Item.Width := 50 + Random(50);

    FController.Subject(nil, TThCommandItemAdd.Create(FCanvas, Item));
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FCanvas.DeleteSelection;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FHorzTrackAni := TFloatAnimation.Create(Self);
  FHorzTrackAni.Parent := FCanvas;
  FHorzTrackAni.AnimationType := TAnimationType.atOut;
  FHorzTrackAni.Interpolation := TInterpolationType.itQuadratic;
  FHorzTrackAni.PropertyName := 'ViewPortPosition.X';
  FHorzTrackAni.StartFromCurrent := True;
  FHorzTrackAni.Delay := 0;
  FHorzTrackAni.Duration := 5;

  FHorzTrackAni.StartValue := 0;
  FHorzTrackAni.StopValue := 50;
  FHorzTrackAni.Start;

end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  FCanvas.ViewPortPosition.X := FCanvas.ViewPortPosition.X + 10;
end;

end.
