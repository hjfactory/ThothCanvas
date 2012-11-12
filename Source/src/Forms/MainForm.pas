{

}

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ThCanvasEditor,
  FMX.Objects, FMX.ExtCtrls, ThothController, ThCanvasController, ThTypes,
  FMX.Edit;

type
  TForm1 = class(TForm, IThObserver)
    Panel1: TPanel;
    btnRectangle: TButton;
    btnLine: TButton;
    btnCircle: TButton;
    btnSelect: TButton;
    btnRedo: TCornerButton;
    btnUndo: TCornerButton;
    btnDeleteSelection: TButton;
    btnZoomOut: TCornerButton;
    btnZoomIn: TCornerButton;
    edtZoom: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnDeleteSelectionClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FController: TThothController;
    FCanvas: TThCanvasEditor;
    FCanvasController: TThCanvasEditorController;

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

procedure TForm1.btnZoomInClick(Sender: TObject);
begin
  FCanvas.ZoomIn;
  edtZoom.Text := FloatToStr(FCanvas.ZoomScale);
end;

procedure TForm1.btnZoomOutClick(Sender: TObject);
begin
  FCanvas.ZoomOut;
  edtZoom.Text := FloatToStr(FCanvas.ZoomScale);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FCanvas.ZoomOutAtPoint(250,FCanvas.Height / 4);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCanvas.ZoomOutAtPoint(750,FCanvas.Height / 4 * 3);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Item: TThItem;
begin
  Item := FCanvas.AddItem(1100);
  Item.Position.Point := PointF(250, FCanvas.Height / 4);
  Item.Width := 100;
  Item.Height := 100;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FCanvas.ZoomInAtPoint(250,FCanvas.Height / 4);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FCanvas.ZoomInAtPoint(750,FCanvas.Height / 4 * 3);
end;

procedure TForm1.btnRectangleClick(Sender: TObject);
begin
  FCanvas.DrawItemID := TButton(Sender).Tag;
end;

procedure TForm1.btnDeleteSelectionClick(Sender: TObject);
begin
  FCanvas.DeleteSelection;
end;

end.
