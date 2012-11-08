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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnDeleteSelectionClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
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

procedure TForm1.btnRectangleClick(Sender: TObject);
begin
  FCanvas.DrawItemID := TButton(Sender).Tag;
end;

procedure TForm1.btnDeleteSelectionClick(Sender: TObject);
begin
  FCanvas.DeleteSelection;
end;

end.
