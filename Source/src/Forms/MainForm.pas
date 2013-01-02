{

}

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, ThCanvasEditor,
  FMX.Objects, FMX.ExtCtrls, ThothController, ThCanvasController, ThTypes,
  FMX.Edit;

type
  TfrmMainDraft = class(TForm, IThObserver)
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
    btnHome: TCornerButton;
    btnImage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnDeleteSelectionClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnHomeClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
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
  frmMainDraft: TfrmMainDraft;

implementation

uses
  ThItem;

{$R *.fmx}

procedure TfrmMainDraft.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvasEditor.Create(Self);
  FCanvas.Parent := Panel1;
  FCanvas.Align := TAlignLayout.alClient;

  FController := TThothController.Create;
  FController.RegistObserver(Self);

  FCanvasController := TThCanvasEditorController.Create;
  FCanvasController.SetSubject(FController);
  FCanvasController.SetThCanvas(FCanvas);

  edtZoom.Text := FloatToStr(FCanvas.ZoomScale);
end;

procedure TfrmMainDraft.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TfrmMainDraft.Notifycation(ACommand: IThCommand);
begin
  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TfrmMainDraft.SetSubject(ASubject: IThSubject);
begin
  FController.RegistObserver(Self);
end;

procedure TfrmMainDraft.btnRedoClick(Sender: TObject);
begin
  FController.Redo;

  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TfrmMainDraft.btnUndoClick(Sender: TObject);
begin
  FController.Undo;

  btnUndo.Enabled := FController.UndoCount > 0;
  btnRedo.Enabled := FController.RedoCount > 0;
end;

procedure TfrmMainDraft.btnZoomInClick(Sender: TObject);
begin
  FCanvas.ZoomIn;
  edtZoom.Text := FloatToStr(FCanvas.ZoomScale);
end;

procedure TfrmMainDraft.btnZoomOutClick(Sender: TObject);
begin
  FCanvas.ZoomOut;
  edtZoom.Text := FloatToStr(FCanvas.ZoomScale);
end;

procedure TfrmMainDraft.btnHomeClick(Sender: TObject);
begin
  FCanvas.ZoomHome;
end;

procedure TfrmMainDraft.btnImageClick(Sender: TObject);
begin
  FCanvas.AppendItemById(TButton(Sender).Tag);
end;

procedure TfrmMainDraft.btnRectangleClick(Sender: TObject);
begin
  FCanvas.DrawItemID := TButton(Sender).Tag;
end;

procedure TfrmMainDraft.btnDeleteSelectionClick(Sender: TObject);
begin
  FCanvas.DeleteSelection;
end;

end.
