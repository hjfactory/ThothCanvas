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
    Button1: TButton;
    btnText: TButton;
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
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FController: TThothController;
    FCanvas: TThCanvasEditor;
    FCanvasController: TThCanvasEditorController;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure DisplayZoomValue(AZoom: Single);
  public
    { Public declarations }
  end;

var
  frmMainDraft: TfrmMainDraft;

implementation

uses
  ThConsts, ThItem, ThItemCommand, ThCanvasCommand;

{$R *.fmx}

procedure TfrmMainDraft.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvasEditor.Create(Self);
  FCanvas.Parent := Panel1;
  FCanvas.Align := TAlignLayout.alClient;
  FCanvas.Initialize;

  FController := TThothController.Create;
  FController.RegistObserver(Self);

  // 너는 ThothController 안으로 들어가야 한다.
  FCanvasController := TThCanvasEditorController.Create(FCanvas);
  FCanvasController.SetSubject(FController);

  DisplayZoomValue(FCanvas.ZoomScale);
end;

procedure TfrmMainDraft.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
  FCanvasController.Free;
  FController.Free;
end;

procedure TfrmMainDraft.Notifycation(ACommand: IThCommand);
begin
  if ACommand is TThItemCommand then
  begin
    btnUndo.Enabled := FController.UndoCount > 0;
    btnRedo.Enabled := FController.RedoCount > 0;
  end
  else if ACommand is TThCommandCanvasZoom then
  begin
    DisplayZoomValue(FCanvas.ZoomScale);
  end;
end;

procedure TfrmMainDraft.SetSubject(ASubject: IThSubject);
begin
  FController.RegistObserver(Self);
end;

procedure TfrmMainDraft.DisplayZoomValue(AZoom: Single);
begin
  AZoom := AZoom / CanvasZoomScaleDefault;

  edtZoom.Text := FormatFloat('0.##', AZoom);
end;

{
  Events
}

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
end;

procedure TfrmMainDraft.btnZoomOutClick(Sender: TObject);
begin
  FCanvas.ZoomOut;
end;

procedure TfrmMainDraft.Button1Click(Sender: TObject);
begin
  FCanvas.Test;
end;

procedure TfrmMainDraft.btnHomeClick(Sender: TObject);
begin
  FCanvas.ZoomHome;
end;

procedure TfrmMainDraft.btnImageClick(Sender: TObject);
begin
  FCanvas.AppendFileItem(TButton(Sender).Tag);
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
