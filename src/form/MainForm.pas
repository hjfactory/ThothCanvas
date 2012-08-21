unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  ThCanvas, ThMainController, FMX.Edit;

type
  TForm1 = class(TForm)
    pnlMainMenu: TPanel;
    pnlMenu: TPanel;
    pnlMain: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Undo: TButton;
    Redo: TButton;
    Button5: TButton;
    Edit2: TEdit;
    Button7: TButton;
    Edit3: TEdit;
    Button6: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
    FThCanvas: TThCanvas;
    FController: TThMainController;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ThShape, ThLayout;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FController := TThMainController.Create;

  FThCanvas := TThCanvas.Create(Self);
  FThCanvas.Parent := pnlMain;
  FThCanvas.Align := TAlignLayout.alClient;
  FThCanvas.SetSubject(FController);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThCanvas.Free;
end;

procedure TForm1.RedoClick(Sender: TObject);
begin
  FController.Redo;
end;

procedure TForm1.UndoClick(Sender: TObject);
begin
  FController.Undo;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FThCanvas.CurrentShapeClass := TThRectangle;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FThCanvas.CurrentShapeClass := TThLine;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Edit2.Text := FloatToStr(FThCanvas.ZoomOut);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Edit2.Text := FloatToStr(FThCanvas.ZoomIn);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FThCanvas.DeleteSelection;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FThCanvas.RotateContent := FThCanvas.RotateContent + 45;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  MV := StrToFloatDef(Edit3.Text, 0.0);
  FThCanvas.Repaint;

  Panel2.Cursor := crDefault;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  FThCanvas.RotateContent := FThCanvas.RotateContent - 45;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  FThCanvas.Test(StrToFloatDef(Edit1.Text, 0.0));
end;

end.
