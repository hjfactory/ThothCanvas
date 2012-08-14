unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  ThCanvas, ThMainController;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
  ThShape;

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
  FThCanvas.ShapeClass := TThRectangle;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FThCanvas.ShapeClass := TThLine;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FThCanvas.ZoomOut;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FThCanvas.ZoomIn;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FThCanvas.DeleteSelection;
end;

end.
