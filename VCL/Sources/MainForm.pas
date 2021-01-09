unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ThCanvas, Vcl.ExtCtrls,
  Vcl.ActnMan, Vcl.ActnColorMaps, Vcl.ComCtrls, Vcl.Samples.Spin;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    pnlMain: TPanel;
    Button2: TButton;
    ColorBox1: TColorBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
    FCanvas: TThCanvas;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses DebugForm;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  frmDebug.Show;
end;

procedure TfrmMain.ColorBox1Change(Sender: TObject);
begin
  FCanvas.PenColor := ColorBox1.Selected;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvas.Create(nil);
  FCanvas.Align := alClient;
  FCanvas.Parent := pnlMain;
  FCanvas.CreatePage(640, 480);

  FCanvas.PenColor := ColorBox1.Selected;
  FCanvas.PenSize := TrackBar1.Position;
  FCanvas.PenOpacity := SpinEdit1.Value;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TfrmMain.SpinEdit1Change(Sender: TObject);
begin
  FCanvas.PenOpacity := SpinEdit1.Value;
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
  FCanvas.PenSize := TrackBar1.Position;
  Label1.Caption := IntToStr(FCanvas.PenSize);
end;

end.
