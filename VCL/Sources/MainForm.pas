unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ThCanvas, Vcl.ExtCtrls,
  Vcl.ActnMan, Vcl.ActnColorMaps, Vcl.ComCtrls, Vcl.Samples.Spin, Vcl.WinXCtrls;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    pnlMain: TPanel;
    Button2: TButton;
    ColorBox1: TColorBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Button3: TButton;
    ToggleSwitch1: TToggleSwitch;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ToggleSwitch1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

uses
  DebugForm, GR32,
  ThTypes;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ToggleSwitch1.State := tssOff;
//  FCanvas.PenDrawMode := bdmPen;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  LFont: TFont;
begin
  LFont := Font;

  frmDebug.Show;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  ToggleSwitch1.State := tssOff;
  FCanvas.Clear;

  Font.Size
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  ToggleSwitch1.State := tssOff;
//  FCanvas.PenDrawMode := bdmEraser;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  ToggleSwitch1.State := tssOff;
//  FCanvas.ShapeMode := smRectangle;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  ToggleSwitch1.State := tssOff;
//  FCanvas.ShapeMode := smNone;
end;

procedure TfrmMain.ColorBox1Change(Sender: TObject);
begin
  FCanvas.PenStyle.Color := Color32(ColorBox1.Selected);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCanvas := TThCanvas.Create(nil);
  FCanvas.Align := alClient;
  FCanvas.Parent := pnlMain;
//  FCanvas.CreatePage(640, 480);
  FCanvas.CreatePage;

  FCanvas.PenStyle.Color := Color32(ColorBox1.Selected);
  FCanvas.PenStyle.Thickness := TrackBar1.Position;
  FCanvas.PenStyle.Opacity := SpinEdit1.Value;

  Font.Size
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TfrmMain.SpinEdit1Change(Sender: TObject);
begin
  FCanvas.PenStyle.Opacity := SpinEdit1.Value;
end;

procedure TfrmMain.ToggleSwitch1Click(Sender: TObject);
begin
  if ToggleSwitch1.IsOn then
    FCanvas.CanvasMode := cmFreeDraw
  else
    FCanvas.CanvasMode := cmShapeDraw;
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
  FCanvas.PenStyle.Thickness := TrackBar1.Position;
  Label1.Caption := IntToStr(FCanvas.PenStyle.Thickness);
end;

end.
