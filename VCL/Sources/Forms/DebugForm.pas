unit DebugForm;

interface

uses
  System.Types,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmDebug = class(TForm)
    edtMousePos: TLabeledEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure DebugMousePos(AText: string; APoint: TPointF);

var
  frmDebug: TfrmDebug;

implementation

{$R *.dfm}

procedure DebugMousePos(AText: string; APoint: TPointF);
begin
  if Assigned(frmDebug) then
  begin
    frmDebug.edtMousePos.EditLabel.Caption := AText;
    frmDebug.edtMousePos.Text := Format('%f x %f', [APoint.X, APoint.Y]);
  end;
end;

procedure TfrmDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
