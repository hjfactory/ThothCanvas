unit DebugForm;

interface

uses
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

procedure DebugMousePos(const X, Y: Single);

var
  frmDebug: TfrmDebug;

implementation

{$R *.dfm}

procedure DebugMousePos(const X, Y: Single);
begin
  if Assigned(frmDebug) then
    frmDebug.edtMousePos.Text := Format('%f x %f', [X, Y]);
end;

procedure TfrmDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
