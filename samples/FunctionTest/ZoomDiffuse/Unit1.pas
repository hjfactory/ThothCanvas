unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani;

type
  TTest = class(TControl)
  private
    FDiffuse: Single;
    FAni: TFloatAnimation;

    procedure SetDiffuse(const Value: Single);
  protected
    procedure Paint; override;
  public
    property Diffuse: Single read FDiffuse write SetDiffuse;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FTest: TTest;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTest := TTest.Create(Self);
  FTest.Parent := Self;
  FTest.Width := 100;
  FTest.Height := 100;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTest.Free;
end;

{ TTest }

procedure TTest.Paint;
begin
  inherited;

end;

procedure TTest.SetDiffuse(const Value: Single);
begin
  FDiffuse := Value;

  Repaint;
end;

end.
