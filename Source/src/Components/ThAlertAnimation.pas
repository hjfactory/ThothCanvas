unit ThAlertAnimation;

interface

uses
  System.Classes, System.Types,
  FMX.Types, FMX.Controls, FMX.Ani, FMX.Objects;

type
  TAlertAnimation = class(TControl)
  private
    FText: TText;
    FMessagePanel: TRoundRect;
    FAnimation: TFloatAnimation;

    procedure Finished(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Message(msg: string);
  end;

implementation

{ TAlertAnimation }

constructor TAlertAnimation.Create(AOwner: TComponent);
begin
  inherited;

  Visible := False;
  HitTest := False;

  Height := 80;

  FMessagePanel := TRoundRect.Create(Self);
  FMessagePanel.Parent := Self;
  FMessagePanel.Align := TAlignLayout.alClient;
  FMessagePanel.Fill.Color := $00FF333333;
  FMessagePanel.HitTest := False;

  FText := TText.Create(Self);
  FText.Parent := FMessagePanel;
  FText.Align := TAlignLayout.alClient;
  FText.Font.Size := 35;
  FText.Color := $FFFFFFFF;
  FText.HitTest := False;

  FAnimation := TFloatAnimation.Create(Self);
  FAnimation.Parent := Self;
  FAnimation.PropertyName := 'Opacity';
  FAnimation.OnFinish := Finished;
  FAnimation.StartValue := 0.8;
  FAnimation.StopValue := 0;
  FAnimation.Delay := 1;
  FAnimation.Duration := 0.4;
end;

destructor TAlertAnimation.Destroy;
begin

  inherited;
end;

procedure TAlertAnimation.Finished(Sender: TObject);
begin
  Visible := False;
end;

procedure TAlertAnimation.Message(msg: string);
begin
  FText.Text := msg;
  FText.Canvas.Font.Size := FText.Font.Size;
  Width := FText.Canvas.TextWidth(msg) * 1.1;

  if not Assigned(FParent) then
    Exit;

  Position.X := (TControl(Parent).Width - Width) / 2;
  Position.Y := (TControl(Parent).Height - Height) / 2;

  if FAnimation.Running then
    FAnimation.Stop;
  Visible := True;
  FAnimation.Start;
end;

end.
