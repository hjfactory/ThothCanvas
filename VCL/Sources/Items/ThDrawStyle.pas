unit ThDrawStyle;

interface

uses
  System.Classes,
  GR32,
  ThTypes;

type
  IThDrawStyle = interface
    ['{33DDCFFB-A309-4CAF-806C-E87A6CDB6048}']
  end;

  TThDrawStyle = class(TThInterfacedPersistent, IThDrawStyle)
  private
    FOnChange: TNotifyEvent;
    procedure DoChange;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TThBrushStyle = class(TThDrawStyle)
  private
    FThickness: Integer;
    procedure SetThickness(const Value: Integer);
  public
    property Thickness: Integer read FThickness write SetThickness;
  end;

  TThPenStyle = class(TThBrushStyle)
  private
    FColor: TColor32;
    FAlpha: Byte;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor32);
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;

    property Color: TColor32 read FColor write SetColor;
    property Alpha: Byte read FAlpha write SetAlpha;
  end;

  TThEraserStyle = class(TThBrushStyle)
  end;

implementation

{ TThDrawStyle }

procedure TThDrawStyle.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TThBrushStyle }

procedure TThBrushStyle.SetThickness(const Value: Integer);
begin
  if FThickness = Value then
    Exit;
  FThickness := Value;
  DoChange;
end;

{ TThPenStyle }

procedure TThPenStyle.Assign(Source: TPersistent);
begin
  if Source is TThPenStyle then
  begin
    FColor := TThPenStyle(Source).Color;
    FThickness := TThPenStyle(Source).Thickness;
    FAlpha := TThPenStyle(Source).Alpha;
  end;

  inherited;
end;

constructor TThPenStyle.Create;
begin
  FColor := clBlack32;
  FThickness := 10;
  FAlpha := 255;
end;

procedure TThPenStyle.SetAlpha(const Value: Byte);
begin
  if FAlpha = Value then
    Exit;
  FAlpha := Value;
  DoChange;
end;

procedure TThPenStyle.SetColor(const Value: TColor32);
begin
  if FColor = Value then
    Exit;
  FColor := Value;
  DoChange;
end;

end.
