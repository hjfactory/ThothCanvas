unit ThDrawStyle;

interface

uses
  System.Classes,

  GR32,

  ThTypes;

type
  TThDrawStyle = class(TInterfacedObject, IThDrawStyle)
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
    FOpacity: TThPercent;
    procedure SetOpacity(const Value: TThPercent);
    procedure SetColor(const Value: TColor32);
    function GetAplha: Byte;
  public
    constructor Create;

    property Color: TColor32 read FColor write SetColor;
    property Opacity: TThPercent read FOpacity write SetOpacity;
    property Alpha: Byte read GetAplha;
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

constructor TThPenStyle.Create;
begin
  FColor := clBlack32;
  FThickness := 10;
  FOpacity := 100;
end;

function TThPenStyle.GetAplha: Byte;
begin
  Result := Round(FOpacity / 100 * 255);
end;

procedure TThPenStyle.SetOpacity(const Value: TThPercent);
begin
  if FOpacity = Value then
    Exit;
  FOpacity := Value;
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
