{
  Role
    Specify the default style for draw objects
}
unit ThDrawStyle;

interface

uses
  System.Classes,

  GR32,

  ThTypes, ThClasses;

type
  TThDrawStyle = class;
  TThDrawStyleClass = class of TThDrawStyle;

  TThDrawStyle = class(TInterfacedObject, IThDrawStyle)
  private
    FOnChange: TNotifyEvent;
    procedure DoChange;
  public
    constructor Create; virtual;
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
    constructor Create; override;
    destructor Destroy; override;

    property Color: TColor32 read FColor write SetColor;
    property Opacity: TThPercent read FOpacity write SetOpacity;
    property Alpha: Byte read GetAplha;
  end;

  TThEraserStyle = class(TThBrushStyle)
  public
    constructor Create; override;
  end;

  TThShapeStyle = class(TThDrawStyle)
  private
    FColor: TColor32;
    FBorderColor: TColor32;
    FBorderWidth: Integer;
  public
    constructor Create; override;

    property Color: TColor32 read FColor write FColor;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BorderColor: TColor32 read FBorderColor write FBorderColor;
  end;

implementation

{ TThDrawStyle }

constructor TThDrawStyle.Create;
begin
end;

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
  FThickness := 10;
  FColor := clBlack32;
  FOpacity := 100;
end;

destructor TThPenStyle.Destroy;
begin

  inherited;
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

{ TThEraserStyle }

constructor TThEraserStyle.Create;
begin
  FThickness := 10;
end;

{ TThShapeStyle }

constructor TThShapeStyle.Create;
begin
  FColor := clYellow32;
  FBorderColor := clGray32;
  FBorderWidth := 4;
end;

end.
