unit ThItem;

interface

uses
  GR32, GR32_VectorUtils,
  ThTypes;


type
  TThItem = class
  end;

  TThFreeDrawItem = class(TThItem)
  private
    FPath: TArray<TFloatPoint>;
    FPolyPoly: TThPolyPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
    FBounds: TFloatRect;
    FIsToDelete: Boolean;
  public
    property Path: TThPath read FPath;
    property Color: TColor32 read FColor;
    property Bounds: TFloatRect read FBounds;
    property Alpha: Byte read FAlpha write FAlpha;
    property IsToDelete: Boolean read FIsToDelete write FIsToDelete;
    property PolyPoly: TThPolyPoly read FPolyPoly;

    constructor Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
    destructor Destroy; override;
  end;

  TThRectangleItem = class(TThItem)
  private
    FRect: TFloatRect;
    FPoly: TThPoly;
    FThickness: Single;
    FColor: TColor32;
    FAlpha: Byte;
  public
    property Poly: TThPoly read FPoly;
    property Color: TColor32 read FColor;
    property Alpha: Byte read FAlpha write FAlpha;

    constructor Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
  end;


implementation

{ TThRectangleItem }

constructor TThRectangleItem.Create(ARect: TFloatRect; APoly: TThPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FRect := ARect;
  FPoly := APoly;
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;
end;


{ TThFreeDrawItem }

constructor TThFreeDrawItem.Create(APath: TThPath; APolyPoly: TThPolyPoly; AThickness: Single; AColor: TColor32; AAlpha: Byte);
begin
  FPath := APath;
  FPolyPoly := APolyPoly;
  FThickness := AThickness;
  FColor := AColor;
  FAlpha := AAlpha;

  FBounds := PolypolygonBounds(FPolyPoly);
end;

destructor TThFreeDrawItem.Destroy;
begin

  inherited;
end;

end.
