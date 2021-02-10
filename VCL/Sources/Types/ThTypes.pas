unit ThTypes;

interface

uses
  System.Classes,
  System.Generics.Collections,
  GR32;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;

type
  TThDrawMode       = (dmSelect, dmDraw, dmPen, dmEraser);

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThCanvas = interface
  end;

  IThDrawStyle = interface
  end;

  IThDrawItem = interface
  end;

  IThShapeItem = interface(IThDrawItem)
    function MakePolyPoly(ARect: TFloatRect): TThPolyPoly;
  end;

  IThItemSelection = interface
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure Realign;

    procedure MouseDown(const APoint: TFloatPoint);
    procedure MouseMove(const APoint: TFloatPoint);   // MouseDown & move
    procedure MouseUp(const APoint: TFloatPoint);
    procedure MouseOver(const APoint: TFloatPoint);   // Not mouse downed & move

    function PtInSelection(APoint: TFloatPoint): Boolean;
    function IsOverHandle: Boolean;
  end;

  IThDrawObject = interface
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    function GetDrawItem: IThDrawItem;
    property DrawItem: IThDrawItem read GetDrawItem;
  end;

  TFloatRectHelper = record helper for TFloatRect
  private
    function GetBottomLeft: TFloatPoint;
    function GetTopRight: TFloatPoint;
    procedure SetBottomLeft(const Value: TFloatPoint);
    procedure SetTopRight(const Value: TFloatPoint);
    function GetWidth: TFloat;
    procedure SetWidth(const Value: TFloat);
    function GetHeight: TFloat;
    procedure SetHeight(const Value: TFloat);
  public
    property TopRight: TFloatPoint read GetTopRight write SetTopRight;
    property BottomLeft: TFloatPoint read GetBottomLeft write SetBottomLeft;

    property Width: TFloat read GetWidth write SetWidth;
    property Height: TFloat read GetHeight write SetHeight;
  end;

implementation

{ TFloatRectHelper }

function TFloatRectHelper.GetBottomLeft: TFloatPoint;
begin
  Result := FloatPoint(Self.Bottom, Self.Left);
end;

function TFloatRectHelper.GetHeight: TFloat;
begin
  Result := Self.Bottom - Self.Top;
end;

function TFloatRectHelper.GetTopRight: TFloatPoint;
begin
  Result := FloatPoint(Self.Top, Self.Right);
end;

function TFloatRectHelper.GetWidth: TFloat;
begin
  Result := Self.Right - Self.Left;
end;

procedure TFloatRectHelper.SetBottomLeft(const Value: TFloatPoint);
begin
  Self.Bottom := Value.Y;
  Self.Left := Value.X;
end;

procedure TFloatRectHelper.SetHeight(const Value: TFloat);
begin
  Self.Bottom := Self.Top + Value;
end;

procedure TFloatRectHelper.SetTopRight(const Value: TFloatPoint);
begin
  Self.Top := Value.Y;
  Self.Right := value.X;
end;

procedure TFloatRectHelper.SetWidth(const Value: TFloat);
begin
  Self.Right := Self.Left + Value;
end;

end.
