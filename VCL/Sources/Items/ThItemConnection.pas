{
  Role
    -
}
unit ThItemConnection;

interface

uses
  System.Generics.Collections,
  GR32,
  ThItem;

type
  TThItemAnchorPoint = class

  end;

  TThItemAnchorPoints = class
  private
    FShape: TThShapeItem;
  protected
    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    FHandles: TArray<TThItemAnchorPoint>;

    procedure CreateHandles; virtual; abstract;
    procedure FreeHandles; virtual;
    procedure RealignHandles; virtual; abstract;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
  public
//    constructor Create(AParent: TThItem);
//    destructor Destroy; override;

    procedure SetShape(AShape: TThShapeItem);
  end;

implementation

{ TThItemLinkPoints }

procedure TThItemAnchorPoints.SetShape(AShape: TThShapeItem);
begin
  FShape := AShape;
end;

procedure TThItemAnchorPoints.FreeHandles;
begin

end;

procedure TThItemAnchorPoints.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin

end;

end.
