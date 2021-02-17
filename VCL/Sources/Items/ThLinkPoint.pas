{
  Role
    -
}
unit ThLinkPoint;

interface

uses
  System.Generics.Collections,
  GR32,
  ThDrawItem;

type
  TThItemLinkHandle = class

  end;

  TThItemLinkPoints = class
  private
    FShape: TThShapeItem;
  protected
    FFillColor,
    FHotColor,
    FBorderColor: TColor32;
    FRadius: Single;
    FBorderWidth: Single;

    FHandles: TArray<TThItemLinkHandle>;

    procedure CreateHandles; virtual; abstract;
    procedure FreeHandles; virtual;
    procedure RealignHandles; virtual; abstract;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
  public
//    constructor Create(AParent: TThDrawItem);
//    destructor Destroy; override;

    procedure SetShape(AShape: TThShapeItem);
  end;

implementation

{ TThItemLinkPoints }

procedure TThItemLinkPoints.SetShape(AShape: TThShapeItem);
begin
  FShape := AShape;
end;

procedure TThItemLinkPoints.FreeHandles;
begin

end;

procedure TThItemLinkPoints.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin

end;

end.
