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
  TThCanvasMode = (cmSelection, cmFreeDraw);
  TThBrushDrawMode = (bdmNone, bdmPen, bdmEraser);
  TThShapeMode = (smNone, smRectangle);
  TThPath = TArray<TFloatPoint>;
  TThPercent = 0..100;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

  TThItem = class
  end;

  IThCanvas = interface
  ['{5BC92595-BFFF-4D3F-A175-8A69F9C42CF9}']
  end;

  IThDrawObject = interface
    ['{F82BC1D0-3BB7-417F-897D-44E41154F0B1}']
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    procedure Move(const X, Y: TFloat); overload;
    procedure Move(const APoint: TFloatPoint); overload;

    function CreateItem: TThItem;
    procedure Clear;
  end;

  IThDrawStyle = interface
    ['{33DDCFFB-A309-4CAF-806C-E87A6CDB6048}']
  end;

  TThInterfacedPersistent = class(TPersistent, IInterface)
  private
    FRefCount: Integer;
  protected
    { IInterface }
    function QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  public
  end;

implementation

{ TThInterfacedPersistent }

function TThInterfacedPersistent.QueryInterface(const iid: TGuid;
  out obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TThInterfacedPersistent._AddRef: LongInt;
begin
  Result := AtomicIncrement(FRefCount)
end;

function TThInterfacedPersistent._Release: LongInt;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.
