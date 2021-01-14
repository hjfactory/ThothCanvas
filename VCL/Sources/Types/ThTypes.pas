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
  IThCanvas = interface
  end;

  TThCanvasMode = (cmSelection, cmFreeDraw);
  TThFreeDrawMode = (fdmPen, fdmEraser);
  TThShapeMode = (smNone, smRectangle);
  TThPath = TArray<TFloatPoint>;
  TThPercent = 0..100;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

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
