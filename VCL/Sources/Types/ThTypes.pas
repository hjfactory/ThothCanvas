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
  TThCanvasMode = (cmSelection, cmFreeDraw, cmShapeDraw);

  TThBrushDrawMode = (bdmNone, bdmPen, bdmEraser);
  TThShapeMode = (smNone, smRectangle);

  TThPath = TArray<TFloatPoint>;
  TThPercent = 0..100;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

  TThItem = class
  end;

  IThCanvas = interface
  end;

  IThDrawObject = interface
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    procedure Move(const X, Y: TFloat); overload;
    procedure Move(const APoint: TFloatPoint); overload;

    function CreateItem: TObject;
    procedure Clear;
  end;

  IThDrawStyle = interface
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

  TObjectHelper = class helper for TObject
    function Clone: TObject;
  end;

implementation

uses
  System.JSON,
  DBXJSONReflect
//  REST.JsonReflect
  ;

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

function TObjectHelper.Clone: TObject;
var
  MarshalObj: TJSONMarshal;
  UnMarshalObj: TJSONUnMarshal;
  JSONValue: TJSONValue;
begin
  Result:= nil;
  MarshalObj := TJSONMarshal.Create;
  UnMarshalObj := TJSONUnMarshal.Create;
  try
    JSONValue := MarshalObj.Marshal(Self);
    try
      if Assigned(JSONValue) then
        Result:= UnMarshalObj.Unmarshal(JSONValue);
    finally
      JSONValue.Free;
    end;
  finally
    MarshalObj.Free;
    UnMarshalObj.Free;
  end;
end;

end.
