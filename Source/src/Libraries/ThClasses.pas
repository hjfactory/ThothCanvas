unit ThClasses;

interface

uses
  System.Classes, ThTypes, ThItem;

type
  TThInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

implementation

{ TThInterfacedObject }

function TThInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TThInterfacedObject._AddRef: Integer;
begin
  Result := 0;
end;

function TThInterfacedObject._Release: Integer;
begin
  Result := 0;
end;

end.
