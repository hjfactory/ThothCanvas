unit ThInterface;

interface

uses
  FMX.Types;

type
  TThInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

  IThObserver = interface;

  IThCommand = interface
  end;

  IThSubject = interface
    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

  IThCanvas = interface
  end;

  IThItem = interface(IControl)
    procedure SetSelected(const Value: Boolean);
    property Selected: Boolean write SetSelected;
  end;

  IThShape = interface
  end;

  IThControlPanel = interface
    procedure Show;
    procedure Hide;
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
