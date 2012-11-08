unit ThClasses;

interface

uses
  System.Classes, System.Types, ThTypes, ThItem;

type
//  TThFixedSizeObject = class(TInterfacedObject)
//  protected
//    FInheritedOpacity: Boolean;
//    FInheritedScale: Boolean;
//
//    function GetAbsoluteOpacity: Single; override;
//    function GetAbsoluteMatrix: TMatrix; override;
//  public
//    constructor Create(AOwner: TComponent); override;
//  end;
//
  TThInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

implementation

//{ TThFixedSizeObject }
//
//constructor TThFixedSizeObject.Create(AOwner: TComponent);
//begin
//  inherited;
//
//  FInheritedOpacity := True;
//  FInheritedScale := True;
//end;
//
//function TThFixedSizeObject.GetAbsoluteMatrix: TMatrix;
//begin
//  Result := inherited GetAbsoluteMatrix;
//  if not FInheritedScale then
//  begin
////    Result.m31 := Position.X
////    Result.m32
//    Result.m11 := Scale.X;
//    Result.m22 := Scale.Y;
//  end;
//end;
//
//function TThFixedSizeObject.GetAbsoluteOpacity: Single;
//begin
//  if FInheritedOpacity then
//    Result := inherited
//  else
//    Result := FOpacity;
//end;

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
