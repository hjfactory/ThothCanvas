unit ThDrawObjectManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  ThTypes,
  ThDrawItem,
  ThDrawObject,

  DMX.DesignPattern;

type
  TDOMFI{DrawObjectManagerFactoryItem} = record
    DrawObjId: Integer;
    DrawObjAlias: string;
    DrawObject: TThDrawObject;
    DrawObjCls: TThDrawObjectClass;
    DrawItemCls: TThDrawItemClass;
  end;

  TDrawObjectManager = class(TSingleton<TDrawObjectManager>)
  private
    FDrawObjs: TDictionary<Integer, TDOMFI>;
    FDrawItems: TDictionary<TThDrawItemClass, TDOMFI>;

    function GetDrawObject(AItem: TDOMFI): TThDrawObject; overload;
  public
    procedure Initialize; override;
    procedure Finalize; override;

    procedure RegistDrawObj(AId: Integer; AAlias: string; ACls: TThDrawObjectClass; AItemCls: TThDrawItemClass = nil);

    function GetDrawObject(AId: Integer): TThDrawObject; overload;
    function GetDrawObject(AAlias: string): TThDrawObject; overload;
    function GetDrawObject(AItemCls: TThDrawItemClass): TThDrawObject; overload;
    function GetDrawObject(AItem: TThDrawItem): TThDrawObject; overload;

    function AliasToId(AValue: string): Integer;
  end;

function DOMgr: TDrawObjectManager;

implementation

function DOMgr: TDrawObjectManager;
begin
  Result := TDrawObjectManager.Instance;
end;

{ TDrawObjectManager }

procedure TDrawObjectManager.Initialize;
begin
  FDrawObjs := TDictionary<Integer, TDOMFI>.Create;
  FDrawItems := TDictionary<TThDrawItemClass, TDOMFI>.Create;
end;

procedure TDrawObjectManager.Finalize;
begin
  inherited;

  FDrawItems.Free;
  FDrawObjs.Free;
end;

procedure TDrawObjectManager.RegistDrawObj(AId: Integer; AAlias: string;
  ACls: TThDrawObjectClass; AItemCls: TThDrawItemClass);
var
  Item: TDOMFI;
begin
  Item.DrawObjId := AId;
  Item.DrawObjAlias := AAlias;
  Item.DrawObjCls := ACls;
  Item.DrawItemCls := AItemCls;
  Item.DrawObject := nil;

  FDrawObjs.Add(AId, Item);
  if Assigned(AItemCls) then
    FDrawItems.Add(AItemCls, Item);
end;

function TDrawObjectManager.GetDrawObject(AItem: TDOMFI): TThDrawObject;
begin
  if not Assigned(AItem.DrawObject) and Assigned(AItem.DrawObjCls) then
    AItem.DrawObject := AItem.DrawObjCls.Create(nil);

  Result := AItem.DrawObject;
end;

function TDrawObjectManager.GetDrawObject(
  AItemCls: TThDrawItemClass): TThDrawObject;
begin
  Result := nil;
  if FDrawItems.ContainsKey(AItemCls) then
    Result := GetDrawObject(FDrawItems.Items[AItemCls]);
end;

function TDrawObjectManager.GetDrawObject(AItem: TThDrawItem): TThDrawObject;
begin
  Result := GetDrawObject(TThDrawItemClass(AItem.ClassType));
end;

function TDrawObjectManager.GetDrawObject(AAlias: string): TThDrawObject;
var
  I: Integer;
  Item: TDOMFI;
begin
  Result := nil;
  for I := 0 to FDrawObjs.Count - 1 do
    if CompareText(FDrawObjs.ToArray[I].Value.DrawObjAlias, AAlias) = 0 then
      Exit(GetDrawObject(FDrawObjs.ToArray[I].Value));
end;

function TDrawObjectManager.GetDrawObject(AId: Integer): TThDrawObject;
var
  I: Integer;
  Item: TDOMFI;
begin
  Result := nil;
  if FDrawObjs.ContainsKey(AId) then
    Result := GetDrawObject(FDrawObjs.Items[AId]);
end;

function TDrawObjectManager.AliasToId(AValue: string): Integer;
var
  I: Integer;
  Item: TDOMFI;
begin
  Result := -1;

  for I := 0 to FDrawObjs.Count - 1 do
    if CompareText(FDrawObjs.ToArray[I].Value.DrawObjAlias, AValue) = 0 then
      Exit(FDrawObjs.ToArray[I].Value.DrawObjId);
end;

end.
