unit ThDrawObjectManager;

interface

uses
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
    FItems: TDictionary<TThDrawItemClass, TDOMFI>;

    function GetDrawObject(AItem: TDOMFI): TThDrawObject; overload;
  public
    procedure Initialize; override;
    procedure Finalize; override;

    procedure RegistDrawObj(AId: Integer; AAlias: string; ACls: TThDrawObjectClass; AItemCls: TThDrawItemClass);

    function GetDrawObject(AId: Integer): TThDrawObject; overload;
    function GetDrawObject(AAlias: string): TThDrawObject; overload;
    function GetDrawObject(AItemCls: TThDrawItemClass): TThDrawObject; overload;
  end;

function DOMgr: TDrawObjectManager;

implementation

function DOMgr: TDrawObjectManager;
begin
  Result := TDrawObjectManager.Instance;
end;

{ TDrawObjectManager }

function TDrawObjectManager.GetDrawObject(AItem: TDOMFI): TThDrawObject;
begin
  if not Assigned(AItem.DrawObject) then
    AItem.DrawObject := AItem.DrawObjCls.Create(nil);

  Result := AItem.DrawObject;
end;

function TDrawObjectManager.GetDrawObject(
  AItemCls: TThDrawItemClass): TThDrawObject;
begin
  Result := nil;
  if FItems.ContainsKey(AItemCls) then
    Result := GetDrawObject(FItems.Items[AItemCls]);
end;

function TDrawObjectManager.GetDrawObject(AAlias: string): TThDrawObject;
var
  I: Integer;
  Item: TDOMFI;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if FItems.ToArray[I].Value.DrawObjAlias = AAlias then
      Exit(GetDrawObject(FItems.ToArray[I].Value));
end;

function TDrawObjectManager.GetDrawObject(AId: Integer): TThDrawObject;
var
  I: Integer;
  Item: TDOMFI;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if FItems.ToArray[I].Value.DrawObjId = AId then
      Exit(GetDrawObject(FItems.ToArray[I].Value));
end;

procedure TDrawObjectManager.Initialize;
begin
  FItems := TDictionary<TThDrawItemClass, TDOMFI>.Create;
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

  FItems.Add(AItemCls, Item);
end;

procedure TDrawObjectManager.Finalize;
begin
  inherited;

  FItems.Free;
end;

end.
