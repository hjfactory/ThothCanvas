unit ThDrawObjectManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  ThTypes,
  ThDrawItem,
  ThDrawObject,
  ThDrawStyle,

  DMX.DesignPattern;

type
  TDOMgrItem{DrawObjectManagerFactoryItem} = class
    DrawObjId: Integer;
    DrawObjAlias: string;
    DrawObject: TThDrawObject;
    DrawObjCls: TThDrawObjectClass;
    DrawStyleCls: TThDrawStyleClass;
    DrawItemCls: TThDrawItemClass;
  end;

  TDrawObjectManager = class(TSingleton<TDrawObjectManager>)
  private
    FDrawObjs: TObjectDictionary<Integer, TDOMgrItem>;
    FDrawItems: TObjectDictionary<TThDrawItemClass, TDOMgrItem>;

//    FEraDrawObj: TThObjErsDrawObject;
//    FPenDrawObj: TThPenDrawObject;
//    FSelDrawObj: TThShapeSelectObject;
    FEraItem: TDOMgrItem;
    FPenItem: TDOMgrItem;
    FSelItem: TDOMgrItem;

    function GetDrawObject(AItem: TDOMgrItem): TThDrawObject; overload;
    function GetEraDrawObj: TThObjErsDrawObject;
    function GetPenDrawObj: TThPenDrawObject;
    function GetSelDrawObj: TThShapeSelectObject;
    procedure SetEraDrawObj(const Value: TThObjErsDrawObject);
    procedure SetPenDrawObj(const Value: TThPenDrawObject);
    procedure SetSelDrawObj(const Value: TThShapeSelectObject);
  public
    procedure Initialize; override;
    procedure Finalize; override;

    procedure RegistDrawObj(ACls: TThDrawObjectClass);

    function GetDrawObject(AId: Integer): TThDrawObject; overload;
    function GetDrawObject(AAlias: string): TThDrawObject; overload;
    function GetDrawObject(AItemCls: TThDrawItemClass): TThDrawObject; overload;
    function GetDrawObject(AItem: TThDrawItem): TThDrawObject; overload;

    function AliasToId(AValue: string): Integer;

    // Specific objects are pre-declared
    property PenDrawObj: TThPenDrawObject read GetPenDrawObj write SetPenDrawObj;
    property EraDrawObj: TThObjErsDrawObject read GetEraDrawObj write SetEraDrawObj;
    property SelDrawObj: TThShapeSelectObject read GetSelDrawObj write SetSelDrawObj;
  end;

function DOMgr: TDrawObjectManager;

implementation

uses
  ThAttributes;

function DOMgr: TDrawObjectManager;
begin
  Result := TDrawObjectManager.Instance;
end;

{ TDrawObjectManager }

procedure TDrawObjectManager.Initialize;
begin
  FDrawObjs := TObjectDictionary<Integer, TDOMgrItem>.Create([doOwnsValues]);
  FDrawItems := TObjectDictionary<TThDrawItemClass, TDOMgrItem>.Create;
end;

procedure TDrawObjectManager.Finalize;
begin
  inherited;

  FDrawItems.Free;
  FDrawObjs.Free;
end;

procedure TDrawObjectManager.RegistDrawObj(ACls: TThDrawObjectClass);
var
  Item: TDOMgrItem;
  Info: TDrawObjInfo;
  LAttr: DrawObjAttribute;
begin
  if not TryDrawObjAttr(ACls, Info) then
    raise Exception.CreateFmt('Not found DrawObjAttr on ''%s''', [ACls.ClassName]);

  Item := TDOMgrItem.Create;

  Item.DrawObjId := Info.Id;
  Item.DrawObjAlias := Info.Alias;
  Item.DrawObjCls := ACls;
  Item.DrawStyleCls := Info.StyleCls;
  Item.DrawItemCls := Info.ItemCls;
  Item.DrawObject := nil;

  FDrawObjs.Add(Item.DrawObjId, Item);
  if Assigned(Item.DrawItemCls) then
    FDrawItems.Add(Item.DrawItemCls, Item);

  if Item.DrawObjCls = TThPenDrawObject then
    FPenItem := Item //Item.DrawObject as TThPenDrawObject
  else if Item.DrawObjCls = TThObjErsDrawObject then
    FEraItem := Item //Item.DrawObject as TThObjErsDrawObject
  else if Item.DrawObjCls = TThShapeSelectObject then
    FSelITem := Item //Item.DrawObject as TThShapeSelectObject
  ;
end;

procedure TDrawObjectManager.SetEraDrawObj(const Value: TThObjErsDrawObject);
begin
  FEraItem.DrawObject := Value;
end;

procedure TDrawObjectManager.SetPenDrawObj(const Value: TThPenDrawObject);
begin
  FPenItem.DrawObject := Value;
end;

procedure TDrawObjectManager.SetSelDrawObj(const Value: TThShapeSelectObject);
begin
  FSelItem.DrawObject := Value;
end;

function TDrawObjectManager.GetDrawObject(AItem: TDOMgrItem): TThDrawObject;
begin
  if not Assigned(AItem.DrawObject) and Assigned(AItem.DrawObjCls) then
    if Assigned(AItem.DrawStyleCls) then
      AItem.DrawObject := AItem.DrawObjCls.Create(AItem.DrawStyleCls.Create)
    else
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

function TDrawObjectManager.GetEraDrawObj: TThObjErsDrawObject;
begin
  Result := GetDrawObject(FEraItem.DrawObjId) as TThObjErsDrawObject;
end;

function TDrawObjectManager.GetPenDrawObj: TThPenDrawObject;
begin
  Result := GetDrawObject(FPenItem.DrawObjId) as TThPenDrawObject;
end;

function TDrawObjectManager.GetSelDrawObj: TThShapeSelectObject;
begin
  Result := GetDrawObject(FSelItem.DrawObjId) as TThShapeSelectObject;
end;

function TDrawObjectManager.GetDrawObject(AAlias: string): TThDrawObject;
var
  I: Integer;
  Item: TDOMgrItem;
begin
  Result := nil;
  for I := 0 to FDrawObjs.Count - 1 do
    if CompareText(FDrawObjs.ToArray[I].Value.DrawObjAlias, AAlias) = 0 then
      Exit(GetDrawObject(FDrawObjs.ToArray[I].Value));
end;

function TDrawObjectManager.GetDrawObject(AId: Integer): TThDrawObject;
begin
  Result := nil;
  if FDrawObjs.ContainsKey(AId) then
    Result := GetDrawObject(FDrawObjs.Items[AId]);
end;

function TDrawObjectManager.AliasToId(AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDrawObjs.Count - 1 do
    if CompareText(FDrawObjs.ToArray[I].Value.DrawObjAlias, AValue) = 0 then
      Exit(FDrawObjs.ToArray[I].Value.DrawObjId);
end;

end.
