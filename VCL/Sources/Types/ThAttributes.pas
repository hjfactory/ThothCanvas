unit ThAttributes;

interface

uses
  ThDrawItem, ThDrawStyle;

type
  TDrawObjInfo = record
    Id: Integer;
    Alias: string;
    StyleCls: TThDrawStyleClass;
    ItemCls: TThDrawItemClass;
  end;
  DrawObjAttribute = class(TCustomAttribute)
  public
    FInfo: TDrawObjInfo;
  public
    constructor Create(const AId: Integer; AAlias: string; AStyleCls: TThDrawStyleClass = nil; AItemCls: TThDrawItemClass = nil);
    destructor Destroy; override;

    property Info: TDrawObjInfo read FInfo;
  end;

  DrawObjAttr = DrawObjAttribute;

function TryDrawObjAttr(ACls: TClass; var AInfo: TDrawObjInfo): Boolean;


implementation

uses
  System.Rtti, ThDrawObject;

function TryDrawObjAttr(ACls: TClass; var AInfo: TDrawObjInfo): Boolean;
 var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  Result := False;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(ACls.ClassInfo);
    for LAttr in LType.GetAttributes do
    begin
      if LAttr is DrawObjAttribute then
      begin
        AInfo := DrawObjAttribute(LAttr).Info;
        Exit(True);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

{ DrawObjAttribute }

constructor DrawObjAttribute.Create(const AId: Integer; AAlias: string; AStyleCls: TThDrawStyleClass; AItemCls: TThDrawItemClass);
begin
  FInfo.Id := AId;
  FInfo.Alias := AAlias;
  FInfo.StyleCls := AStyleCls;
  FInfo.ItemCls := AItemCls;
end;

destructor DrawObjAttribute.Destroy;
begin

  inherited;
end;

end.
