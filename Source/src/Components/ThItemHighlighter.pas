unit ThItemHighlighter;

interface

uses
  ThItemHighlighterIF, System.Types, FMX.Types, System.UITypes;

type
  TThItemShadowHighlighter = class(TInterfacedObject, IItemHighlighter)
  private
    FParent: IItemHighlitObject;

    FHighlight: Boolean;
    FHighlightColor: TAlphaColor;
    FHighlightSize: Single;

    procedure SetHighlightColor(const Value: TAlphaColor);
    procedure SetHighlightSize(const Value: Single);
  protected
    function GetParent: IItemHighlitObject;
    procedure SetParent(Parent: IItemHighlitObject);
    function GetHighlightRect: TRectF;
  public
    constructor Create(AOwner: IItemHighlitObject);

    procedure DrawHighlight;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

implementation

uses
  System.UIConsts, ThConsts;

{ TThItemShadowHighligher }

constructor TThItemShadowHighlighter.Create(AOwner: IItemHighlitObject);
begin
  FParent := AOwner;

  FHighlight := False;
  FHighlightColor := ITEM_HIGHLIGHT_COLOR;
  FHighlightSize := ITEM_HIGHLIGHT_MIN;
end;

procedure TThItemShadowHighlighter.DrawHighlight;
begin
  FParent.DrawItem(GetHighlightRect, FHighlightColor);
end;

function TThItemShadowHighlighter.GetHighlightRect: TRectF;
begin
  Result := TControl(FParent).LocalRect;
  Result.Offset(HighlightSize, HighlightSize);
end;

function TThItemShadowHighlighter.GetParent: IItemHighlitObject;
begin
  Result := FParent;
end;

procedure TThItemShadowHighlighter.SetParent(Parent: IItemHighlitObject);
begin
  FParent := Parent;
end;

procedure TThItemShadowHighlighter.SetHighlightColor(const Value: TAlphaColor);
begin
  if FHighlightColor = Value then
    Exit;

  FHighlightColor := Value;
  TControl(FParent).Repaint;
end;

procedure TThItemShadowHighlighter.SetHighlightSize(const Value: Single);
begin
  if FHighlightSize = Value then
    Exit;

  FHighlightSize := Value;
  TControl(FParent).Repaint;
end;

end.
