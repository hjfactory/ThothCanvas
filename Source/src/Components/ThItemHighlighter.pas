unit ThItemHighlighter;

interface

uses
  ThItemHighlighterIF, System.Types, FMX.Types, System.UITypes;

type
  TThItemShadowHighlighter = class(TInterfacedObject, IThItemHighlighter)
  private
    FParent: IThItemHighlitObject;

    FHighlight: Boolean;
    FHighlightColor: TAlphaColor;
    FHighlightSize: Single;

    procedure SetHighlightColor(const Value: TAlphaColor);
    procedure SetHighlightSize(const Value: Single);
  protected
    function GetParent: IThItemHighlitObject;
    procedure SetParent(Parent: IThItemHighlitObject);
    function GetHighlightRect: TRectF;
  public
    constructor Create(AOwner: IThItemHighlitObject);

    procedure DrawHighlight;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

implementation

uses
  System.UIConsts;

{ TThItemShadowHighligher }

constructor TThItemShadowHighlighter.Create(AOwner: IThItemHighlitObject);
begin
  FParent := AOwner;

  FHighlight := False;
  FHighlightColor := claGray;
  FHighlightSize := 10;
end;

procedure TThItemShadowHighlighter.DrawHighlight;
begin
  FParent.DrawHighlight;
end;

function TThItemShadowHighlighter.GetHighlightRect: TRectF;
begin
  Result := TControl(FParent).LocalRect;
  Result.Offset(HighlightSize, HighlightSize);
end;

function TThItemShadowHighlighter.GetParent: IThItemHighlitObject;
begin
  Result := FParent;
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

procedure TThItemShadowHighlighter.SetParent(Parent: IThItemHighlitObject);
begin
  FParent := Parent;
end;

end.
