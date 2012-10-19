unit ThItemHighlighter;

interface

uses
  System.Types, System.SysUtils, FMX.Types, System.UITypes,
  ThTypes;

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
    function GetHighlightRect: TRectF;
  public
    constructor Create(AParent: IItemHighlitObject);

    procedure DrawHighlight;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

implementation

uses
  System.UIConsts, ThConsts;

{ TThItemShadowHighligher }

constructor TThItemShadowHighlighter.Create(AParent: IItemHighlitObject);
begin
  FParent := AParent;

  FHighlight := False;
end;

procedure TThItemShadowHighlighter.DrawHighlight;
begin
  FParent.PaintItem(GetHighlightRect, FHighlightColor);
end;

function TThItemShadowHighlighter.GetHighlightRect: TRectF;
begin
  Result := TControl(FParent).ClipRect;
  Result.Offset(HighlightSize, HighlightSize);
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
