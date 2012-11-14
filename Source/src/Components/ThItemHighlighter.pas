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
var
  ViewportScale: Single;
  ScaledHighlightSize: Single;
begin
{
  Result := TControl(FParent).ClipRect;
  InflateRect(Result, HighlightSize + 1, HighlightSize + 1);
Exit;
  Result := TControl(FParent).AbsoluteRect;
  Result.Offset(HighlightSize, HighlightSize);
Exit;
}
  Result := TControl(FParent).ClipRect;
  ViewportScale := TControl(FParent).AbsoluteScale.X;
    // Item의 스케일에서 Canvas의 Scale로 처리 필요
    //  - Item도 스케일이 변경될 수 있음
  ScaledHighlightSize := HighlightSize / ViewportScale;
  Result.Offset(ScaledHighlightSize, ScaledHighlightSize);
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
