{
  ON_HIGHLIGHT�� ������ ��츸 ���̶���Ʈ(�׸���) ȿ���� �߻���
   - $DEFINE ON_HIGHLIGHT
}
unit ThItemHighlighter;

interface

uses
  System.Types, System.SysUtils, FMX.Types, System.UITypes, FMX.Controls,
  FMX.Graphics, ThTypes;

type
  TThItemShadowHighlighter = class(TInterfacedObject, IItemHighlighter)
  private
    FParent: IItemHighlightObject;

    FHighlight: Boolean;
    FHighlightColor: TAlphaColor;
    FHighlightSize: Single;

    procedure SetHighlightColor(const Value: TAlphaColor);
    procedure SetHighlightSize(const Value: Single);
  protected
    function GetHighlightRect: TRectF;
  public
    constructor Create(AParent: IItemHighlightObject);

    procedure DrawHighlight;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

  // Not used
  TThItemRectBorderHighlighter = class(TInterfacedObject, IItemHighlighter)
  private
    FParent: IItemHighlightObject;

    FHighlight: Boolean;
    FHighlightColor: TAlphaColor;
    FHighlightSize: Single;

    procedure SetHighlightColor(const Value: TAlphaColor);
    procedure SetHighlightSize(const Value: Single);
  protected
    function GetHighlightRect: TRectF;
  public
    constructor Create(AParent: IItemHighlightObject);

    procedure DrawHighlight;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

implementation

uses
  System.UIConsts, ThConsts;

{ TThItemShadowHighligher }

constructor TThItemShadowHighlighter.Create(AParent: IItemHighlightObject);
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
  Result := TControl(FParent).ClipRect;
  ViewportScale := TControl(FParent).AbsoluteScale.X;
    // Item�� �����Ͽ��� Canvas�� Scale�� ó�� �ʿ�
    //  - Item�� �������� ����� �� ����
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

{ TThItemBorderHighlighter }

constructor TThItemRectBorderHighlighter.Create(AParent: IItemHighlightObject);
begin
  FParent := AParent;

  FHighlight := False;
end;

procedure TThItemRectBorderHighlighter.DrawHighlight;
var
  State: TCanvasSaveState;
begin
  with TControl(FParent) do
  begin
    State := Canvas.SaveState;
    try
      Canvas.Stroke.Color := FHighlightColor;
      Canvas.Stroke.Thickness := FHighlightSize;
      Canvas.Stroke.Dash := TStrokeDash.Dash;
      Canvas.DrawRect(ClipRect, 0, 0, AllCorners, 1);
    finally
      Canvas.RestoreState(State);
    end;
  end;
end;

function TThItemRectBorderHighlighter.GetHighlightRect: TRectF;
var
  ViewportScale: Single;
  ScaledHighlightSize: Single;
begin
  Result := TControl(FParent).ClipRect;
  ViewportScale := TControl(FParent).AbsoluteScale.X;
    // Item�� �����Ͽ��� Canvas�� Scale�� ó�� �ʿ�
    //  - Item�� �������� ����� �� ����
  ScaledHighlightSize := HighlightSize / ViewportScale;
  Result.Inflate(ScaledHighlightSize, ScaledHighlightSize);
end;

procedure TThItemRectBorderHighlighter.SetHighlightColor(const Value: TAlphaColor);
begin
  if FHighlightColor = Value then
    Exit;

  FHighlightColor := Value;
  TControl(FParent).Repaint;
end;

procedure TThItemRectBorderHighlighter.SetHighlightSize(const Value: Single);
begin
  if FHighlightSize = Value then
    Exit;

  FHighlightSize := Value;
  TControl(FParent).Repaint;
end;

end.
