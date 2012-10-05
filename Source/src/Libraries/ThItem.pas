unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types;

type
  IThItem = interface

  end;

  TThItem = class(TControl, IThItem)
  protected
    FSelected: Boolean;

    FOnSelected: TNotifyEvent;
    FOnTrack: TNotifyEvent;

    procedure SetSelected(const Value: Boolean);
  public
    property Selected: Boolean read FSelected write SetSelected;

    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
  end;

  TThHighlightItem = class(TThItem)
  private
    FHighlight: Boolean;
    FHighlightColor: TAlphaColor;
    FHighlightSize: Single;

    procedure SetHighlightColor(const Value: TAlphaColor);
    procedure SetHighlightSize(const Value: Single);
  protected
//    function GetWidth: Single;

    procedure DrawHighlight; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

  TThItemClass = class of TThItem;

implementation

{ TThItem }

procedure TThItem.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected and Assigned(FOnSelected) then
    FOnSelected(Self);

  Repaint;
end;

{ TThHighlightItem }

constructor TThHighlightItem.Create(AOwner: TComponent);
begin
  inherited;

  FHighlight := False;
  FhighlightColor := claGray;
  FHighlightSize := 10;
end;

procedure TThHighlightItem.DrawHighlight;
begin

end;

//function TThHighlightItem.GetWidth: Single;
//begin
//  Result := FWidth;
//end;

procedure TThHighlightItem.SetHighlightColor(const Value: TAlphaColor);
begin
  if FHighlightColor = Value then
    Exit;

  FHighlightColor := Value;
  Repaint;
end;

procedure TThHighlightItem.SetHighlightSize(const Value: Single);
begin
  if FHighlightSize = Value then
    Exit;

  FHighlightSize := Value;
  Repaint;
end;

end.
