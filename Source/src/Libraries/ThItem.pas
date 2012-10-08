unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThItemHighlighterIF;

type
  IThItem = interface

  end;

  TThItem = class(TControl, IThItem)
  private
    FOnUnselected: TNotifyEvent;
  protected
    FHighlighter: IItemHighlighter;
    FSelected,
    FHighlight: Boolean;

    FOnSelected: TNotifyEvent;
    FOnTrack: TNotifyEvent;

    function GetClipRect: TRectF; override;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
    procedure SetSelected(const Value: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    property Selected: Boolean read FSelected write SetSelected;

    procedure Painting; override;

    function PointInObject(X, Y: Single): Boolean; override;

    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property OnUnselected: TNotifyEvent read FOnUnselected write FOnUnselected;
  end;

  TThItemClass = class of TThItem;

implementation

{ TThItem }

procedure TThItem.DoMouseEnter;
begin
  inherited;

  InvalidateRect(ClipRect);
end;

procedure TThItem.DoMouseLeave;
begin
  inherited;

  InvalidateRect(ClipRect);
end;

function TThItem.GetClipRect: TRectF;
begin
  Result := inherited GetClipRect;
  if Assigned(FHighlighter) then
    Result := UnionRect(Result, FHighlighter.HighlightRect);
end;

procedure TThItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  case Button of
    TMouseButton.mbLeft: Selected := True;
  end;
end;

procedure TThItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

procedure TThItem.Painting;
begin
  inherited;

  if (FSelected or IsMouseOver) and Assigned(FHighlighter) then
    FHighlighter.DrawHighlight;
end;

function TThItem.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  P := AbsoluteToLocal(PointF(X, Y));

  Result := PtInItem(P);
end;

procedure TThItem.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected and Assigned(FOnSelected) then
    FOnSelected(Self);

  InvalidateRect(ClipRect);
  Repaint;
end;

end.
