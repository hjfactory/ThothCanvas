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

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
    function PointInObject(X, Y: Single): Boolean; override;
    procedure SetSelected(const Value: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
//    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
//    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
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
    procedure Painting; override;

    function GetClipRect: TRectF; override;
    function GetHighlightRect: TRectF; virtual;
    procedure DrawHighlight; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property HighlightColor: TAlphaColor read FHighlightColor write SetHighlightColor;
    property HighlightSize: Single read FHighlightSize write SetHighlightSize;
  end;

  TThItemClass = class of TThItem;

implementation

{ TThItem }

procedure TThItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  case Button of
    TMouseButton.mbLeft: Selected := True;
  end;
end;

function TThItem.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  P := AbsoluteToLocal(PointF(X, Y));

  Result := PtInItem(P);
end;

procedure TThItem.SetSelected(const Value: Boolean);
var
  R: TRectF;
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected and Assigned(FOnSelected) then
    FOnSelected(Self);

  InvalidateRect(ClipRect);
  Repaint;
end;

{ TThHighlightItem }

constructor TThHighlightItem.Create(AOwner: TComponent);
begin
  inherited;

  ClipParent := False;

  FHighlight := False;
  FhighlightColor := claGray;
  FHighlightSize := 10;
end;

procedure TThHighlightItem.DrawHighlight;
begin
end;

// 그림자 영역을 더해야 한다.
function TThHighlightItem.GetClipRect: TRectF;
begin
  Result := inherited GetClipRect;
  Result.Bottom := Result.Bottom + FHighlightSize;
  Result.Right := Result.Right + FHighlightSize;
end;

function TThHighlightItem.GetHighlightRect: TRectF;
begin
  Result := LocalRect;
  Result.Offset(HighlightSize, HighlightSize);
end;

procedure TThHighlightItem.Painting;
begin
  inherited;

  if FSelected then
    DrawHighlight;
end;

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
