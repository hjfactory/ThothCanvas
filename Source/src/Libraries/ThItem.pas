unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThItemHighlighterIF;

type
//  IThItem = interface
//
//  end;

  TThItem = class(TControl)
  private
    FOnUnselected: TNotifyEvent;
  protected
    FHighlighter: IItemHighlighter;

    FSelected,
    FHighlight: Boolean;

    FMouseDownPos: TPointF;

    FOnSelected: TNotifyEvent;
    FOnTrack: TNotifyEvent;

    function GetClipRect: TRectF; override;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
    procedure SetSelected(const Value: Boolean);
    function GetMinimumSize: TPointF; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Selected: Boolean read FSelected write SetSelected;

    procedure Painting; override;

    function PointInObject(X, Y: Single): Boolean; override;

    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property OnUnselected: TNotifyEvent read FOnUnselected write FOnUnselected;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

  TThItemClass = class of TThItem;

implementation

{ TThItem }

constructor TThItem.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;
end;

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

function TThItem.GetMinimumSize: TPointF;
begin
  Result := PointF(30, 30);
end;

procedure TThItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
   Selected := True;
   FMouseDownPos := PointF(X, Y);
  end;
end;

procedure TThItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FPressed then
  begin
    //  잔상을 지우기 위해 기존 영역을 다시 그린다.
    InvalidateRect(ClipRect);

    Position.X := Position.X + (X - FMouseDownPos.X);
    Position.Y := Position.Y + (Y - FMouseDownPos.Y);
  end;
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
end;

end.
