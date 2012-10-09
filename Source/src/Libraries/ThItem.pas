unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThItemHighlighterIF, ThItemResizablerIF;

type
//  IThItem = interface
//
//  end;

  TThItem = class(TControl)
  private
    FOnUnselected: TNotifyEvent;
    FOnSelected: TNotifyEvent;

    procedure SetSelected(const Value: Boolean);
  protected
    FHighlighter: IItemHighlighter;
    FResizabler: IItemResizabler;
    FSelected: Boolean;
    FMouseDownPos: TPointF;
//    FOnTrack: TNotifyEvent;

    function CreateHighlighter: IItemHighlighter; virtual;
    function CreateResizabler: IItemResizabler; virtual;

    function GetClipRect: TRectF; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure DoSelected(Selected: Boolean); virtual;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;

    function GetMinimumSize: TPointF; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Selected: Boolean read FSelected write SetSelected;

    procedure Painting; override;

    function PointInObject(X, Y: Single): Boolean; override;

//    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
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

  FHighlighter := CreateHighlighter;
  FResizabler := CreateResizabler;
end;

destructor TThItem.Destroy;
begin
  FHighlighter := nil;
  FResizabler := nil; // Interface destory

  inherited;
end;

function TThItem.CreateHighlighter: IItemHighlighter;
begin
end;

function TThItem.CreateResizabler: IItemResizabler;
begin
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

procedure TThItem.DoSelected(Selected: Boolean);
begin
  if FSelected then
    if Assigned(FOnSelected) then
      FOnSelected(Self)
  else
    if Assigned(FOnUnselected) then
      FOnUnselected(Self);
end;

function TThItem.GetClipRect: TRectF;
begin
  Result := inherited GetClipRect;
  if Assigned(FHighlighter) then
    Result := UnionRect(Result, FHighlighter.HighlightRect);
  if Assigned(FResizabler) then
    Result := UnionRect(Result, FResizabler.ResizablerRect);
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
    InvalidateRect(ClipRect);     //  잔상을 지우기 위해 기존 영역을 다시 그린다.
    Position.X := Position.X + (X - FMouseDownPos.X);
    Position.Y := Position.Y + (Y - FMouseDownPos.Y);
    InvalidateRect(ClipRect);     //  이동 시 하이라이트 표시 지원
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

  DoSelected(FSelected);

  InvalidateRect(ClipRect);
end;

end.
