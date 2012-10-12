unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThItemHighlighterIF, ThItemResizerIF;

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
    FResizer: IItemResizer;
    FSelected: Boolean;
    FMouseDownPos: TPointF;
//    FOnTrack: TNotifyEvent;

    function CreateHighlighter: IItemHighlighter; virtual;
    function CreateResizer: IItemResizer; virtual;

    function GetUpdateRect: TRectF; override;

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

    procedure Painting; override;
    function PointInObject(X, Y: Single): Boolean; override;

    procedure DrawItem(AFrom, ATo, AOffset: TPointF); virtual;

    property Selected: Boolean read FSelected write SetSelected;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property OnUnselected: TNotifyEvent read FOnUnselected write FOnUnselected;
    property MinimumSize: TPointF read GetMinimumSize;
  end;

  TThItemClass = class of TThItem;

implementation

uses
  CommonUtils;

{ TThItem }

constructor TThItem.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;

  FHighlighter := CreateHighlighter;
  FResizer := CreateResizer;
end;

destructor TThItem.Destroy;
begin
  FHighlighter := nil;
  FResizer := nil; // Interface destory

  inherited;
end;

function TThItem.CreateHighlighter: IItemHighlighter;
begin
end;

function TThItem.CreateResizer: IItemResizer;
begin
end;

procedure TThItem.DoMouseEnter;
begin
  inherited;

  // TControl.IsMouseOver 참고하여 Repaint만 수행
  Repaint;
end;

procedure TThItem.DoMouseLeave;
begin
  inherited;

  // TControl.IsMouseOver 참고하여 Repaint만 수행
  Repaint;
end;

procedure TThItem.DoSelected(Selected: Boolean);
begin
  if FSelected then
    if Assigned(FOnSelected) then
      FOnSelected(Self)
  else
    if Assigned(FOnUnselected) then
      FOnUnselected(Self);

  if Selected then
    FResizer.ShowSpots
  else
    FResizer.HideSpots;
end;

procedure TThItem.DrawItem(AFrom, ATo, AOffset: TPointF);
begin
end;

function TThItem.GetMinimumSize: TPointF;
begin
  Result := PointF(30, 30);
end;

function TThItem.GetUpdateRect: TRectF;
var
  R: TRectF;
begin
  Result := inherited GetUpdateRect;

  if Assigned(FHighlighter) then
  begin
    R := FHighlighter.HighlightRect;
    R.Offset(AbsoluteRect.Left, AbsoluteRect.Top);
    Result := UnionRect(Result, R);
  end;
  if Assigned(FResizer) then
  begin
    R := FResizer.ResizerRect;
    R.Offset(AbsoluteRect.Left, AbsoluteRect.Top);
    Result := UnionRect(Result, R);
  end;

  InflateRect(R, 1, 1);
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

  InvalidateRect(UpdateRect);
//  Repaint;
end;

procedure TThItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FPressed then
  begin
    Position.X := Position.X + (X - FMouseDownPos.X);
    Position.Y := Position.Y + (Y - FMouseDownPos.Y);

    InvalidateRect(UpdateRect);     //  잔상을 지우기 위해 기존 영역을 다시 그린다.
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

//  InvalidateRect(UpdateRect);
  Repaint;
end;

end.
