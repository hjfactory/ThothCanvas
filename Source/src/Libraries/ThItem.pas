unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThTypes, System.Generics.Collections;

type
  TThItem = class;
  TThItems = TList<TThItem>;

  TItemEvent = procedure(Item: TThItem) of object;
  TItemSelectedEvent = procedure(Item: TThItem; IsMultiSelect: Boolean) of object;
  TItemMoveEvent = procedure(Item: TThItem; StartPos: TPointF) of object;
  TItemResizeEvent = procedure(Item: TThItem; BeforeRect: TRectF) of object;

  TItemListEvent = procedure(Items: TThItems) of object;
  TItemListPointvent = procedure(Items: TThItems; Distance: TPointF) of object;

  TThItem = class(TControl, IThItem)
  private
    FParentCanvas: IThCanvas;

    FOnSelected: TItemSelectedEvent;
    FOnUnselected: TItemEvent;
    FOnTracking: TTrackingEvent;
    FOnMove: TItemMoveEvent;
    FOnResize: TItemResizeEvent;

    procedure SetSelected(const Value: Boolean);
    procedure SetParentCanvas(const Value: IThCanvas);
  protected
    FHighlighter: IItemHighlighter;
    FResizer: IItemResizer;
    FBeforeSelect,
    FSelected: Boolean;
    FMouseDownPos,
    FDownItemPos: TPointF;

    function CreateHighlighter: IItemHighlighter; virtual;
    function CreateResizer: IItemResizer; virtual;

    function GetUpdateRect: TRectF; override;
    function GetItemRect: TRectF; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure DoSelected(Value: Boolean; IsMultiSelect: Boolean = False); virtual;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Painting; override;
    function PointInObject(X, Y: Single): Boolean; override;
    procedure ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;

    procedure DrawItemAtMouse(AFrom, ATo: TPointF); virtual;
    procedure RealignSpot;
    procedure ShowSpots;
    procedure ShowDisableSpots;

    property ParentCanvas: IThCanvas read FParentCanvas write SetParentCanvas;
    property Selected: Boolean read FSelected write SetSelected;

    property OnSelected: TItemSelectedEvent read FOnSelected write FOnSelected;
    property OnUnselected: TItemEvent read FOnUnselected write FOnUnselected;
    property OnTracking: TTrackingEvent read FOnTracking write FOnTracking;
    property OnMove: TItemMoveEvent read FOnMove write FOnMove;
    property OnResize: TItemResizeEvent read FOnResize write FOnResize;
  end;

  TThItemClass = class of TThItem;

implementation

uses
  ThConsts;

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
  FResizer := nil; // Interface Free(destory)

  inherited;
end;

procedure TThItem.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;

  // 크기 변경 요청 시 Spot위치 조정(SpotCorner로 크기 조정 시 요청됨)
//  if Assigned(FResizer) then
//    FResizer.RealignSpot;
end;

procedure TThItem.SetParentCanvas(const Value: IThCanvas);
begin
  FParentCanvas := Value;
  Parent := TFMXObject(Value);
end;

function TThItem.CreateHighlighter: IItemHighlighter;
begin
end;

function TThItem.CreateResizer: IItemResizer;
begin
end;

procedure TThItem.Painting;
begin
  inherited;

//  if (FSelected or IsMouseOver) and Assigned(FHighlighter) then
  if (IsMouseOver) and Assigned(FHighlighter) then
    FHighlighter.DrawHighlight;
end;

function TThItem.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;

  if IThCanvas(ParentCanvas).IsDrawingItem then
    Exit;
  P := AbsoluteToLocal(PointF(X, Y));

  Result := PtInItem(P);
end;

procedure TThItem.ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);
begin
//  BeforeRect.Offset(Position.Point);
  if Assigned(FOnResize) then
    FOnResize(Self, BeforeRect);
end;

procedure TThItem.DoMouseEnter;
begin
  inherited;

  Repaint;
end;

procedure TThItem.DoMouseLeave;
begin
  inherited;

  Repaint;
end;

procedure TThItem.DrawItemAtMouse(AFrom, ATo: TPointF);
begin
end;

function TThItem.GetItemRect: TRectF;
begin
  Result := LocalRect;
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

procedure TThItem.RealignSpot;
begin
  if Assigned(FResizer) then
    FResizer.RealignSpot;
end;

procedure TThItem.ShowDisableSpots;
begin
  if Assigned(FResizer) then
    FResizer.ShowDisableSpots;
end;

procedure TThItem.ShowSpots;
begin
  if Assigned(FResizer) then
    FResizer.ShowSpots;
end;

procedure TThItem.SetSelected(const Value: Boolean);
begin
  DoSelected(Value);
end;

procedure TThItem.DoSelected(Value: Boolean; IsMultiSelect: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if Value and Assigned(FOnSelected) then
    FOnSelected(Self, IsMultiSelect)
  else if (not Value) and Assigned(FOnUnselected) then
    FOnUnselected(Self);

  if Assigned(FResizer) then
  begin
    if FSelected and FParentCanvas.IsMultiSelected then
      FResizer.ShowDisableSpots
    else if {(not FParentCanvas.IsMultiSelected) and} Value then
      FResizer.ShowSpots
    else
      FResizer.HideSpots;
  end;

  // 선택시는 다시 그릴 필요 없음(ResizeSpot만 추가되기 때문)
  if not FSelected then
    Repaint;
end;

procedure TThItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    FBeforeSelect := FSelected;

    if ssShift in Shift then
      DoSelected(True, True)
    else if not FSelected then
      DoSelected(True);
    FMouseDownPos := PointF(X, Y);
    FDownItemPos := Position.Point;
  end;

  InvalidateRect(UpdateRect);
end;

procedure TThItem.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Gap: TPointF;
begin
  inherited;

  if FPressed then
  begin
    if FSelected and Assigned(FOnTracking) then
    begin
      Gap := PointF(X, Y).Subtract(FMouseDownPos);  // Down and Move Gap
      FOnTracking(Self, Gap.X, Gap.Y);
    end;
  end;
end;

procedure TThItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    if (ssShift in Shift) and (FDownItemPos = Position.Point) and FBeforeSelect then
      DoSelected(False, True)
    else if (FDownItemPos <> Position.Point) and Assigned(FOnMove) then
      FOnMove(Self, FDownItemPos);
  end;
end;

end.
