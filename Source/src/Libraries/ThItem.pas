unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts,
  FMX.Types, ThTypes, System.Generics.Collections;

type
  TThItem = class;
  TThItems = TList<TThItem>;

  TItemEvent = procedure(Item: TThItem) of object;
  TItemListEvent = procedure(Items: TThItems) of object;
  TItemSelectedEvent = procedure(Item: TThItem; IsAdded: Boolean) of object;
  TItemMoveEvent = procedure(Sender: TObject; X, Y: Single) of object;

  TThItem = class(TControl, IThItem)
  private
    FParentCanvas: IThCanvas;

    FOnSelected: TItemSelectedEvent;
    FOnUnselected: TItemEvent;
    FOnTracking: TTrackingEvent;

    procedure SetSelected(const Value: Boolean);
    procedure SetParentCanvas(const Value: IThCanvas);
  protected
    FHighlighter: IItemHighlighter;
    FResizer: IItemResizer;
    FBeforeSelect,
    FSelected: Boolean;
    FMouseDownPos: TPointF;

    function CreateHighlighter: IItemHighlighter; virtual;
    function CreateResizer: IItemResizer; virtual;

    function GetUpdateRect: TRectF; override;
    function GetItemRect: TRectF; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    procedure DoSelected(Value: Boolean; IsAdded: Boolean); virtual;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Painting; override;
    function PointInObject(X, Y: Single): Boolean; override;

    procedure DrawItemWithMouse(AFrom, ATo: TPointF); virtual;

    property ParentCanvas: IThCanvas read FParentCanvas write SetParentCanvas;
    property Selected: Boolean read FSelected write SetSelected;

    property OnSelected: TItemSelectedEvent read FOnSelected write FOnSelected;
    property OnUnselected: TItemEvent read FOnUnselected write FOnUnselected;
    property OnTracking: TTrackingEvent read FOnTracking write FOnTracking;
  end;

  TThItemClass = class of TThItem;

implementation

uses
  ThConsts, CommonUtils;

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

  if (FSelected or IsMouseOver) and Assigned(FHighlighter) then
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

procedure TThItem.DrawItemWithMouse(AFrom, ATo: TPointF);
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

procedure TThItem.SetSelected(const Value: Boolean);
begin
  if (not FParentCanvas.IsMultiSelected) and Value then
    FResizer.ShowSpots
  else
    FResizer.HideSpots;

  if FSelected = Value then
    Exit;

  FSelected := Value;

//  DoSelected(FSelected);

  // 선택시는 다시 그릴 필요 없음(ResizeSpot만 추가되기 때문)
  if not FSelected then
    Repaint;
end;

procedure TThItem.DoSelected(Value: Boolean; IsAdded: Boolean);
begin
  if FSelected = Value then
    Exit;

  if Value and Assigned(FOnSelected) then
    FOnSelected(Self, IsAdded)
  else if (not Value) and Assigned(FOnUnselected) then
    FOnUnselected(Self);
end;

procedure TThItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    FBeforeSelect := FSelected;
    DoSelected(True, ssShift in Shift);
    FMouseDownPos := PointF(X, Y);
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
    Gap := PointF(X, Y).Subtract(FMouseDownPos);  // Down and Move Gap

    if FSelected and Assigned(FOnTracking) then
      FOnTracking(Self, Gap.X, Gap.Y);

//    InvalidateRect(UpdateRect);     //  잔상을 지우기 위해 기존 영역을 다시 그린다.
  end;
end;

procedure TThItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Select: Boolean;
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    // Shift로 선택 해제는 마우스 이동하지 않은 경우만(이미 선택되었던 개체에 대해)
    if (ssShift in Shift) and (FMouseDownPos = PointF(X, Y)) and FBeforeSelect then
      Select := False
    else
      Select := True;
    DoSelected(Select, ssShift in Shift);

//    if FMouseDownPos <> PointF(X, Y) then

  end;
end;

end.
