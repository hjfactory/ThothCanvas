unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes, System.UIConsts, System.SysUtils,
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

  TThItemData = class(TInterfacedObject, IThItemData)
  private
    FInt: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

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
    FSelection: IItemSelection;
    FBeforeSelect,
    FSelected: Boolean;
    FMouseDownPos,
    FDownItemPos: TPointF;

    function CreateHighlighter: IItemHighlighter; virtual;
    function CreateSelection: IItemSelection; virtual;

    function GetUpdateRect: TRectF; override;
    function GetItemRect: TRectF; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;

    // Virtual method
    procedure DoSelected(Value: Boolean; IsMultiSelect: Boolean = False); virtual;

    function PtInItem(Pt: TPointF): Boolean; virtual; abstract;
  public
//    constructor Create(AOwner: TComponent; AItemData: TThItemData = nil); reintroduce; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetItemData(AItemData: IThItemData); virtual;

    procedure Painting; override;
    function PointInObject(X, Y: Single): Boolean; override;
    procedure ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);

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

  TThFileItemData = class(TThItemData)
  private
    FFilename: TFileName;
  public
    constructor Create(AFileName: TFileName);

    property Filename: TFileName read FFilename;
  end;

implementation

uses
  ThConsts;

{ TThItem }

//constructor TThItem.Create(AOwner: TComponent; AItemData: TThItemData);
constructor TThItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoCapture := True;

{$IFDEF ON_HIGHLIGHT}
  FHighlighter := CreateHighlighter;
{$ENDIF}
  FSelection := CreateSelection;

  Cursor := crSizeAll;
end;

destructor TThItem.Destroy;
begin
  FHighlighter := nil;
  FSelection := nil; // Interface Free(destory)

  inherited;
end;

procedure TThItem.SetItemData(AItemData: IThItemData);
begin
end;

procedure TThItem.SetParentCanvas(const Value: IThCanvas);
begin
  FParentCanvas := Value;
  Parent := TFMXObject(Value);
end;

function TThItem.CreateHighlighter: IItemHighlighter;
begin
end;

function TThItem.CreateSelection: IItemSelection;
begin
end;

procedure TThItem.Painting;
begin
  inherited;

//  if (FSelected or IsMouseOver) and Assigned(FHighlighter) then
//  if (IsMouseOver or (Assigned(FSelection) and (FSelection.IsMouseOver))) and Assigned(FHighlighter) then
{$IFDEF ON_HIGHLIGHT}
  if (IsMouseOver) and Assigned(FHighlighter) then
    FHighlighter.DrawHighlight;
{$ENDIF}

  if (IsMouseOver or Selected) and Assigned(FSelection) then
    FSelection.DrawSelection;
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
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result, ItemResizeSpotRadius, ItemResizeSpotRadius);
  InflateRect(Result, 1, 1);
end;

procedure TThItem.RealignSpot;
begin
  if Assigned(FSelection) then
    FSelection.RealignSpot;
end;

procedure TThItem.ShowDisableSpots;
begin
  if Assigned(FSelection) then
    FSelection.ShowDisableSpots;
end;

procedure TThItem.ShowSpots;
begin
  if Assigned(FSelection) then
    FSelection.ShowSpots;
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

  if Assigned(FSelection) then
  begin
    if FSelected and FParentCanvas.IsMultiSelected then
      FSelection.ShowDisableSpots
    else if {(not FParentCanvas.IsMultiSelected) and} Value then
      FSelection.ShowSpots
    else
      FSelection.HideSpots;
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

    if (ssShift in Shift) or (ssCtrl in Shift) then
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
    if ((ssShift in Shift) or (ssCtrl in Shift)) and (FDownItemPos = Position.Point) and FBeforeSelect then
      DoSelected(False, True)
    else if (FDownItemPos <> Position.Point) and Assigned(FOnMove) then
      FOnMove(Self, FDownItemPos);
  end;
end;

{ TThItemData }

constructor TThItemData.Create;
begin

end;

destructor TThItemData.Destroy;
begin
  if FInt > 0 then
  begin

  end;


  inherited;
end;

{ TThFileItem }

constructor TThFileItemData.Create(AFileName: TFileName);
begin
  FFilename := AFileName;
end;

end.
