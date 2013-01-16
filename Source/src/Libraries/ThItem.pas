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

  TThItem = class(TControl, IThItem, IThItemContainer)
  private
    FParentCanvas: IThCanvas;
    FSpotCount: Integer;

    FOnSelected: TItemSelectedEvent;
    FOnUnselected: TItemEvent;
    FOnTracking: TTrackingEvent;
    FOnMove: TItemMoveEvent;
    FOnResize: TItemResizeEvent;

    procedure SetSelected(const Value: Boolean);
    procedure SetParentCanvas(const Value: IThCanvas);
    function GetItem(Index: Integer): IThItem;
    function GetItemCount: Integer;

    function GetAbsolutePoint(APoint: TPointF): TPointF;
    function GetBeforeParent: TFmxObject;
    procedure SetBeforeParent(const Value: TFmxObject);
    function GetBeforendex: Integer;
    procedure SetBeforeIndex(const Value: Integer);
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

    function IsContain(AItem: IThItem): Boolean; virtual;
    procedure Contain(AItem: TThItem); virtual;
    procedure ReleaseContain; virtual;

    property BeforeParent: TFmxObject read GetBeforeParent write SetBeforeParent;
    property BeforeIndex: Integer read GetBeforendex write SetBeforeIndex;

    property ParentCanvas: IThCanvas read FParentCanvas write SetParentCanvas;
    property Selected: Boolean read FSelected write SetSelected;

    property OnSelected: TItemSelectedEvent read FOnSelected write FOnSelected;
    property OnUnselected: TItemEvent read FOnUnselected write FOnUnselected;
    property OnTracking: TTrackingEvent read FOnTracking write FOnTracking;
    property OnMove: TItemMoveEvent read FOnMove write FOnMove;
    property OnResize: TItemResizeEvent read FOnResize write FOnResize;

    property Items[Index: Integer]: IThItem read GetItem;
    property ItemCount: Integer read GetItemCount;
    function GetContainChildrenItems(AItem: IThItem; AChildren: IThItems): Boolean;
    function GetContainParentItem(AItem: IThItem): IThItem;
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

constructor TThItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoCapture := True;
{$IFDEF ON_HIGHLIGHT}
  FHighlighter := CreateHighlighter;
{$ENDIF}
  FSelection := CreateSelection;

  FSpotCount := 0;
  if Assigned(FSelection) then
    FSpotCount := FSelection.Count;

  Cursor := crSizeAll;
end;

destructor TThItem.Destroy;
begin
  FHighlighter := nil;
  FSelection := nil; // Interface Free(destory)

  inherited;
end;

function TThItem.CreateHighlighter: IItemHighlighter;
begin
end;

function TThItem.CreateSelection: IItemSelection;
begin
end;

//////////////////////////////////////
// Contain functions
function TThItem.IsContain(AItem: IThItem): Boolean;
begin
  Result := False;
end;

procedure TThItem.Contain(AItem: TThItem);
var
  AbsoluteP: TPointF;
begin
  AbsoluteP := GetAbsolutePoint(PointF(0, 0));

  AItem.Position.Point := AItem.GetAbsolutePoint(PointF(0, 0)).Subtract(AbsoluteP);
  AItem.BeforeParent := AItem.Parent;
  AItem.Parent := Self;
end;

procedure TThItem.ReleaseContain;
var
  P: TThitem;
begin
  P := TThItem(Parent);
  Parent := BeforeParent;
  if Assigned(P) then
    Position.Point := Position.Point.Add(P.Position.Point);
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

function TThItem.GetAbsolutePoint(APoint: TPointF): TPointF;
begin
  Result := APoint.Add(Position.Point);
  if Parent is TThItem then
    Result := TThItem(Parent).GetAbsolutePoint(Result);
end;

function TThItem.GetContainChildrenItems(AItem: IThItem;
  AChildren: IThItems): Boolean;
var
  I: Integer;
begin
  Result := AbsoluteRect.IntersectsWith(TThItem(AItem).AbsoluteRect);
  if not Result then
    Exit;

  for I := 0 to ItemCount - 1 do
  begin
    if AItem = Items[I] then
      Continue;

    if TThItem(AItem).IsContain(Items[I]) then
      AChildren.Add(Items[I])
    ;
  end;

  Result := AChildren.Count > 0;
end;

function TThItem.GetContainParentItem(AItem: IThItem): IThItem;
var
  I: Integer;
  Item: TThItem;
begin
  Result := nil;
  if IsContain(TThItem(AItem)) then
  begin
    for I := ItemCount - 1 downto 0 do
    begin
      Item := TThItem(Items[I]);
      if Item = TThItem(AItem) then
        Continue;

      Result := Item.GetContainParentItem(AItem);
      if Assigned(Result) then
        Exit;
    end;
    Result := Self;
  end;
end;

function TThItem.GetItem(Index: Integer): IThItem;
begin
  Result := nil;
  if ChildrenCount > (Index + FSpotCount) then
    Result := TThItem(FChildren[Index + FSpotCount]);
end;

function TThItem.GetItemCount: Integer;
begin

  Result := ChildrenCount - FSpotCount;
end;

function TThItem.GetItemRect: TRectF;
begin
  Result := LocalRect;
end;

///////////////////////////////////////
// Setter & Getter
procedure TThItem.SetItemData(AItemData: IThItemData);
begin
end;

procedure TThItem.SetBeforeIndex(const Value: Integer);
begin
  Tag := Value;
end;

procedure TThItem.SetBeforeParent(const Value: TFmxObject);
begin
  TagObject := Value;
end;

procedure TThItem.SetParentCanvas(const Value: IThCanvas);
begin
  FParentCanvas := Value;
  Parent := TFMXObject(Value);
end;

function TThItem.GetBeforendex: Integer;
begin
  Result := Tag;
end;

function TThItem.GetBeforeParent: TFmxObject;
begin
  Result := TFmxObject(TagObject);
end;
// Setter & Getter
///////////////////////////////////////

function TThItem.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result, ItemResizeSpotRadius, ItemResizeSpotRadius);
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
  Distance: TPointF;
begin
  inherited;

  if FPressed then
  begin
    if FSelected and Assigned(FOnTracking) then
    begin
      Distance := PointF(X, Y).Subtract(FMouseDownPos);  // Down and Move Gap
      FOnTracking(Self, Distance.X, Distance.Y);
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
