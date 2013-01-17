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

  IThItemContainer = interface
    function GetItem(Index: Integer): TThItem;
    function GetItemCount: Integer;
    property Items[Index: Integer]: TThItem read GetItem;
    property ItemCount: Integer read GetItemCount;

    function FindParent(AItem: TThItem): TFMXObject;
    procedure ContainChildren(AItem: TThItem);

    procedure DoAddObject(AObject: TFmxObject);
    procedure DoRemoveObject(AObject: TFmxObject);
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

    // 최종 포함된 자식 목록(Undo시 필요)
    FLastContainItems: TThItems;

    procedure SetSelected(const Value: Boolean);
    procedure SetParentCanvas(const Value: IThCanvas);
    function GetBeforendex: Integer;
    function GetBeforeParent: TFmxObject;
    procedure SetBeforeIndex(const Value: Integer);
    procedure SetBeforeParent(const Value: TFmxObject);
    function GetAbsolutePoint: TPointF; overload;
    function GetAbsolutePoint(APoint: TPointF): TPointF; overload;
    function GetItem(Index: Integer): TThItem;
    function GetItemCount: Integer;
  protected
    FHighlighter: IItemHighlighter;
    FSelection: IItemSelection;
    FBeforeSelect,
    FSelected: Boolean;
    FMouseDownPos,
    FDownItemPos: TPointF;

    procedure DoAddObject(AObject: TFmxObject); override;
    procedure DoRemoveObject(AObject: TFmxObject); override;

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

    property Items[Index: Integer]: TThItem read GetItem;
    property ItemCount: Integer read GetItemCount;

    procedure SetItemData(AItemData: IThItemData); virtual;

    procedure Painting; override;
    function PointInObject(X, Y: Single): Boolean; override;
    procedure ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);

    procedure DrawItemAtMouse(AFrom, ATo: TPointF); virtual;
    procedure RealignSpot;
    procedure ShowSpots;
    procedure ShowDisableSpots;

    function FindParent(AItem: TThItem): TFMXObject;
    procedure ContainChildren(AItem: TThItem);

    function IsContain(AChild: TThItem): Boolean; virtual;
    procedure Contain(AItem: TThItem); virtual;
    procedure ReleaseContain; virtual;


    property ParentCanvas: IThCanvas read FParentCanvas write SetParentCanvas;
    property Selected: Boolean read FSelected write SetSelected;

    property BeforeParent: TFmxObject read GetBeforeParent write SetBeforeParent;
    property BeforeIndex: Integer read GetBeforendex write SetBeforeIndex;
    property LastContainItems: TThItems read FLastContainItems;

    property AbsolutePoint: TPointF read GetAbsolutePoint;

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
procedure TThItem.Contain(AItem: TThItem);
var
  AbsoluteP: TPointF;
begin
  AbsoluteP := GetAbsolutePoint;

  AItem.Position.Point := AItem.AbsolutePoint.Subtract(AbsoluteP);
  AItem.BeforeParent := AItem.Parent;
  AItem.Parent := Self;end;

procedure TThItem.ContainChildren(AItem: TThItem);
begin

end;

constructor TThItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Cursor := crSizeAll;
  AutoCapture := True;

  FLastContainItems := TList<TThItem>.Create;

{$IFDEF ON_HIGHLIGHT}
  FHighlighter := CreateHighlighter;
{$ENDIF}
  FSelection := CreateSelection;

  FSpotCount := 0;
  if Assigned(FSelection) then
    FSpotCount := FSelection.Count;
end;

destructor TThItem.Destroy;
begin
  FHighlighter := nil;
  FSelection := nil; // Interface Free(destory)

  FLastContainItems.Free;

  inherited;
end;

procedure TThItem.SetBeforeIndex(const Value: Integer);
begin

end;

procedure TThItem.SetBeforeParent(const Value: TFmxObject);
begin

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

function TThItem.IsContain(AChild: TThItem): Boolean;
begin
  Result := False;
end;

procedure TThItem.ItemResizeBySpot(Sender: TObject; BeforeRect: TRectF);
begin
  if Assigned(FOnResize) then
    FOnResize(Self, BeforeRect);
end;

procedure TThItem.DoAddObject(AObject: TFmxObject);
//  AObject.Parent = nil 임
var
  Item: TThItem;
begin
  if not (AObject is TThItem) then
    Exit;

  Item := TThItem(AObject);
  Item.Position.Point := Item.AbsolutePoint.Subtract(Position.Point);

  inherited;
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

procedure TThItem.DoRemoveObject(AObject: TFmxObject);
var
  Item: TThItem;
begin
  if (AObject is TThItem) then
  begin
    Item := TThItem(AObject);
    Item.BeforeParent := Self;
    Item.BeforeIndex := AObject.Index;
  end;

  // 초기화 전에 하장
  inherited;
end;

procedure TThItem.DrawItemAtMouse(AFrom, ATo: TPointF);
begin
end;

function TThItem.FindParent(AItem: TThItem): TFMXObject;
var
  I: Integer;
  Item: TThItem;
begin
  Result := nil;
  for I := ItemCount - 1 downto 0 do
  begin
    Item := Items[I];
    if Item = AItem then
      Continue;

    Result := Item.FindParent(AItem);
    if Assigned(Result) then
      Exit;
  end;
end;

function TThItem.GetAbsolutePoint: TPointF;
begin
  Result := GetAbsolutePoint(PointF(0, 0));
end;

function TThItem.GetAbsolutePoint(APoint: TPointF): TPointF;
begin
  Result := APoint.Add(Position.Point);
  if Parent is TThItem then
    Result := TThItem(Parent).GetAbsolutePoint(Result);end;

function TThItem.GetBeforendex: Integer;
begin
  Result := Tag;
end;

function TThItem.GetBeforeParent: TFmxObject;
begin
  Result := TFMXObject(TagObject);
end;

function TThItem.GetItem(Index: Integer): TThItem;
begin
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

procedure TThItem.ReleaseContain;
var
  CurrParent: TFMXObject;
begin
  CurrParent := Parent;
  Parent := BeforeParent;
  BeforeParent := CurrParent;
  if Assigned(CurrParent) then
    Position.Point := Position.Point.Add(TControl(CurrParent).Position.Point);
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
