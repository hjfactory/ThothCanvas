{
  Role
    Draw shapes with the mouse actions.
}

unit ThDrawObject;

interface

uses
  System.Classes,
  System.Generics.Collections,

  GR32, GR32_Polygons, GR32_VectorUtils,
  clipper,

  ThTypes, ThClasses, ThDrawStyle,
  ThDrawItem, ThShapeItem;

type
  TThDrawObject = class;
  TThDrawObjectClass = class of TThDrawObject;

  TThDrawObject = class(TThInterfacedObject, IThDrawObject)
  private
    FDrawStyle: IThDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); virtual; abstract;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); virtual;

    function GetDrawItem: IThDrawItem; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  /// Brush Objects
  TThBrushDrawObject = class(TThDrawObject)
  public
    destructor Destroy; override;
  end;

  TThPenDrawObject = class(TThBrushDrawObject)
  private
    FDrawItem: TThPenDrawItem;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;
  end;

  TThEraserDrawObject = class(TThBrushDrawObject)
  private
    FDrawItems: TThDrawItems;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  protected
    FPos: TFloatPoint;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThDrawItems); reintroduce;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;
  end;

  TThObjErsDrawObject = class(TThEraserDrawObject)
  public
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;
  end;

  /// Shape Objects
  TThShapeDrawObject = class(TThDrawObject)
  private
    FShapeId: string;
    FDrawItem: TThShapeItem;
  protected
    FDownPt, FCurrPt: TFloatPoint;

    function GetDrawStyle: TThShapeStyle;
    property DrawStyle: TThShapeStyle read GetDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    function GetDrawItem: IThDrawItem; override;

    property ShapeId: string read FShapeId write FShapeId;
  end;

  // Shape Select
  TThSelectObject = class(TThDrawObject)
  private
    FLastPt: TFloatPoint;
    FDrawItems: TThDrawItems;
    FSelected: TThShapeItem;
    FSelectItems: TThShapeDrawItems;

    function SetSelection(AItem: TThShapeItem; AShift: TShiftState): TThShapeItem;
    procedure SetSelected(const Value: TThShapeItem);
  public
    constructor Create(AItems: TThDrawItems); reintroduce;
    destructor Destroy; override;

    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState); override;

    property Items: TThShapeDrawItems read FSelectItems;
    property Selected: TThShapeItem read FSelected write SetSelected;
    procedure DeleteSelectedItems;

    procedure SetDrawItems(ADrawItems: TThDrawItems);

    procedure ClearSelection;
  end;

implementation

uses
  ThUtils, System.Math;

{ TThDrawObject }

constructor TThDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

procedure TThDrawObject.MouseDown;
begin
end;

procedure TThDrawObject.MouseUp;
begin
end;

function TThDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := nil;
end;

procedure TThDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
end;

{ TThPenObject }

procedure TThPenDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FDrawItem := TThPenDrawItem.Create(FDrawStyle);
  FDrawItem.Start(APoint);
end;

procedure TThPenDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FDrawItem.Move(APoint);
end;

procedure TThPenDrawObject.MouseUp;
begin
  inherited;

  FDrawItem := nil;
end;

procedure TThPenDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
begin
  if Assigned(FDrawItem) then
    FDrawItem.Draw(Bitmap, AScale, AOffset);
end;

function TThPenDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := FDrawItem;
  FDrawItem := nil;
end;

destructor TThBrushDrawObject.Destroy;
begin

  inherited;
end;

{ TThEraserDrawObject }

constructor TThEraserDrawObject.Create(AStyle: IThDrawStyle;
  AItems: TThDrawItems);
begin
  if not Assigned(AStyle) then
    AStyle := TThEraserStyle.Create;
  inherited Create(AStyle);

  FDrawItems := AItems;
end;

procedure TThEraserDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
var
  Poly: TThPoly;
begin
  Poly := Circle(FPos, DrawStyle.Thickness / 2);
  PolylineFS(Bitmap, Poly, clBlack32, True);
end;

function TThEraserDrawObject.GetDrawStyle: TThEraserStyle;
begin
  Result := TThEraserStyle(FDrawStyle);
end;

{ TThObjErsDrawObject }

procedure TThObjErsDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FPos := APoint;
  MouseMove(APoint, AShift);
end;

procedure TThObjErsDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Poly: TThPoly;
  LDrawItems: TArray<TThDrawItem>;
begin
  FPos := APoint;
  Poly := Circle(APoint, DrawStyle.Thickness / 2);
  LDrawItems := FDrawItems.PolyInItems(Poly);
  for I := 0 to Length(LDrawItems) - 1 do
    TThPenDrawItem(LDrawItems[I]).IsDeletion := True;
end;

procedure TThObjErsDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Item: TThDrawItem;
begin
  for I := FDrawItems.Count - 1 downto 0 do
  begin
    Item := FDrawItems[I];
    if TThPenDrawItem(Item).IsDeletion then
      FDrawItems.Delete(I);
  end;
end;

{ TThShapeDrawObject }

procedure TThShapeDrawObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FDrawItem := TThShapeItemFactory.GetShapeItem(FShapeId, FDrawStyle);

  if not Assigned(FDrawItem) then
    Exit;
  FDrawItem.Start(APoint);
  FDownPt := APoint;
end;

procedure TThShapeDrawObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
begin
  if not Assigned(FDrawItem) then
    Exit;
  FCurrPt := APoint;
  FDrawItem.Move(APoint);
end;

constructor TThShapeDrawObject.Create(AStyle: IThDrawStyle);
begin
  if not Assigned(AStyle) then
    AStyle := TThShapeStyle.Create;
  inherited Create(AStyle);
end;

destructor TThShapeDrawObject.Destroy;
begin
  inherited;
end;

procedure TThShapeDrawObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FCurrPt := EmptyPoint;
end;

procedure TThShapeDrawObject.Draw(Bitmap: TBitmap32; AScale,
  AOffset: TFloatPoint);
begin
  if Assigned(FDrawItem) then
    FDrawItem.Draw(Bitmap, AScale, AOffset);
end;

function TThShapeDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := FDrawItem;
  FDrawItem := nil;
end;

function TThShapeDrawObject.GetDrawStyle: TThShapeStyle;
begin
  Result := TThShapeStyle(FDrawStyle);
end;

{ TThShapeSelectObject }

procedure TThSelectObject.ClearSelection;
var
  Item: TThShapeItem;
begin
  for Item in FSelectItems do
    Item.Selected := False;
  FSelectItems.Clear;
end;

constructor TThSelectObject.Create(AItems: TThDrawItems);
begin
  FDrawItems := AItems;
  FSelectItems := TThShapeDrawItems.Create;
end;

procedure TThSelectObject.DeleteSelectedItems;
var
  Item: TThDrawItem;
begin
  for Item in FSelectItems do
    FDrawItems.Remove(Item);
  FSelectItems.Clear;
end;

destructor TThSelectObject.Destroy;
begin
  FSelectItems.Free;

  inherited;
end;

procedure TThSelectObject.SetDrawItems(ADrawItems: TThDrawItems);
begin
  FDrawItems := ADrawItems;
  FSelectItems := TThShapeDrawItems.Create;
end;

procedure TThSelectObject.SetSelected(const Value: TThShapeItem);
begin
  ClearSelection;
  FSelected := Value;
  Value.Selected := True;
  FSelectItems.Add(Value);
end;

function TThSelectObject.SetSelection(AItem: TThShapeItem; AShift: TShiftState): TThShapeItem;
begin
  Result := AItem;
  if Assigned(AItem) then
  begin
    // 선택되지 않은 경우 선택 처리(이미 선택된 경우 무시)
    if AItem.Selected then
    begin
      if ssShift in AShift then
      begin
        AItem.Selected := False;
        FSelectItems.Remove(AItem);
        Result := nil;
      end;
    end
    else
    begin
      if not (ssShift in AShift)  then
        ClearSelection;

      AItem.Selected := True;
      FSelectItems.Add(AItem);
    end;
  end
  else
  begin
    if not (ssShift in AShift) then
      ClearSelection;
  end;
end;

procedure TThSelectObject.MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
var
  Item: TThDrawItem;
begin
  FLastPt := APoint;
  Item := FDrawItems.PtInItem(APoint);

  FSelected := SetSelection(Item as TThShapeItem, AShift);
end;

procedure TThSelectObject.MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
var
  P: TFloatPoint;
begin
  if Assigned(FSelected) then
  begin
    P := APoint - FLastPt;
    FSelectItems.Move(P);
    FLastPt := APoint;
  end;
end;

procedure TThSelectObject.MouseUp(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

end;

end.
