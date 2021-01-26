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

  ThTypes, ThClasses, ThAttributes,
  ThDrawStyle, ThDrawItem;

type
  TThDrawObject = class;
  TThDrawObjectClass = class of TThDrawObject;

  TThDrawObject = class(TThInterfacedObject, IThDrawObject)
  private
    FDrawStyle: IThDrawStyle;
  protected
    FDrawItem: IThDrawItem;
  public
    constructor Create(AStyle: IThDrawStyle); virtual;
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); virtual;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); virtual;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); virtual; abstract;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); virtual;

    function GetDrawItem: IThDrawItem; virtual;

    property DrawStyle: IThDrawStyle read FDrawStyle write FDrawStyle;
  end;

  /// Brush Objects
  TThBrushDrawObject = class(TThDrawObject)
  public
    destructor Destroy; override;
  end;

  [DrawObjAttr(100, 'Pen', TThPenStyle, TThPenDrawItem)]
  TThPenDrawObject = class(TThBrushDrawObject)
  private
    FPath: TList<TFloatPoint>;
    FPolyPolyPath: TPaths;
    FPolyPoly: TArrayOfArrayOfFloatPoint;
    function GetPenStyle: TThPenStyle;

    property PenStyle: TThPenStyle read GetPenStyle;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint); override;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;
    function GetDrawItem: IThDrawItem; override;
  end;

  TThEraserDrawObject = class(TThBrushDrawObject)
  private
    FDrawItems: TThDrawItems;
    function GetDrawStyle: TThEraserStyle;

    property DrawStyle: TThEraserStyle read GetDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle; AItems: TThDrawItems); overload;
  end;

  [DrawObjAttr(110, 'Eraser')]
  TThObjErsDrawObject = class(TThEraserDrawObject)
  public
    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;
  end;

  /// Shape Objects
  TThShapeDrawObject = class(TThDrawObject)
  protected
    FDownPt, FCurrPt: TFloatPoint;
    FLastObject: TThDrawItem;

    function GetDrawStyle: TThShapeStyle;
    property DrawStyle: TThShapeStyle read GetDrawStyle;
  public
    constructor Create(AStyle: IThDrawStyle); override;
    destructor Destroy; override;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;

    property LastObject: TThDrawItem read FLastObject;
  end;

  // Shape Select
  [DrawObjAttr(200, 'Select')]
  TThSelectObject = class(TThDrawObject)
  private
    FLastPt: TFloatPoint;
    FDrawItems: TThDrawItems;
    FSelected: TThShapeDrawItem;
    FSelectItems: TThShapeDrawItems;

    function SetSelection(AItem: TThShapeDrawItem; AShift: TShiftState): TThShapeDrawItem;
    procedure SetSelected(const Value: TThShapeDrawItem);
  public
    constructor Create(AItems: TThDrawItems); overload;
    destructor Destroy; override;

    procedure Start(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Move(const APoint: TFloatPoint; AShift: TShiftState); override;
    procedure Done(const APoint: TFloatPoint; AShift: TShiftState); override;

    property Items: TThShapeDrawItems read FSelectItems;
    property Selected: TThShapeDrawItem read FSelected write SetSelected;
    procedure DeleteSelectedItems;

    procedure SetDrawItems(ADrawItems: TThDrawItems);

    procedure ClearSelection;
  end;

implementation

uses
  ThUtils, ThDrawObjectManager;

procedure RegistDrawObjectManager;
begin
  // Freedraw
  DOMgr.RegistDrawObj(TThPenDrawObject);
  DOMgr.RegistDrawObj(TThObjErsDrawObject);

  // Shape
  DOMgr.RegistDrawObj(TThSelectObject);
end;

{ TThDrawObject }

constructor TThDrawObject.Create(AStyle: IThDrawStyle);
begin
  FDrawStyle := AStyle;
end;

procedure TThDrawObject.Start;
begin
end;

procedure TThDrawObject.Done;
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

constructor TThPenDrawObject.Create(AStyle: IThDrawStyle);
begin
  if not Assigned(AStyle) then
    AStyle := TThPenStyle.Create;

  inherited Create(AStyle);

  FPath := TList<TFloatPoint>.Create;
end;

destructor TThPenDrawObject.Destroy;
begin
  FPath.Free;

  inherited;
end;

procedure TThPenDrawObject.Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
var
  Color: TColor32;
  PolyPoly: TThPolyPoly;
begin
  if Assigned(FDrawItem) then
    FDrawItem.Draw(Bitmap, AScale, AOffset);
end;

procedure TThPenDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
begin
  FPath.Add(APoint);

  FDrawItem := TThPenDrawItem.Create(FDrawStyle);
end;

procedure TThPenDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
var
  Poly: TThPoly;
  PolyPath: TPath;
  LastP: TFloatPoint; // Adjusted point
begin
  FPath.Add(APoint);
  FDrawItem.AddPoint(APoint);
end;

procedure TThPenDrawObject.Done;
begin
  inherited;

  FPath.Clear;
  FPolyPolyPath := nil;
  FPolyPoly := nil;

  FDrawItem := nil;
end;

function TThPenDrawObject.GetDrawItem: IThDrawItem;
begin
  Result := FDrawItem;
end;

function TThPenDrawObject.GetPenStyle: TThPenStyle;
begin
  Result := TThPenStyle(FDrawStyle);
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

function TThEraserDrawObject.GetDrawStyle: TThEraserStyle;
begin
  Result := TThEraserStyle(FDrawStyle);
end;

{ TThObjErsDrawObject }

procedure TThObjErsDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
begin
  Move(APoint, AShift);
end;

procedure TThObjErsDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
var
  I: Integer;
  Poly: TThPoly;
  LDrawItems: TArray<TThDrawItem>;
begin
  Poly := Circle(APoint, DrawStyle.Thickness / 2);
  LDrawItems := FDrawItems.PolyInItems(Poly);
  for I := 0 to Length(LDrawItems) - 1 do
    TThPenDrawItem(LDrawItems[I]).IsDeletion := True;
end;

procedure TThObjErsDrawObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
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

procedure TThShapeDrawObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FLastObject := nil;
  FDownPt := APoint;
end;

procedure TThShapeDrawObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
begin
  FCurrPt := APoint;
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

procedure TThShapeDrawObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

  FCurrPt := EmptyPoint;
end;

function TThShapeDrawObject.GetDrawStyle: TThShapeStyle;
begin
  Result := TThShapeStyle(FDrawStyle);
end;

{ TThShapeSelectObject }

procedure TThSelectObject.ClearSelection;
var
  Item: TThShapeDrawItem;
begin
  for Item in FSelectItems do
    Item.IsSelection := False;
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

procedure TThSelectObject.SetSelected(const Value: TThShapeDrawItem);
begin
  ClearSelection;
  FSelected := Value;
  Value.IsSelection := True;
  FSelectItems.Add(Value);
end;

function TThSelectObject.SetSelection(AItem: TThShapeDrawItem; AShift: TShiftState): TThShapeDrawItem;
begin
  Result := AItem;
  if Assigned(AItem) then
  begin
    // 선택되지 않은 경우 선택 처리(이미 선택된 경우 무시)
    if AItem.IsSelection then
    begin
      if ssShift in AShift then
      begin
        AItem.IsSelection := False;
        FSelectItems.Remove(AItem);
        Result := nil;
      end;
    end
    else
    begin
      if not (ssShift in AShift)  then
        ClearSelection;

      AItem.IsSelection := True;
      FSelectItems.Add(AItem);
    end;
  end
  else
  begin
    if not (ssShift in AShift) then
      ClearSelection;
  end;
end;

procedure TThSelectObject.Start(const APoint: TFloatPoint; AShift: TShiftState);
var
  Item: TThDrawItem;
begin
  FLastPt := APoint;
  Item := FDrawItems.PtInItem(APoint);

  FSelected := SetSelection(Item as TThShapeDrawItem, AShift);
end;

procedure TThSelectObject.Move(const APoint: TFloatPoint; AShift: TShiftState);
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

procedure TThSelectObject.Done(const APoint: TFloatPoint; AShift: TShiftState);
begin
  inherited;

end;

initialization
  RegistDrawObjectManager;
finalization

end.
