unit ThCanvas;

interface

uses
  System.UITypes, System.Classes, System.Types,
  ThInterface, ThTypes, ThLayout, ThShape;

type
  TThCanvas = class(TThContainer, IThCanvas, IThObserver)
  private
    FSubject: IThSubject;

    FDrawShape: TThShape;
    FCurrentShapeClass: TThSahpeClass;
    FSelectionShapes: TList;

    procedure ShapeMove(Sender: TObject; const AStartPos: TPointF);
    procedure ShapeSelect(Sender: TObject);

    // 사용자 요청
    procedure ReqInsertShape;
    procedure ReqMoveShapes(AShapes: TList; FromPos, ToPos: TPointF);
    procedure ReqDeleteShapes(AShapes: TList);

    // 실제 처리
    procedure InsertShape(AShape: TThShape);
    procedure InsertShapes(AShapes: TList);
    procedure MoveShapes(AShapes: TList; const ToPos: TPointF);
    procedure DeleteShapes(AShapes: TList);
    procedure ClearSelection;
  protected
    procedure BlankClick; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure DeleteSelection;

    property CurrentShapeClass: TThSahpeClass read FCurrentShapeClass write FCurrentShapeClass;

    // OnSelectShape;
    // OnTracking
    // OnClick
    // OnMoveShape
    // OnDeleteShape
    // OnInsertShape
  end;


implementation

uses
  FMX.Platform,
  ThCommand;

{ TThCanvas }

procedure TThCanvas.BlankClick;
begin
  ClearSelection;
end;

procedure TThCanvas.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to FSelectionShapes.Count - 1 do
    TThShape(FSelectionShapes[I]).Selected := False;
  FSelectionShapes.Clear;
end;

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FSelectionShapes := TList.Create;
end;

destructor TThCanvas.Destroy;
begin
  FSelectionShapes.Free;

  inherited;
end;

procedure TThCanvas.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;

  ASubject.RegistObserver(Self);
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  Debug('TThCanvas - ' + TThShapeCommand(ACommand).ClassName);

  if ACommand is TThInsertShapeCommand then
    // Canvas에 추가
    InsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThDeleteShapeCommand then
    // Canvas에서 제거
    DeleteShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRestoreShapeCommand then
    // Canvas에 InsertObject
    InsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRemoveShapeCommand then
    // Canvas에서 제거(있으면)
  else if ACommand is TThMoveShapeCommand then
    MoveShapes(TThShapeCommand(ACommand).List, TThMoveShapeCommand(ACommand).ToPos)
  ;
end;

procedure TThCanvas.InsertShape(AShape: TThShape);
begin
  AShape.Parent := Self;
end;

procedure TThCanvas.InsertShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    InsertShape(AShapes[I]);
end;

procedure TThCanvas.DeleteShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    TThShape(AShapes[I]).Parent := nil;
end;

procedure TThCanvas.MoveShapes(AShapes: TList; const ToPos: TPointF);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    TThShape(AShapes[I]).Position.Point := ToPos;
end;

procedure TThCanvas.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;

  // MultiSelect 지원을 위해 Ctrl 및 Shift SaveState 필요
end;

procedure TThCanvas.ReqInsertShape;
begin
  FSubject.Subject(Self, TThInsertShapeCommand.Create(FDrawShape));

  FCurrentShapeClass := nil;
  FDrawShape := nil;
end;

procedure TThCanvas.ReqDeleteShapes(AShapes: TList);
begin
  FSubject.Subject(Self, TThDeleteShapeCommand.Create(AShapes));
end;

procedure TThCanvas.ReqMoveShapes(AShapes: TList; FromPos, ToPos: TPointF);
begin
  FSubject.Subject(Self, TThMoveShapeCommand.Create(FSelectionShapes, FromPos, ToPos));
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  if Assigned(FCurrentShapeClass) then
  begin
    P := FCurrentPos;
    P.X := P.X - OffsetPos.X;
    P.Y := P.Y - OffsetPos.Y;

//    Debug('OffsetPos(%f, %f, %f, %f)', [OffsetPos.X, OffsetPos.Y, P.X, P.Y]);

    FDrawShape := FCurrentShapeClass.Create(nil);
    FDrawShape.Parent := Self;
    FDrawShape.Position.Point := ScalePoint(P, 1/FContentScale, 1/FContentScale);
    FDrawShape.OnMove := ShapeMove;
    FDrawShape.OnSelect := ShapeSelect;
//    FDrawShape.OnTrack := ShapeTrack;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FCurrentShapeClass) and Assigned(FDrawShape) then
  begin
    FDrawShape.Width := (X - FCurrentPos.X) / FContentScale;
    FDrawShape.Height := (Y - FCurrentPos.Y) / FContentScale;
  end
  else
    inherited;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Assigned(FDrawShape) then
    ReqInsertShape;
end;

procedure TThCanvas.ShapeMove(Sender: TObject; const AStartPos: TPointF);
begin
  ReqMoveShapes(FSelectionShapes, AStartPos, TThShape(Sender).Position.Point);
end;

procedure TThCanvas.ShapeSelect(Sender: TObject);
begin
  ClearSelection;

  FSelectionShapes.Add(Sender);
end;

procedure TThCanvas.DeleteSelection;
begin
  ReqDeleteShapes(FSelectionShapes);
  DeleteShapes(FSelectionShapes);
end;

end.
