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
    FShapeClass: TThSahpeClass;
    FSelectionShapes: TList;

    procedure ShapeMove(Sender: TObject; const AStartPos: TPointF);
    procedure ShapeSelect(Sender: TObject);

    // 사용자 요청
    procedure InsertShape;
    procedure MoveShapes(AShapes: TList; FromPos, ToPos: TPointF);
    procedure DeleteShapes(AShapes: TList);

    // 실제 처리
    procedure DoInsertShape(AShape: TThShape);
    procedure DoInsertShapes(AShapes: TList);
    procedure DoMoveShapes(AShapes: TList; const ToPos: TPointF);
    procedure DoDeleteShapes(AShapes: TList);
  protected
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

    property ShapeClass: TThSahpeClass read FShapeClass write FShapeClass;
  end;


implementation

uses
  FMX.Platform,
  ThCommand;

{ TThCanvas }

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
    DoInsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThDeleteShapeCommand then
    // Canvas에서 제거
    DoDeleteShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRestoreShapeCommand then
    // Canvas에 InsertObject
    DoInsertShapes(TThShapeCommand(ACommand).List)
  else if ACommand is TThRemoveShapeCommand then
    // Canvas에서 제거(있으면)
  else if ACommand is TThMoveShapeCommand then
    DoMoveShapes(TThShapeCommand(ACommand).List, TThMoveShapeCommand(ACommand).ToPos)
  ;
end;

procedure TThCanvas.DoInsertShape(AShape: TThShape);
begin
  AShape.Parent := Self;
end;

procedure TThCanvas.DoInsertShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    DoInsertShape(AShapes[I]);
end;

procedure TThCanvas.DoDeleteShapes(AShapes: TList);
var
  I: Integer;
begin
  for I := 0 to AShapes.Count - 1 do
    TThShape(AShapes[I]).Parent := nil;
end;

procedure TThCanvas.DoMoveShapes(AShapes: TList; const ToPos: TPointF);
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


end;

procedure TThCanvas.InsertShape;
begin
  FSubject.Subject(Self, TThInsertShapeCommand.Create(FDrawShape));

  FShapeClass := nil;
  FDrawShape := nil;
end;

procedure TThCanvas.DeleteShapes(AShapes: TList);
begin
  FSubject.Subject(Self, TThDeleteShapeCommand.Create(AShapes));
end;

procedure TThCanvas.MoveShapes(AShapes: TList; FromPos, ToPos: TPointF);
begin
  FSubject.Subject(Self, TThMoveShapeCommand.Create(FSelectionShapes, FromPos, ToPos));
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPointF;
begin
  inherited;

  if Assigned(FShapeClass) then
  begin
    P := FCurrentPos;
    P.X := P.X - OffsetPos.X;
    P.Y := P.Y - OffsetPos.Y;

//    Debug('OffsetPos(%f, %f, %f, %f)', [OffsetPos.X, OffsetPos.Y, P.X, P.Y]);

    FDrawShape := FShapeClass.Create(nil);
    FDrawShape.Parent := Self;
    FDrawShape.Position.Point := ScalePoint(P, 1/FContentScale, 1/FContentScale);
    FDrawShape.OnMove := ShapeMove;
    FDrawShape.OnSelect := ShapeSelect;
//    FDrawShape.OnTrack := ShapeTrack;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
//    inherited;

  if Assigned(FShapeClass) and Assigned(FDrawShape) then
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

  InsertShape;
end;

procedure TThCanvas.ShapeMove(Sender: TObject; const AStartPos: TPointF);
begin
  MoveShapes(FSelectionShapes, AStartPos, TThShape(Sender).Position.Point);
end;

procedure TThCanvas.ShapeSelect(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FSelectionShapes.Count - 1 do
    TThShape(FSelectionShapes[I]).Selected := False;

  FSelectionShapes.Clear;
  FSelectionShapes.Add(Sender);
end;

procedure TThCanvas.DeleteSelection;
begin
  DeleteShapes(FSelectionShapes);
  DoDeleteShapes(FSelectionShapes);
end;

end.
