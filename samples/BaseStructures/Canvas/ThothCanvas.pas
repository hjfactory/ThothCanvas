unit ThothCanvas;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Layouts,
  ThothTypes, ThothObjects, ThothCommands, ObjectManager;

type
  TThDrawMode = (dmSelect, dmMove, dmDraw, dmDrawing);

///////////////////////////////////////////////////////
// Canvas
  TThCanvas = class(TFramedScrollBox, IThObserver, IThCanvas)
  private
//    FObjectManager: TThothObjectManager;
    FObjectManager: IThSubject;
    FPosition: TPosition;
    FShape: IThShape;
    FDrawClass: TThShapeClass;
    FDrawMode: TThDrawMode;

    function PointInShape(const X, Y: Single): TThShape;
    procedure SetDrawClass(const Value: TThShapeClass);
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand); virtual;
    procedure SetSubject(ASubject: IThSubject);

    property DrawClass: TThShapeClass read FDrawClass write SetDrawClass;
    property DrawMode: TThDrawMode read FDrawMode write FDrawMode;
  end;


implementation

uses
  Winapi.Windows;

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FDrawClass := nil;
  FPosition := TPosition.Create(PointF(0, 0));
end;

destructor TThCanvas.Destroy;
begin
  FPosition.Free;

  inherited;
end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Shape: TThShape;
begin
  inherited;

  if FDrawMode = dmSelect then
  begin
//    Shape := PointInShape(X, Y);
    Exit;
  end;

//  OutputDebugString(PChar(Format('ScrollBox Down: X:%f, Y:%f', [X, Y])));

  FPosition.X := X;
  FPosition.Y := y;

  FShape := FDrawClass.Create(Self);
  TThShape(FShape).Position.X := FPosition.X;
  TThShape(FShape).Position.Y := FPosition.Y;
  TThShape(FShape).Parent := Self;

  FDrawMode := dmDrawing;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if FDrawMode = dmDrawing then
  begin
    TThShape(FShape).Width := X - FPosition.X;
    TThShape(FShape).Height := Y - FPosition.Y;
  end;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FDrawMode = dmDrawing then
  begin
    TThShape(FShape).Width := X - FPosition.X;
    TThShape(FShape).Height := Y - FPosition.Y;

    FObjectManager.Report(TThInsertShapeCommand.Create(TThShape(FShape)));
//    TThothObjectManager(FObjectManager).
    FDrawMode := dmSelect;
  end;
//  FShape.Parent := Self;

//  OutputDebugString(PChar(Format('Up: X:%f, Y:%f', [X, Y])));
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  if ACommand is TThInsertShapeCommand then
    OutputDebugSTring(PChar('TThCanvas TThInsertShapeCommand'));
end;

function TThCanvas.PointInShape(const X, Y: Single): TThShape;
begin
  Result := nil;
end;

procedure TThCanvas.SetDrawClass(const Value: TThShapeClass);
begin
  FDrawClass := Value;
  if Value = nil then
  begin
    FDrawMode := dmSelect;
  end
  else
  begin
    FDrawMode := dmDraw;
  end;
end;

procedure TThCanvas.SetSubject(ASubject: IThSubject);
begin
  if Assigned(ASubject) then
    FObjectManager := TThothObjectManager(ASubject);

  if Assigned(ASubject) then
  begin

  end;
//  FObjectManager := (ASubject) as TThothObjectManager;

  ASubject.RegistObserver(Self);
end;

end.
