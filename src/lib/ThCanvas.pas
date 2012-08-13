unit ThCanvas;

interface

uses
  System.UITypes, System.Classes, System.Types,
  ThTypes, ThLayout, ThShape;

type
  TThCanvas = class(TThContainer, IThCanvas, IThObserver)
  private
    FDrawShape: TThShape;
    FShapeClass: TThSahpeClass;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ShapeClass: TThSahpeClass read FShapeClass write FShapeClass;
  end;


implementation

{ TThCanvas }

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
//    P.Add(OffsetPos);

    Debug('OffsetPos(%f, %f, %f, %f)', [OffsetPos.X, OffsetPos.Y, P.X, P.Y]);

    FDrawShape := FShapeClass.Create(nil);
    FDrawShape.Parent := Self;
//    FDrawShape.Position.Point := FCurrentPos;
    FDrawShape.Position.Point := P;
  end;
end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
//    inherited;

  if Assigned(FShapeClass) and Assigned(FDrawShape) then
  begin
    FDrawShape.Width := X - FCurrentPos.X;
    FDrawShape.Height := Y - FCurrentPos.Y;
  end
  else
    inherited;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  FShapeClass := nil;
  FDrawShape := nil;
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin

end;

procedure TThCanvas.SetSubject(ASubject: IThSubject);
begin

end;

end.
