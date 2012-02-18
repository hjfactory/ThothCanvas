unit ThothCanvas;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Layouts,
  ThothTypes, ThothObjects;

type
///////////////////////////////////////////////////////
// Canvas
  TThCanvas = class(TFramedScrollBox, IThObserver, IThCanvas)
  private
    FPosition: TPosition;
    FShape: TThShape;
    FDrawClass: TThShapeClass;

    procedure DrawBegin(Shape: IThShape);
    procedure DrawEnd(Shape: IThShape);
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Notifycation(ACommand: IThCommand); virtual;

    property DrawClass: TThShapeClass read FDrawClass write FDrawClass;
  end;


implementation

{ TThCanvas }

constructor TThCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FDrawClass := TThLine;
end;

procedure TThCanvas.DrawBegin(Shape: IThShape);
begin

end;

procedure TThCanvas.DrawEnd(Shape: IThShape);
begin

end;

procedure TThCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

//  OutputDebugString(PChar(Format('ScrollBox Down: X:%f, Y:%f', [X, Y])));

  if Assigned(FPosition) then
  begin
    FPosition.Free;
  end;

  FPosition := TPosition.Create(PointF(X, Y));
  FPosition.X := X;
  FPosition.Y := y;

  FShape := FDrawClass.Create(Self);
  FShape.Position.X := FPosition.X;
  FShape.Position.Y := FPosition.Y;

end;

procedure TThCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if not Assigned(FPosition) then
    Exit;


  FShape.Width := X - FPosition.X;
  FShape.Height := Y - FPosition.Y;
  FShape.Parent := Self;
end;

procedure TThCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if not Assigned(FPosition) then
    Exit;

  FShape.Width := X - FPosition.X;
  FShape.Height := Y - FPosition.Y;
  FShape.Parent := Self;

  FPosition.Free;
  FPosition := nil;

//  OutputDebugString(PChar(Format('Up: X:%f, Y:%f', [X, Y])));
end;

procedure TThCanvas.Notifycation(ACommand: IThCommand);
begin
  inherited;

end;

end.
