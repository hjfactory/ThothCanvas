unit ThShape;

interface

uses
  System.Types, System.Classes, System.UITypes,
  FMX.Types, ThItem;

type
  TThShape = class(TThItem)
  private
    FSelected: Boolean;

    FOnSelect: TNotifyEvent;
    FOnTrack: TNotifyEvent;

    procedure SetSelected(const Value: Boolean);
  strict private
    procedure Paint; override;
  protected
    procedure PaintShape; virtual; abstract;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    function GetShapeRect: TRectF; virtual;
    function PtInShape(APt: TPointF): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property Selected: Boolean read FSelected write SetSelected;

    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TThLineShape = class(TThShape)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TThFillShape = class(TThShape)
  end;

  TThRectangle = class(TThFillShape)
  protected
    procedure PaintShape; override;
  end;

  TThLine = class(TThLineShape)
  protected
    procedure PaintShape; override;
  end;

implementation

uses
  ThItemFactory, System.UIConsts;

{ TThShape }

constructor TThShape.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;

  FWidth := 0;
  FHeight := 0;

  FSelected   := False;
end;

function TThShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TThShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThShape.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TThShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThShape.Paint;
begin
  PaintShape;
end;

function TThShape.PtInShape(APt: TPointF): Boolean;
begin
  Result := PtInRect(GetShapeRect, APt);
end;

procedure TThShape.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected then
  begin
//    ShowSelection;
    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end
  else
//    HideSelection;
  ;

  Repaint;
end;

{ TThLineShape }

constructor TThLineShape.Create(AOwner: TComponent);
begin
  inherited;

end;

{ TThRectangle }

procedure TThRectangle.PaintShape;
var
  R: TRectF;
begin
  R := GetShapeRect;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity, TCornerType.ctRound);
end;

{ TThLine }

procedure TThLine.PaintShape;
begin

end;

initialization
  RegisterItem(1100, TThRectangle);

end.
