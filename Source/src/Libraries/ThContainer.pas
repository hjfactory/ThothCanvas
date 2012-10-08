unit ThContainer;

interface

uses
  System.Classes, System.SysUtils, ThItem,
  System.Types, System.UITypes, System.UIConsts, FMX.Types;

type
  TThContents = class(TControl)
  private
    FTrackingPos: TPointF;
  protected
    function GetClipRect: TRectF; override;
    function GetUpdateRect: TRectF; override;
    procedure Paint; override;
  public
    procedure AddTrackingPos(const Value: TPointF);
  end;

  TThContainer = class(TControl)
  private
    FUseMouseTracking: Boolean;
    FSelecteditem: TThItem;
    FBackgroundColor: TAlphaColor;

    procedure ItemSelect(Sender: TObject);
    procedure ItemUnselect(Sender: TObject);
    function GetContentPos: TPosition;
    function GetItemCount: Integer;
    procedure SetBackgroundColor(const Value: TAlphaColor);
  protected
    FContents: TThContents;
    FMouseDownPos,          // MouseDown 시 좌표
    FMouseCurrPos: TPointF; // MouseMove 시 좌표

    procedure Paint; override;
//    procedure PaintChildren; override;

    procedure DoAddObject(AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    function InsertItem(const ItemID: Integer): TThItem;
    procedure ClearSelection;

    property SelectedItem: TThItem read FSelectedItem;

    property ContentPos: TPosition read GetContentPos;
    property ItemCount: Integer read GetItemCount;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
   end;

implementation

uses
  ThItemFactory, CommonUtils;

{ TThContent }

procedure TThContents.AddTrackingPos(const Value: TPointF);
begin
  FTrackingPos := Value;

  Position.X := Position.X + Value.X;
  Position.Y := Position.Y + Value.Y;
end;

function TThContents.GetClipRect: TRectF;
begin
  Result :=  TControl(Parent).ClipRect;
  OffsetRect(Result, -Position.X, -Position.Y);
end;

function TThContents.GetUpdateRect: TRectF;
begin
  if not Assigned(Parent) then
    Exit;

{   ClipClildren := True 설정 시 Canvas 영역을 빠져나가면 Contents 표시 멈춤
      TControl.GetUpdateRect 11 line
          if TControl(P).ClipChildren or TControl(P).SmallSizeControl then
            IntersectRect(FUpdateRect, FUpdateRect, TControl(P).UpdateRect);}

  TControl(Parent).ClipChildren := False;
  try
    Result := inherited GetUpdateRect;
  finally
    TControl(Parent).ClipChildren := True;
  end;
end;

procedure TThContents.Paint;
begin
  inherited;

{$IFDEF DEBUG}
  Canvas.Fill.Color := claNull;
  Canvas.Stroke.Color := claBlack;

  Canvas.DrawRect(TControl(Parent).ClipRect, 0, 0, AllCorners, 1);
{$ENDIF}
end;

{ TThContainer }

constructor TThContainer.Create(AOwner: TComponent);
begin
  inherited;

  ClipChildren := True; // 컨트롤이 영역밖에 표시되지 않도록 처리
  AutoCapture := True;  // 영역밖으로 나가도 컨트롤 되도록 처리

  FUseMouseTracking := True;

  FContents := TThContents.Create(Self);
  FContents.Parent := Self;
  FContents.HitTest := False;
  FContents.Stored := False;
  FContents.Locked := True;

{$IFDEF DEBUG}
  FBackgroundColor := $FFDDDDDD;
{$ELSE}
  FBackgroundColor := $FFFFFFFF;
{$ENDIF}
end;

destructor TThContainer.Destroy;
begin
  FContents.Free;

  inherited;
end;

procedure TThContainer.DoAddObject(AObject: TFmxObject);
begin
  if Assigned(FContents) and (AObject <> FContents) then
    FContents.AddObject(AObject)
  else
    inherited;
end;

procedure TThContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FPressed and FUseMouseTracking then
  begin
    FMouseDownPos := PointF(X, Y);
    FMouseCurrPos := PointF(X, Y);
  end;
end;

procedure TThContainer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  TrackingPos: TPointF;
begin
  if FPressed and FUseMouseTracking then
  begin
    TrackingPos := PointF(X - FMouseCurrPos.X, Y - FMouseCurrPos.Y);
    FMouseCurrPos := PointF(X, Y);

    FContents.AddTrackingPos(TrackingPos);
  end;
end;

procedure TThContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if FMouseDownPos = PointF(X, Y) then
    ClearSelection;
end;

procedure TThContainer.Paint;
begin
  inherited;

  Canvas.Fill.Color := FBackgroundColor;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
end;

procedure TThContainer.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor = Value then
    Exit;

  FBackgroundColor := Value;
  Repaint;
end;

function TThContainer.InsertItem(const ItemID: Integer): TThItem;
begin
  Result := ItemFactory.Get(ItemID);
  if Assigned(Result) then
  begin
    Result.Parent := Self;
    Result.OnSelected := ItemSelect;
    Result.OnUnselected := ItemUnselect;
  end;
end;

procedure TThContainer.ItemSelect(Sender: TObject);
begin
  // List처리 필요
  if Assigned(FSelectedItem) then
    FSelectedItem.Selected := False;

  FSelectedItem := TThItem(Sender);
end;

procedure TThContainer.ItemUnselect(Sender: TObject);
begin
  FSelectedItem := nil;
  // List처리 필요
end;

procedure TThContainer.ClearSelection;
begin
  if Assigned(FSelectedItem) then
    FSelectedItem.Selected := False;
  FSelectedItem := nil;
end;

function TThContainer.GetContentPos: TPosition;
begin
  Result := FContents.Position;
end;

function TThContainer.GetItemCount: Integer;
begin
  Result := FContents.ChildrenCount;
end;

end.
