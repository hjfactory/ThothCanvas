unit Unit5;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Objects, FMX.Layouts, FMX.Controls, FMX.Ani;

type
  TThothContent = class(TContent)
  protected
    function GetClipRect: TRectF; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    function GetUpdateRect: TRectF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddObject(AObject: TFmxObject); override;
    procedure RemoveObject(AObject: TFmxObject); override;
  end;

  TThothCanvas = class(TStyledControl)
  private
    FAutoHide: Boolean;
    FShowScrollBars: Boolean;

    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;
    FContentLayout: TControl;

    FHScrollAni: TFloatAnimation;
    FVScrollAni: TFloatAnimation;

    FScrollDesign: TPointF;

    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    function GetVScrollBar: TScrollBar;
    function GetHScrollBar: TScrollBar;

    procedure HScrollChange(Sender: TObject);
    procedure VScrollChange(Sender: TObject);

    procedure ApplyStyle; override;
    procedure FreeStyle; override;

    function GetContentBounds: TRectF; virtual;
    procedure RealignContent(R: TRectF); virtual;
    procedure SetShowScrollBars(const Value: Boolean);

    property ContentLayout: TControl read FContentLayout;
    procedure ContentAddObject(AObject: TFmxObject); virtual;
    procedure ContentBeforeRemoveObject(AObject: TFmxObject); virtual;
    procedure ContentRemoveObject(AObject: TFmxObject); virtual;
  protected
    FContent: TThothContent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddObject(AObject: TFmxObject); override;

    procedure Center;

    procedure Realign; override;

    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    property HScrollBar: TScrollBar read GetHScrollBar;
    property VScrollBar: TScrollBar read GetVScrollBar;

    property AutoHide: Boolean read FAutoHide write FAutoHide default True;
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
  end;

implementation

{ TThothCanvas }

procedure TThothCanvas.AddObject(AObject: TFmxObject);
begin
  if (FContent <> nil) and (AObject <> FContent) and (AObject <> FResourceLink) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.AddObject(AObject);
  end
  else
    inherited;
end;

procedure TThothCanvas.ApplyStyle;
var
  B: TFmxObject;
begin
  inherited;
  // hide all before align
  B := FindStyleResource('vscrollbar');
  if (B <> nil) and (B is TControl) then
    TControl(B).Visible := False;
  B := FindStyleResource('hscrollbar');
  if (B <> nil) and (B is TControl) then
    TControl(B).Visible := False;
  B := FindStyleResource('vsmallscrollbar');
  if (B <> nil) and (B is TControl) then
    TControl(B).Visible := False;
  B := FindStyleResource('hsmallscrollbar');
  if (B <> nil) and (B is TControl) then
    TControl(B).Visible := False;
  if (FVScrollBar = nil) or (FHScrollBar = nil) then
  begin
    B := FindStyleResource('vscrollbar');
    if (B <> nil) and (B is TScrollBar) then
    begin
      FVScrollBar := TScrollBar(B);
      FVScrollBar.OnChange := VScrollChange;
      FVScrollBar.Locked := True;
      if FVScrollBar.Tag = 0 then
        FVScrollBar.Tag := Integer(FVScrollBar.Align);
    end;
    B := FindStyleResource('hscrollbar');
    if (B <> nil) and (B is TScrollBar) then
    begin
      FHScrollBar := TScrollBar(B);
      FHScrollBar.OnChange := HScrollChange;
      FHScrollBar.Locked := True;
      if FHScrollBar.Tag = 0 then
        FHScrollBar.Tag := Integer(FHScrollBar.Align);
    end;
  end;
  B := FindStyleResource('content');
  if (B <> nil) and (B is TControl) then
    FContentLayout := TControl(B);

  Realign;
  FVScrollAni := nil;
  FHScrollAni := nil;
end;

procedure TThothCanvas.Center;
begin
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := (VScrollBar.Max - VScrollBar.Min - VScrollBar.ViewportSize) / 2;
  end;
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
  begin
    HScrollBar.Value := (HScrollBar.Max - HScrollBar.Min - HScrollBar.ViewportSize) / 2;
  end;
end;

procedure TThothCanvas.ContentAddObject(AObject: TFmxObject);
begin

end;

procedure TThothCanvas.ContentBeforeRemoveObject(AObject: TFmxObject);
begin

end;

procedure TThothCanvas.ContentRemoveObject(AObject: TFmxObject);
begin

end;

constructor TThothCanvas.Create(AOwner: TComponent);
begin
  inherited;

  FAutoHide := False;
  FShowScrollBars := True;

  FContent := TThothContent.Create(Self);
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
end;

procedure TThothCanvas.DefineProperties(Filer: TFiler);
begin
  inherited;

end;

destructor TThothCanvas.Destroy;
begin
  FContent := nil;

  inherited;
end;

procedure TThothCanvas.FreeStyle;
begin
  inherited;
  FContentLayout := nil;
  FHScrollBar := nil;
  FVScrollBar := nil;
end;

function TThothCanvas.GetContentBounds: TRectF;
var
  i: Integer;
  R, LocalR: TRectF;
begin
  Result := RectF(-1000, -1000, 1000, 1000);
{
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    R := ContentLayout.LocalRect;
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TControl then
        if (TControl(FContent.Children[i]).Visible) then
        begin
          if (csDesigning in ComponentState) and not (csDesigning in FContent.Children[i].ComponentState) then Continue;
          LocalR := TControl(FContent.Children[i]).ParentedRect;
          R := UnionRect(R, LocalR);
        end;
    Result := R;
  end;
}
end;

function TThothCanvas.GetHScrollBar: TScrollBar;
begin
  if FHScrollBar = nil then
    ApplyStyleLookup;
  Result := FHScrollBar;
end;

function TThothCanvas.GetVScrollBar: TScrollBar;
begin
  if FVScrollBar = nil then
    ApplyStyleLookup;
  Result := FVScrollBar;
end;

procedure TThothCanvas.HScrollChange(Sender: TObject);
begin
  if ContentLayout = nil then
    Exit;
  if HScrollBar.Visible then
    FContent.Position.X := ContentLayout.Position.X - HScrollBar.Value
  else
    FContent.Position.X := ContentLayout.Position.X;
  FScrollDesign.X := HScrollBar.Value;
end;

procedure TThothCanvas.Loaded;
begin
  inherited;

end;

procedure TThothCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThothCanvas.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TThothCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

procedure TThothCanvas.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;

end;

procedure TThothCanvas.Realign;

  procedure IntAlign;
  var
    R: TRectF;
  begin
    R := GetContentBounds;
    if RectWidth(R) * RectHeight(R) = 0 then
      Exit;
    OffsetRect(R, ContentLayout.Position.X, ContentLayout.Position.Y);
    if (HScrollBar <> nil) and (HScrollBar.Enabled) then
      OffsetRect(R, -FScrollDesign.X, 0);
    if (VScrollBar <> nil) and (VScrollBar.Enabled) then
      OffsetRect(R, 0, -FScrollDesign.Y);
    RealignContent(R);
    // realign resource
    if (ContentLayout.Parent <> nil) and (ContentLayout.Parent is TControl) then
      TControl(ContentLayout.Parent).BeginUpdate;
    if (VScrollBar <> nil) then
    begin
      VScrollBar.Enabled := RectHeight(R) > ContentLayout.Height;
      if FAutoHide then
        VScrollBar.Visible := VScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        VScrollBar.Opacity := 0;
        VScrollBar.Align := TAlignLayout.alNone;
      end
      else
      begin
        VScrollBar.Opacity := 1;
        VScrollBar.Align := TAlignLayout(VScrollBar.Tag);
      end;
    end;
    if (HScrollBar <> nil) then
    begin
      HScrollBar.Enabled := RectWidth(R) > ContentLayout.Width;
      if FAutoHide then
        HScrollBar.Visible := HScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        HScrollBar.Opacity := 0;
        HScrollBar.Align := TAlignLayout.alNone;
      end
      else
      begin
        HScrollBar.Opacity := 1;
        HScrollBar.Align := TAlignLayout(HScrollBar.Tag);
        if (VScrollBar <> nil) and (VScrollBar.Enabled) then
          HScrollBar.Padding.right := VScrollBar.Width;
      end;
    end;
    if (ContentLayout.Parent <> nil) and (ContentLayout.Parent is TControl) then
    begin
      TControl(ContentLayout.Parent).EndUpdate;
      TControl(ContentLayout.Parent).Realign;
    end;
    // align scrollbars
    if (VScrollBar <> nil) then
    begin
      VScrollBar.Enabled := RectHeight(R) > ContentLayout.Height;
      if FAutoHide then
        VScrollBar.Visible := VScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        VScrollBar.Opacity := 0;
        VScrollBar.Align := TAlignLayout.alNone;
        VScrollBar.Position.Y := Width + 100;
      end
      else
      begin
        VScrollBar.Opacity := 1;
        VScrollBar.HitTest := True;
        VScrollBar.Align := TAlignLayout(VScrollBar.Tag);
      end;
      VScrollBar.BringToFront;
      if VScrollBar.Visible and (ContentLayout <> nil) then
      begin
        VScrollBar.Max := R.Bottom; //RectHeight(R); hjf
        VScrollBar.ViewportSize := ContentLayout.Height;
        VScrollBar.SmallChange := VScrollBar.ViewportSize / 5;
        VScrollBar.Value := FScrollDesign.Y;
      end
      else
      begin
        VScrollBar.Value := 0;
      end;
    end;
    if (HScrollBar <> nil) then
    begin
      HScrollBar.Enabled := RectWidth(R) > ContentLayout.Width;
      HScrollBar.Padding.right := 0;
      if FAutoHide then
        HScrollBar.Visible := HScrollBar.Enabled;
      if not FShowScrollBars then
      begin
        HScrollBar.Opacity := 0;
        HScrollBar.Align := TAlignLayout.alNone;
        HScrollBar.Position.Y := Height + 100;
      end
      else
      begin
        HScrollBar.Opacity := 1;
        HScrollBar.Align := TAlignLayout(HScrollBar.Tag);
        if (VScrollBar <> nil) and (VScrollBar.Enabled) then
          HScrollBar.Padding.right := VScrollBar.Width;
      end;
      HScrollBar.BringToFront;
      if HScrollBar.Visible and (ContentLayout <> nil) then
      begin
        HScrollBar.Max := R.Right; //RectWidth(R); hjf
        HScrollBar.ViewportSize := ContentLayout.Width;
        HScrollBar.SmallChange := HScrollBar.ViewportSize / 5;
        HScrollBar.Value := ContentLayout.Position.X - FContent.Position.X;
      end
      else
        HScrollBar.Value := 0;
    end;
  end;

var
  R, NewR: TRectF;
begin
  if csDestroying in ComponentState then
    Exit;
  inherited;
  if csLoading in ComponentState then
    Exit;
  if ContentLayout = nil then
    Exit;
  if FDisableAlign then
    Exit;
  if FUpdating > 0 then
    Exit;
  FDisableAlign := True;
  try
    R := ContentLayout.LocalRect;
    IntAlign;
    NewR := ContentLayout.LocalRect;
    if (RectWidth(NewR) <> RectWidth(R)) or (RectHeight(NewR) <> RectHeight(R))
    then
    begin
      IntAlign;
    end;
  finally
    FDisableAlign := False;
  end;
end;

procedure TThothCanvas.RealignContent(R: TRectF);
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    FContent.SetBounds(R.Left, R.Top, RectWidth(R), RectHeight(R));
    FContent.FRecalcUpdateRect := True; // need to recalc
  end;
end;

procedure TThothCanvas.SetShowScrollBars(const Value: Boolean);
begin
  if FShowScrollBars <> Value then
  begin
    FShowScrollBars := Value;
    Realign;
  end;
end;

procedure TThothCanvas.Sort(Compare: TFmxObjectSortCompare);
begin
  inherited;

end;

procedure TThothCanvas.VScrollChange(Sender: TObject);
begin
  if ContentLayout = nil then
    Exit;
  if VScrollBar.Visible then
    FContent.Position.Y := ContentLayout.Position.Y - VScrollBar.Value
  else
    FContent.Position.Y := ContentLayout.Position.Y;
  FScrollDesign.Y := VScrollBar.Value;
end;

{ TThothContent }

procedure TThothContent.AddObject(AObject: TFmxObject);
begin
  inherited;
  if (Parent <> nil) and (Parent is TThothCanvas) then
    TThothCanvas(Parent).ContentAddObject(AObject);
end;

constructor TThothContent.Create(AOwner: TComponent);
begin
  inherited;

  ClipChildren := True;
end;

function TThothContent.GetClipRect: TRectF;
begin
  if (Parent <> nil) and (Parent is TThothCanvas) and
    (TThothCanvas(Parent).ContentLayout <> nil) then
  begin
    Result := TThothCanvas(Parent).ContentLayout.LocalRect;
    if (TThothCanvas(Parent).VScrollBar <> nil) and
      (TThothCanvas(Parent).VScrollBar.Enabled) then
      OffsetRect(Result, 0, TThothCanvas(Parent).VScrollBar.Value);
    if (TThothCanvas(Parent).HScrollBar <> nil) and
      (TThothCanvas(Parent).HScrollBar.Enabled) then
      OffsetRect(Result, TThothCanvas(Parent).HScrollBar.Value, 0);
  end
  else
    Result := inherited GetClipRect;
end;

function TThothContent.GetUpdateRect: TRectF;
begin
  if FRecalcUpdateRect then
  begin
    if (Parent <> nil) and (Parent is TThothCanvas) then
    begin
      if (TThothCanvas(Parent).ContentLayout <> nil) then
        FUpdateRect := TThothCanvas(Parent).ContentLayout.UpdateRect
      else
        FUpdateRect := TThothCanvas(Parent).UpdateRect;
    end;
  end;
  Result := FUpdateRect;
end;

function TThothContent.ObjectAtPoint(P: TPointF): IControl;
begin
  Result := inherited ObjectAtPoint(P);
  if Result <> nil then
  begin
    if FScene <> nil then
      P := FScene.ScreenToLocal(P);
    P := AbsoluteToLocal(P);
    if not PointInRect(P, ClipRect) then
      Result := nil;
  end;
end;

procedure TThothContent.RemoveObject(AObject: TFmxObject);
begin
  if (Parent <> nil) and (Parent is TThothCanvas) then
    TThothCanvas(Parent).ContentBeforeRemoveObject(AObject);
  inherited;
  if (Parent <> nil) and (Parent is TThothCanvas) then
    TThothCanvas(Parent).ContentRemoveObject(AObject);
end;

end.
