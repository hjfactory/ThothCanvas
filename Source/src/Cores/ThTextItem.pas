unit ThTextItem;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, FMX.Objects, ThTypes, ThItem;

type
  TThTextItem = class(TThItem, ITextServiceControl, IItemHighlightObject, IItemSelectionObject)
  private
    FTextService: TTextService;

    FFont: TFont;
    FFontColor: TAlphaColor;
    FTextAlign: TTextAlign;
    FWordWrap: Boolean;
    FSelStart: Integer;
    FSelLength: Integer;

    function GetCharX(a: Integer): Single;

    function GetCaretPosition: Integer;
    procedure SetCaretPosition(const Value: Integer);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
  protected
    function CreateHighlighter: IItemHighlighter; override;
    function CreateSelection: IItemSelection; override;

    // ThItem
    function PtInItem(Pt: TPointF): Boolean; override;
    procedure Paint; override;

    function CreateCaret: TCaret; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure InsertText(const AText: string);
    procedure RepaintEdit;
    function TextWidth(const Str: string): Single;

    // IItemHighlightObject
    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
    // IItemSelectionObject
    function GetMinimumSize: TSizeF; virtual;

    // ITextServiceControl
    function GetTextService: TTextService;
    procedure UpdateCaretPoint;
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Text: string read GetText write SetText;
    property CaretPosition: Integer read GetCaretPosition write SetCaretPosition;
    function ContentRect: TRectF;

    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText;
  end;


implementation

uses
  ThConsts, ThItemFactory, ThItemSelection, ThItemHighlighter, FMX.Platform;

{ TThTextItem }

function TThTextItem.ContentRect: TRectF;
begin
  Result := LocalRect;
end;

constructor TThTextItem.Create(AOwner: TComponent);
var
  PlatformTextService: IFMXTextService;
begin
  inherited;

  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, IInterface(PlatformTextService)) then
    FTextService := PlatformTextService.GetTextServiceClass.Create(Self, False);

  FFont := TFont.Create;
  FFont.Size := 200;
  FFontColor := TAlphaColorRec.Black;
  FWordWrap := False;
  FTextAlign := TTextAlign.taLeading;

  CanFocus := True;
  Cursor := crIBeam;
  AutoCapture := True;

  Width := 100;
  Height := 22;
  FSelStart := 0;
  FSelLength := 0;
end;

destructor TThTextItem.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FTextService);

  inherited;
end;

function TThTextItem.CreateCaret: TCaret;
begin
  Result := TCaret.Create(Self);
  Result.Color := FFontColor;
  Result.Visible := True;
  Result.ReadOnly := False;
end;

function TThTextItem.CreateHighlighter: IItemHighlighter;
var
  Highlighter: TThItemRectBorderHighlighter;
begin
  Highlighter := TThItemRectBorderHighlighter.Create(Self);
  Highlighter.HighlightColor := ItemHighlightColor;
  Highlighter.HighlightSize := ItemHighlightSize;

  Result := Highlighter;
end;

function TThTextItem.CreateSelection: IItemSelection;
var
  Selection: TThItemSelection;
begin
  Selection := TThItemSelection.Create(Self);
  Selection.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
  Selection.OnTracking := SpotTracking;

  Result := Selection;
end;

function TThTextItem.GetMinimumSize: TSizeF;
var
  MinSize: Single;
begin
  MinSize := 100 / AbsoluteScale.X;

  Result := PointF(MinSize, MinSize);
end;

function TThTextItem.GetSelLength: Integer;
begin
  Result := Abs(FSelLength);
end;

function TThTextItem.GetSelStart: Integer;
begin
  if FSelLength > 0 then
    Result := FSelStart
  else if FSelLength < 0 then
    Result := FSelStart + FSelLength
  else
    Result := CaretPosition;
end;

function TThTextItem.GetSelText: string;
begin
  Result := Text.Substring(SelStart, SelLength);
end;

function TThTextItem.GetTargetClausePointF: TPointF;
var
  Str: String;
begin
  Str := FTextService.CombinedText.Substring(0, Round(FTextService.TargetClausePosition.X) );
  Result.X := TextWidth(Str);
  Result.Y := (ContentRect.Height / 2) + FFont.Size / 2 + 2;  // 2 is small space between conrol and IME window
  Result.X := Result.X + ContentRect.Top + Self.Position.Point.X;
  Result.Y := Result.Y + ContentRect.Left + Self.Position.Point.Y;
end;

function TThTextItem.GetText: string;
begin
  Result := FTextService.Text;
end;

function TThTextItem.GetTextService: TTextService;
begin
  Result := FTextService;
end;

procedure TThTextItem.InsertText(const AText: string);
var
  OldText: string;
begin
  OldText := Text;
  // FActionStack.FragmentDeleted(SelStart + 1, Copy(TmpS, SelStart+1, SelLength));
  OldText := OldText.Remove(SelStart, SelLength);

  // FActionStack.FragmentInserted(SelStart + 1, Length(AText), SelLength <> 0);
  OldText := OldText.Insert(SelStart, AText);

  Text := OldText;
  CaretPosition := SelStart + AText.Length;

  SelLength := 0;
end;

procedure TThTextItem.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  S: string;
  TmpS: string;
  LCaret: Integer;
begin
  inherited;

  case Key of
    vkLeft:
      if ([ssCtrl, ssCommand] * Shift) <> [] then
        CaretPosition := FTextService.GetPrevWordBeginPosition(CaretPosition)
      else
        CaretPosition := FTextService.GetPrevCharacterPosition(CaretPosition);
    vkRight:
      if ([ssCtrl, ssCommand] * Shift) <> [] then
        CaretPosition := FTextService.GetNextWordBeginPosition(CaretPosition)
      else
        CaretPosition := FTextService.GetNextCharacterPosition(CaretPosition);
    vkDelete:
      begin
        begin
          TmpS := Text;
          if not TmpS.IsEmpty then
          begin
            if ([ssCtrl, ssCommand] * Shift) <> [] then
            begin
              //Delete whole word
              LCaret := FTextService.GetPrevWordBeginPosition(CaretPosition);
              if LCaret < 0 then
                Exit;
              TmpS := Text;
              TmpS := TmpS.Remove(LCaret, CaretPosition - LCaret);
            end
            else
              //Delete single character
              TmpS := TmpS.Remove(CaretPosition, 1);
            Text := TmpS;
//            DoTyping;
          end;
        end;
      end;
    vkBack:
//      if not ReadOnly and InputSupport then
      begin
//        if SelLength <> 0 then
//        begin
//          DeleteSelection;
//          DoTyping;
//        end
//        else
        begin
          TmpS := Text;
          if not TmpS.IsEmpty then
          begin
            TmpS := TmpS.Remove(CaretPosition - 1, 1);
            CaretPosition := CaretPosition - 1;
            Text := TmpS;
//            DoTyping;
          end;
        end;
      end;
  end;

  if (Ord(KeyChar) >= 32) and (KeyChar <> #0) then
  begin
    S := KeyChar;
    InsertText(S);
    KeyChar := #0;
  end;
end;

procedure TThTextItem.Paint;
begin
  inherited;

  PaintItem(GetItemRect, TAlphaColorRec.Null);
end;

procedure TThTextItem.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  State: TCanvasSaveState;
begin
  { draw text }
  if (FTextService.Text = '') and (not FTextService.HasMarkedText) then
    Exit;
  State := Canvas.SaveState;
  try
    Canvas.IntersectClipRect(ARect);
    Canvas.Font.Assign(FFont);
    Canvas.Fill.Color := FFontColor;
    FTextService.DrawSingleLine(Canvas, ARect, 1, FFont,
      AbsoluteOpacity, FillTextFlags, FTextAlign, TTextAlign.taCenter);
    { carret }
    if IsFocused then
    begin
      { selection }
{
      if SelLength > 0 then
      begin
        R := GetSelRect;
        R1 := ContentRect;
        OffsetRect(R, -R1.Left, -R1.Top);
        Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity, FSelectionFill);
      end;
}
    end;
  finally
    Canvas.RestoreState(State);
  end;
end;

function TThTextItem.PtInItem(Pt: TPointF): Boolean;
begin
  Result := False;
  if (AbsoluteRect.Width < ItemFocusMinimumSize) and (AbsoluteRect.Height < ItemFocusMinimumSize) then
    Exit;

  Result := PtInRect(GetItemRect, Pt);
end;

procedure TThTextItem.RepaintEdit;
begin
  Repaint;
end;

procedure TThTextItem.UpdateCaretPoint;
begin
  SetCaretPosition(CaretPosition);
end;

function TThTextItem.GetCaretPosition: Integer;
begin
  Result := FTextService.CaretPosition.X;
end;

function TThTextItem.GetCharX(a: Integer): Single;
var
  WholeTextWidth: Single;
  EditRectWidth: Single;
  R: TRectF;
  T: string;
begin
  R := ContentRect;
  TCanvasManager.MeasureCanvas.Font.Assign(FFont);
  T := FTextService.CombinedText;
  if T = '' then
    T := 'a';
  TCanvasManager.MeasureCanvas.MeasureText(R, T, False, FillTextFlags, TTextAlign.taLeading, TTextAlign.taCenter);
  WholeTextWidth := R.Right - ContentRect.Left;
  Result := ContentRect.Left;

  if a > 0 then
  begin
    if a <= FTextService.CombinedText.Length then
    begin
      R := ContentRect;
      TCanvasManager.MeasureCanvas.MeasureText(R, T.Substring(0, a), False, FillTextFlags,
        TTextAlign.taLeading, TTextAlign.taCenter);
      Result := R.Right;
    end
    else
    begin
      R := ContentRect;
    end;
  end;

  EditRectWidth := ContentRect.Right - ContentRect.Left;
  if WholeTextWidth < EditRectWidth then
    case FTextAlign of
      TTextAlign.taTrailing:
        Result := Result + (EditRectWidth - WholeTextWidth);
      TTextAlign.taCenter:
        Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
    end;
end;

procedure TThTextItem.SetCaretPosition(const Value: Integer);
var
  P: TPoint;
  Pos: TPointF;
begin
  P.X := 0; P.Y := 0;
  if Value < 0 then
    P.X := 0
  else if Value > Text.Length then
    P.X := Text.Length
  else
    P.X := Value;
  FTextService.CaretPosition := P;

//  UpdateFirstVisibleChar;

  if SelLength <= 0 then
    FSelStart := Value;

  RepaintEdit;

  if IsFocused then
  begin
    Pos.Y := (ContentRect.Top + ContentRect.Bottom - (FFont.Size * 1.25)) / 2;
    if FTextService.HasMarkedText then
      Pos.X := GetCharX(FTextService.TargetClausePosition.X)
    else
      Pos.X := GetCharX(FTextService.CaretPosition.X);
    SetCaretParams(Pos, PointF(1, (FFont.Size * 1.25)), FFontColor);
  end;
end;

procedure TThTextItem.SetSelLength(const Value: Integer);
begin
  if FSelLength <> Value then
  begin
    FSelLength := Value;
    RepaintEdit;
  end;
end;

procedure TThTextItem.SetSelStart(const Value: Integer);
begin
  if FSelStart <> Value then
  begin
    SelLength := 0;
    FSelStart := Value;
    CaretPosition := FSelStart;
    RepaintEdit;
  end;
end;

procedure TThTextItem.SetText(const Value: string);
begin
  if FTextService.Text <> Value then
  begin
    FTextService.Text := Value;
    if FTextService.CaretPosition.X > Text.Length then
      SetCaretPosition(Text.Length);
    RepaintEdit;
  end;
end;

procedure TThTextItem.StartIMEInput;
begin
  FTextService.CaretPosition := Point(CaretPosition, 0);
end;

function TThTextItem.TextWidth(const Str: string): Single;
var
  R: TRectF;
begin
  R := ContentRect;
  R.Right := 10000;
  TCanvasManager.MeasureCanvas.Font.Assign(FFont);
  TCanvasManager.MeasureCanvas.MeasureText(R, Str, False, FillTextFlags, TTextAlign.taLeading, TTextAlign.taCenter);
  Result := R.Width;
end;

procedure TThTextItem.EndIMEInput;
begin
  FTextService.Text := FTextService.CombinedText;
  FTextService.CaretPosition := Point(CaretPosition + FTextService.CombinedText.Length - FTextService.Text.Length, 0);
  RepaintEdit;
end;

initialization
  RegisterItem(ItemFactoryIDText, TThTextItem);

end.
