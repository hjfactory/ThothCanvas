unit ThImageItem;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, FMX.Objects, ThTypes, ThItem;

type
  TThImageItem = class(TThItem, IBitmapObject, IItemHighlitObject, IItemSelectionObject)
  private
    FItemData: TThFileItemData;
    FBitmap: TBitmap;

    procedure LoadImageFile(AFilename: TFileName);
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  protected
    function CreateHighlighter: IItemHighlighter; override;
    function CreateSelection: IItemSelection; override;

    procedure Paint; override;

    procedure PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
    function PtInItem(Pt: TPointF): Boolean; override;
    function GetMinimumSize: TSizeF; virtual;

    procedure DoBitmapChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsContain(AItem: TThItem): Boolean; override;

    procedure SetItemData(AItemData: IThItemData); override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

implementation

uses
  FMX.Dialogs, ThConsts, ThItemFactory, ThItemSelection, ThItemHighlighter, DebugUtils;

{ TThImageItem }

constructor TThImageItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRecalcOpacity := False;
  FAbsoluteOpacity := 1;

  FBitmap := TBitmap.Create(0, 0);
  FBitmap.OnChange := DoBitmapChanged;
end;

destructor TThImageItem.Destroy;
begin
  FBitmap.Free;

  inherited;
end;

procedure TThImageItem.SetItemData(AItemData: IThItemData);
var
  FN: TFileName;
begin
  inherited;

  FItemData := TThFileItemData(AItemData);

  FN := '';
  if Assigned(FItemData) then
    FN := FItemData.Filename;

  LoadImageFile(FN);
end;

procedure TThImageItem.DoBitmapChanged(Sender: TObject);
begin
  Repaint;
  UpdateEffects;
end;

function TThImageItem.CreateHighlighter: IItemHighlighter;
var
  Highlighter: TThItemRectBorderHighlighter;
begin
  Highlighter := TThItemRectBorderHighlighter.Create(Self);
  Highlighter.HighlightColor := ItemHighlightColor;
  Highlighter.HighlightSize := ItemHighlightSize;

  Result := Highlighter;
end;

function TThImageItem.CreateSelection: IItemSelection;
var
  Selection: TThItemSelection;
begin
  Selection := TThItemSelection.Create(Self);
  Selection.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
  Selection.OnTracking := nil;

  Result := Selection;
end;

function TThImageItem.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TThImageItem.GetMinimumSize: TSizeF;
var
  MinSize: Single;
begin
  MinSize := ItemMinimumSize / AbsoluteScale.X;

  Result := PointF(MinSize, MinSize);
end;

function TThImageItem.IsContain(AItem: TThItem): Boolean;
begin
  Result := AbsoluteRect.Contains(TThItem(AItem).AbsoluteRect);
end;

procedure TThImageItem.LoadImageFile(AFilename: TFileName);
var
  Dialog: TOpenDialog;
begin
  if AFilename = '' then
  begin
    Dialog := TOpenDialog.Create(nil);
    try
      Dialog.Filter := TBitmapCodecManager.GetFilterString;
      if Dialog.Execute then
        AFilename := Dialog.FileName;
    finally
      Dialog.Free;
    end;
  end;

  FBitmap.LoadFromFile(AFilename);

  Width := FBitmap.Width;
  Height := FBitmap.Height;
end;

procedure TThImageItem.Paint;
begin
  PaintItem(GetItemRect, claNull);
end;

procedure TThImageItem.PaintItem(ARect: TRectF; AFillColor: TAlphaColor);
var
  R: TRectF;
  B: TBitmap;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, FAbsoluteOpacity, $A0909090);
  end;

  B := FBitmap;
  if B.IsEmpty then
    Exit;

  R := LocalRect;
  Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
    FAbsoluteOpacity, False)
end;

function TThImageItem.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetItemRect, Pt);
end;

procedure TThImageItem.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

initialization
  RegisterItem(ItemFactoryIDImageFile, TThImageItem);

end.
