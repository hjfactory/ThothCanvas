unit ThImageItem;

interface

uses
  System.Types, System.Classes, System.UITypes, System.SysUtils, System.UIConsts,
  FMX.Types, FMX.Objects, ThTypes, ThItem;

type
  TThImageItem = class(TThItem, IBitmapObject, IItemResizerObject)
  private
    FBitmap: TBitmap;
    procedure LoadImageFile;
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  protected
    function CreateResizer: IItemResizer; override;

    procedure Paint; override;

    function PtInItem(Pt: TPointF): Boolean; override;
    function GetMinimumSize: TPointF; virtual;

    property Bitmap: TBitmap read FBitmap write SetBitmap;
    procedure DoBitmapChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Dialogs, ThConsts, ThItemFactory, ThItemResizer, DebugUtils;

{ TThImageItem }

constructor TThImageItem.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap.Create(0, 0);
  FBitmap.OnChange := DoBitmapChanged;

  LoadImageFile;
end;

destructor TThImageItem.Destroy;
begin
//  FImage.Free;
  FBitmap.Free;

  inherited;
end;

procedure TThImageItem.DoBitmapChanged(Sender: TObject);
begin
  Repaint;
  UpdateEffects;
end;

function TThImageItem.CreateResizer: IItemResizer;
var
  Resizer: TThItemResizer;
begin
  Resizer := TThItemResizer.Create(Self);
  Resizer.SetSpotClass(TThItemCircleResizeSpot);
  Resizer.SetResizeSpots([scTopLeft, scTopRight, scBottomLeft, scBottomRight]);
  Resizer.OnTracking := nil;

  Result := Resizer;
end;

function TThImageItem.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TThImageItem.GetMinimumSize: TPointF;
var
  MinSize: Single;
begin
  MinSize := ItemMinimumSize / AbsoluteScale.X;

  Result := PointF(MinSize, MinSize);
end;

procedure TThImageItem.LoadImageFile;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Filter := TBitmapCodecManager.GetFilterString;
    if Dialog.Execute then
    begin
      FBitmap.LoadFromFile(Dialog.FileName);
      Width := FBitmap.Width;
      Height := FBitmap.Height;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TThImageItem.Paint;
var
  R: TRectF;
  B: TBitmap;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  B := FBitmap;
  if B.IsEmpty then
    Exit;

  R := LocalRect;
  Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
    AbsoluteOpacity, False)
end;

function TThImageItem.PtInItem(Pt: TPointF): Boolean;
begin
  Result := PtInRect(GetItemRect, Pt);

//  Debug('%f.%f - %f / %f.%f - %f', [GetItemRect.Left, GetItemRect.Right, Pt.X, GetItemRect.Top, GetItemRect.Bottom, Pt.Y]);
  if Result then
  begin
    Width := Width * 1;
  end;
end;

procedure TThImageItem.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

initialization
  RegisterItem(2000, TThImageItem);

end.
