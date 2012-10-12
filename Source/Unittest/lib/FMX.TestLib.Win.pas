{

  Reference
    - mouse_event : http://msdn.microsoft.com/en-us/library/windows/desktop/ms646260(v=vs.85).aspx
    - keybd_event : http://msdn.microsoft.com/en-us/library/windows/desktop/ms646304(v=vs.85).aspx
}

unit FMX.TestLib.Win;

interface

uses
  System.Types, System.UITypes, System.Classes, System.SysUtils, FMX.TestLib, FMX.Types, VCL.Forms, VCL.Graphics;

function GetTestLibClass: TTestLibClass;

implementation

uses
  Winapi.Windows, Winapi.ShellAPI, FMX.Platform;

type
  TTestLibWin = class(TTestLib)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(WheelDelta: Integer); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar); override;

    procedure TakeScreenshot(Dest: FMX.Types.TBitmap); override;
  end;

function GetTestLibClass: TTestLibClass;
begin
  Result := TTestLibWin;
end;

{ TTestLibWin }

procedure TTestLibWin.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  SetCursorPos(Round(X), Round(Y));
//  mouse_event(MOUSEEVENTF_MOVE, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Flag: DWORD;
begin
  case Button of
    TMouseButton.mbLeft: Flag := MOUSEEVENTF_LEFTDOWN;
    TMouseButton.mbRight: Flag := MOUSEEVENTF_RIGHTDOWN;
    TMouseButton.mbMiddle: Flag := MOUSEEVENTF_MIDDLEDOWN;
    else
      Flag := MOUSEEVENTF_LEFTDOWN;
  end;

  SetCursorPos(Round(X), Round(Y));
  mouse_event(Flag, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Flag: DWORD;
begin
  case Button of
    TMouseButton.mbLeft: Flag := MOUSEEVENTF_LEFTUP;
    TMouseButton.mbRight: Flag := MOUSEEVENTF_RIGHTUP;
    TMouseButton.mbMiddle: Flag := MOUSEEVENTF_MIDDLEUP;
    else
      Flag := MOUSEEVENTF_LEFTDOWN;
  end;

  SetCursorPos(Round(X), Round(Y));
  mouse_event(Flag, Round(X), Round(Y), 0, 0);
end;

procedure TTestLibWin.MouseWheel(WheelDelta: Integer);
begin
  inherited;

end;

procedure WriteWindowsToStream(AStream: TStream);
var
  dc: HDC; lpPal : PLOGPALETTE;
  bm: Vcl.Graphics.TBitMap;
begin
{test width and height}
  bm := Vcl.Graphics.TBitmap.Create;

  bm.Width := Screen.Width;
  bm.Height := Screen.Height;

  //get the screen dc
  dc := GetDc(0);
  if (dc = 0) then exit;
 //do we have a palette device?
  if (GetDeviceCaps(dc, RASTERCAPS) AND RC_PALETTE = RC_PALETTE) then
  begin
    //allocate memory for a logical palette
    GetMem(lpPal, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));
    //zero it out to be neat
    FillChar(lpPal^, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)), #0);
    //fill in the palette version
    lpPal^.palVersion := $300;
    //grab the system palette entries
    lpPal^.palNumEntries :=GetSystemPaletteEntries(dc,0,256,lpPal^.palPalEntry);
    if (lpPal^.PalNumEntries <> 0) then
    begin
      //create the palette
      bm.Palette := CreatePalette(lpPal^);
    end;
    FreeMem(lpPal, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));
  end;
  //copy from the screen to the bitmap
  BitBlt(bm.Canvas.Handle,0,0,Screen.Width,Screen.Height,Dc,0,0,SRCCOPY);

  bm.SaveToStream(AStream);

  FreeAndNil(bm);
  //release the screen dc
  ReleaseDc(0, dc);
end;

procedure TTestLibWin.TakeScreenshot(Dest: FMX.Types.TBitmap);
var
  Stream: TMemoryStream;
begin
  try
    Stream := TMemoryStream.Create;
    WriteWindowsToStream(Stream);
    Stream.Position := 0;
    Dest.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure TTestLibWin.KeyDown(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

procedure TTestLibWin.KeyUp(var Key: Word; var KeyChar: WideChar);
begin
  inherited;

end;

end.
