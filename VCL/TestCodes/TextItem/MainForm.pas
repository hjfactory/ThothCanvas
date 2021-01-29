unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TTextItem = class(TCustomControl)
  private
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;

    procedure WMImeChar(var Msg: TMessage); message WM_IME_CHAR;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;

    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Msg: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;

    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
  private
    FMyText: string;
    procedure InitializeCaret;

    procedure Process(Value: WideString);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTextItem: TTextItem;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  Winapi.Imm,
  System.WideStrUtils;
//  Winapi.Windows;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  FTextItem := TTextItem.Create(Self);
  FTextItem.Parent := Self;
  FTextItem.Left := 100;
  FTextItem.Top := 100;
//  FTextItem.Text := 'test';
end;

{ TTextItem }

constructor TTextItem.Create(AOwner: TComponent);
begin
  inherited;

  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clDefault;

  ControlStyle := ControlStyle + [csOpaque, csSetCaption];

  ParentFont := False;
  ParentColor := False;
  TabStop := True;
end;

procedure TTextItem.CreateParams(var Params: TCreateParams);
begin
  StrDispose(WindowText);
  WindowText := nil;

  inherited CreateParams(Params);
//
//  with Params do
//  begin
//    Style := Style or WS_BORDER or WS_CLIPCHILDREN;
//
//    if NewStyleControls and Ctl3D then
//    begin
//      Style := Style and not WS_BORDER;
//      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
//      // avoid flicker while scrolling or resizing
//      if not (csDesigning in ComponentState) and CheckWin32Version(5, 1) then
//        ExStyle := ExStyle or WS_EX_COMPOSITED;
//    end;
//  end;
end;

destructor TTextItem.Destroy;
begin
  inherited Destroy;
end;

procedure TTextItem.InitializeCaret;
begin
  CreateCaret(Handle, 0, 2, 14);
  SetCaretPos(10, 10);
  ShowCaret(Handle);
end;

procedure TTextItem.Paint;
begin
  inherited;

  Canvas.TextOut(10, 10, FMyText);
end;

procedure TTextItem.Process(Value: WideString);
begin
  OutputDebugString(PChar(Value));
end;

procedure TTextItem.WMChar(var Msg: TWMChar);
var
  C: Char;
begin
  C := Char(Msg.CharCode);

  Text := Text + C;
end;

procedure TTextItem.WMDestroy(var Message: TWMDestroy);
begin
//  if WindowText = nil then
//     WindowText := 'test';

  inherited;
end;

procedure TTextItem.WMGetText(var Msg: TWMGetText);
begin
  WStrLCopy(PWideChar(Msg.Text), PWideChar(Text), Msg.TextMax - 1);
  Msg.Result := WStrLen(PWideChar(Msg.Text));
end;

procedure TTextItem.WMGetTextLength(var Msg: TWMGetTextLength);
begin
//  if csDocking in ControlState then
//    Msg.Result := 0
//  else
//    Msg.Result := Length(Text);
end;

procedure TTextItem.WMImeChar(var Msg: TMessage);
begin

end;

procedure TTextItem.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  PW: PWideChar;
  PA: PAnsiChar;
  PWLength: Integer;
  ImeCount: Integer;
begin
  if (Msg.LParam and GCS_RESULTSTR) <> 0 then
  begin
    imc := ImmGetContext(Handle);
    try
      ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
      // ImeCount is always the size in bytes, also for Unicode
      GetMem(PW, ImeCount + sizeof(WideChar));
      try
        ImmGetCompositionStringW(imc, GCS_RESULTSTR, PW, ImeCount);
        PW[ImeCount div sizeof(WideChar)] := #0;
        Process(PW);
//        CommandProcessor(ecImeStr, #0, PW);
      finally
        FreeMem(PW);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TTextItem.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  LogFontW: TLogFontW;
  LogFontA: TLogFontA;
begin
  with Msg do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if imc <> 0 then
          begin
            GetObjectW(Font.Handle, SizeOf(TLogFontW), @LogFontW);
            ImmSetCompositionFontW(imc, @LogFontW);
            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;

procedure TTextItem.WMSetFocus(var Msg: TWMSetFocus);
begin
  InitializeCaret;
end;

procedure TTextItem.WMSetText(var Msg: TWMSetText);
begin
  Msg.Result := 1;
  try
    FMyText := FMyText + PChar(Msg.Text);
    InvaliDate;
  except
    Msg.Result := 0;
    raise
  end
end;

end.
