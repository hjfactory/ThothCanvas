object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ThCanvas'
  ClientHeight = 757
  ClientWidth = 1062
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1062
    757)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 338
    Top = 28
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 32
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object pnlMain: TPanel
    Left = 32
    Top = 64
    Width = 881
    Height = 577
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Button2: TButton
    Left = 784
    Top = 24
    Width = 129
    Height = 25
    Caption = 'Show debug form'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ColorBox1: TColorBox
    Left = 136
    Top = 24
    Width = 145
    Height = 22
    TabOrder = 3
    OnChange = ColorBox1Change
  end
  object TrackBar1: TTrackBar
    Left = 287
    Top = 24
    Width = 49
    Height = 20
    LineSize = 2
    Max = 30
    Min = 2
    Position = 10
    TabOrder = 4
    ThumbLength = 10
    TickMarks = tmTopLeft
    OnChange = TrackBar1Change
  end
  object SpinEdit1: TSpinEdit
    Left = 347
    Top = 24
    Width = 54
    Height = 22
    Increment = 5
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 100
    OnChange = SpinEdit1Change
  end
end
