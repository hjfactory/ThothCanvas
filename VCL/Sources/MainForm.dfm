object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ThCanvas'
  ClientHeight = 757
  ClientWidth = 953
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
    953
    757)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 570
    Top = 26
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 216
    Top = 20
    Width = 33
    Height = 25
    Caption = 'Pen'
    TabOrder = 0
    OnClick = Button1Click
  end
  object pnlMain: TPanel
    Left = 32
    Top = 88
    Width = 881
    Height = 553
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 2
    BorderStyle = bsSingle
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
    Left = 368
    Top = 22
    Width = 145
    Height = 22
    TabOrder = 3
    OnChange = ColorBox1Change
  end
  object TrackBar1: TTrackBar
    Left = 519
    Top = 22
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
    Left = 579
    Top = 22
    Width = 54
    Height = 22
    Increment = 5
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 100
    OnChange = SpinEdit1Change
  end
  object Button3: TButton
    Left = 303
    Top = 20
    Width = 42
    Height = 25
    Caption = 'Clear'
    TabOrder = 6
    OnClick = Button3Click
  end
  object ToggleSwitch1: TToggleSwitch
    Left = 32
    Top = 29
    Width = 102
    Height = 20
    State = tssOn
    StateCaptions.CaptionOn = 'Freedraw'
    StateCaptions.CaptionOff = 'Selection'
    TabOrder = 7
    OnClick = ToggleSwitch1Click
  end
  object Button4: TButton
    Left = 255
    Top = 20
    Width = 42
    Height = 25
    Caption = 'Eraser'
    TabOrder = 8
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 255
    Top = 51
    Width = 33
    Height = 25
    Caption = 'Rect'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 216
    Top = 51
    Width = 33
    Height = 25
    Caption = 'None'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 294
    Top = 51
    Width = 29
    Height = 25
    Caption = 'Del'
    TabOrder = 11
    OnClick = Button7Click
  end
end
