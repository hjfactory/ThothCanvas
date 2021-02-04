object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ThCanvas'
  ClientHeight = 757
  ClientWidth = 1076
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
    1076
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
    Left = 48
    Top = 20
    Width = 33
    Height = 25
    Caption = 'Pen'
    TabOrder = 0
    OnClick = Button1Click
  end
  object pnlMain: TPanel
    Left = 48
    Top = 96
    Width = 1004
    Height = 635
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 2
    BorderStyle = bsSingle
    TabOrder = 1
  end
  object ColorBox1: TColorBox
    Left = 48
    Top = 54
    Width = 81
    Height = 22
    TabOrder = 2
    OnChange = ColorBox1Change
  end
  object TrackBar1: TTrackBar
    Left = 135
    Top = 51
    Width = 49
    Height = 20
    LineSize = 2
    Max = 30
    Min = 2
    Position = 10
    TabOrder = 3
    ThumbLength = 10
    TickMarks = tmTopLeft
    OnChange = TrackBar1Change
  end
  object SpinEdit1: TSpinEdit
    Left = 190
    Top = 54
    Width = 54
    Height = 22
    Increment = 5
    MaxValue = 100
    MinValue = 0
    TabOrder = 4
    Value = 100
    OnChange = SpinEdit1Change
  end
  object Button3: TButton
    Left = 135
    Top = 20
    Width = 42
    Height = 25
    Caption = 'Clear'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 87
    Top = 20
    Width = 42
    Height = 25
    Caption = 'Eraser'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 319
    Top = 20
    Width = 33
    Height = 25
    Caption = 'Rect'
    TabOrder = 7
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 280
    Top = 20
    Width = 33
    Height = 25
    Caption = 'None'
    TabOrder = 8
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 414
    Top = 20
    Width = 29
    Height = 25
    Caption = 'Del'
    TabOrder = 9
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 358
    Top = 21
    Width = 50
    Height = 25
    Caption = 'RndRct'
    TabOrder = 10
    OnClick = Button8Click
  end
  object SpinEdit2: TSpinEdit
    Left = 488
    Top = 54
    Width = 121
    Height = 22
    Increment = 10
    MaxValue = 0
    MinValue = 0
    TabOrder = 11
    Value = 100
    OnChange = SpinEdit2Change
  end
end
