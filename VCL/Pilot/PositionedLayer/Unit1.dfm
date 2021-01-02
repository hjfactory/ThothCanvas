object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 556
  ClientWidth = 1035
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 156
    Top = 14
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 456
    Top = 296
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 200
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 887
    Top = 20
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Image32: TImage32
    Left = 32
    Top = 39
    Width = 753
    Height = 498
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smScale
    TabOrder = 0
    OnMouseDown = Image32MouseDown
    OnMouseMove = Image32MouseMove
    OnMouseUp = Image32MouseUp
  end
  object Button1: TButton
    Left = 32
    Top = 8
    Width = 98
    Height = 25
    Caption = 'Clear'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 123
    Top = 8
    Width = 26
    Height = 25
    Caption = #8634
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 924
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Layers count'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button2: TButton
    Left = 392
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button5: TButton
    Left = 473
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 304
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 6
  end
  object Button7: TButton
    Left = 554
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Print'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Memo1: TMemo
    Left = 791
    Top = 39
    Width = 236
    Height = 498
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object SpinEdit1: TSpinEdit
    Left = 635
    Top = 8
    Width = 49
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
    OnKeyDown = SpinEdit1KeyDown
  end
  object SpinEdit2: TSpinEdit
    Left = 690
    Top = 8
    Width = 49
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 10
    Value = 0
    OnKeyDown = SpinEdit1KeyDown
  end
  object Button8: TButton
    Left = 824
    Top = 8
    Width = 42
    Height = 25
    Caption = 'DP'
    TabOrder = 11
    OnClick = Button8Click
  end
  object SpinEdit3: TSpinEdit
    Left = 769
    Top = 11
    Width = 49
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 12
    Value = 0
  end
  object CheckBox1: TCheckBox
    Left = 872
    Top = 8
    Width = 25
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 13
    OnClick = CheckBox1Click
  end
end
