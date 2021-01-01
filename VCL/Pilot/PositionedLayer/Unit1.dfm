object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 556
  ClientWidth = 912
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
    Left = 648
    Top = 20
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Image32: TImage32
    Left = 32
    Top = 39
    Width = 849
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
    Left = 791
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Layers count'
    TabOrder = 3
    OnClick = Button4Click
  end
end
