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
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1035
    556)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 536
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 88
    Top = 536
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 216
    Top = 535
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label3'
  end
  object btnClear: TButton
    Left = 32
    Top = 8
    Width = 57
    Height = 25
    Caption = 'Clear'
    TabOrder = 0
    OnClick = btnClearClick
  end
  object Button3: TButton
    Left = 88
    Top = 8
    Width = 26
    Height = 25
    Caption = #8634
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 924
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Layers count'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Memo1: TMemo
    Left = 791
    Top = 39
    Width = 236
    Height = 498
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button2: TButton
    Left = 160
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ImgView: TImgView32
    Left = 32
    Top = 39
    Width = 753
    Height = 487
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 5
    OnMouseWheelDown = ImgViewMouseWheelDown
    OnMouseWheelUp = ImgViewMouseWheelUp
    OnResize = ImgViewResize
  end
  object Button1: TButton
    Left = 843
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 6
  end
  object Button5: TButton
    Left = 241
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 7
    OnClick = Button5Click
  end
end
