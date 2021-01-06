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
end
