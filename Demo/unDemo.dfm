object Form1: TForm1
  Left = 230
  Top = 217
  Width = 233
  Height = 286
  Caption = 'Water Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 12
    Top = 48
    Width = 200
    Height = 200
    Visible = False
  end
  object Image2: TImage
    Left = 12
    Top = 48
    Width = 200
    Height = 200
    OnMouseDown = Image2MouseDown
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Start Simulation'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 122
    Top = 8
    Width = 99
    Height = 25
    Caption = 'Start/Stop Rain'
    TabOrder = 1
    OnClick = Button2Click
  end
  object LTWaterEffect1: TLTWaterEffect
    SrcImage = Image1
    DstImage = Image2
    Left = 176
    Top = 56
  end
end
