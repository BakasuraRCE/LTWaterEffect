object Form1: TForm1
  Left = 230
  Top = 217
  Caption = 'Water Demo'
  ClientHeight = 307
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    582
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 44
    Width = 566
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    Visible = False
  end
  object Image2: TImage
    Left = 8
    Top = 44
    Width = 566
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = Image2MouseDown
    OnMouseMove = Image2MouseMove
  end
  object Button1: TButton
    Left = 8
    Top = 6
    Width = 107
    Height = 25
    Caption = 'Start Simulation'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 475
    Top = 8
    Width = 99
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Start/Stop Rain'
    TabOrder = 1
    OnClick = Button2Click
  end
  object LTWaterEffect1: TLTWaterEffect
    SrcImage = Image1
    DstImage = Image2
    Left = 41
    Top = 54
  end
end
