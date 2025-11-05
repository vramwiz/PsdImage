object FormPSDImage: TFormPSDImage
  Left = 0
  Top = 0
  Caption = 'FormPSDImage'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 624
    Height = 25
    Align = alTop
    Caption = 'PSD'#12501#12449#12452#12523#12398#35501#12415#36796#12415#12392#34920#31034#34920#31034
    TabOrder = 0
    OnClick = Button1Click
    ExplicitWidth = 622
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PSD'#12501#12449#12452#12523'(.*PSD)|*psd'
    Left = 64
    Top = 112
  end
end
