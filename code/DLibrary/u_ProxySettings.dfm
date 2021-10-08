object frmProxySettings: TfrmProxySettings
  Left = 402
  Top = 304
  Width = 389
  Height = 206
  Caption = 'Proxy settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 12
    Top = 92
    Width = 124
    Height = 13
    Caption = 'Proxy name or IP Address:'
  end
  object lblPort: TLabel
    Left = 12
    Top = 120
    Width = 60
    Height = 13
    Caption = 'Port number:'
  end
  object lblInfo: TLabel
    Left = 12
    Top = 12
    Width = 250
    Height = 39
    Caption = 
      'If you connect to the Internet using proxy, fill in the followin' +
      'g settings. If you are unsure of either of these things, ask you' +
      'r network administrator.'
    WordWrap = True
  end
  object chkUseProxy: TCheckBox
    Left = 12
    Top = 144
    Width = 261
    Height = 17
    Caption = 'Use proxy'
    TabOrder = 3
  end
  object fldName: TEdit
    Left = 160
    Top = 88
    Width = 117
    Height = 21
    TabOrder = 0
  end
  object fldPort: TEdit
    Left = 160
    Top = 116
    Width = 101
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object spnPort: TUpDown
    Left = 261
    Top = 116
    Width = 16
    Height = 21
    Associate = fldPort
    Min = 0
    Max = 32767
    Position = 0
    TabOrder = 2
    Wrap = False
  end
  object btnAccept: TButton
    Left = 284
    Top = 12
    Width = 89
    Height = 25
    Caption = '&Accept'
    ModalResult = 1
    TabOrder = 4
    OnClick = btnAcceptClick
  end
  object btnCancel: TButton
    Left = 284
    Top = 44
    Width = 89
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
