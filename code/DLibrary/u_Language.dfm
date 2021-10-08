object frmLanguage: TfrmLanguage
  Left = 450
  Top = 226
  Width = 257
  Height = 311
  Caption = 'Language/Sprache/Lenguaje'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 178
    Top = 0
    Width = 71
    Height = 211
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TButton
      Left = 4
      Top = 8
      Width = 65
      Height = 25
      Caption = '&Ok'
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 4
      Top = 36
      Width = 65
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object sttBar: TStatusBar
    Left = 0
    Top = 258
    Width = 249
    Height = 19
    BorderWidth = 1
    Panels = <>
    SimplePanel = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 211
    Width = 249
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object outlblInfo1: TLabel
      Left = 4
      Top = 4
      Width = 33
      Height = 13
      Caption = 'WWW'
    end
    object outlblInfo2: TLabel
      Left = 4
      Top = 20
      Width = 33
      Height = 13
      Caption = 'WWW'
    end
    object outlblInfo3: TLabel
      Left = 4
      Top = 36
      Width = 33
      Height = 13
      Caption = 'WWW'
    end
  end
  object lisLanguages: TListBox
    Left = 40
    Top = 36
    Width = 77
    Height = 97
    ItemHeight = 20
    Style = lbOwnerDrawVariable
    TabOrder = 3
    OnClick = lisLanguagesClick
    OnDblClick = lisLanguagesDblClick
    OnDrawItem = lisLanguagesDrawItem
    OnMeasureItem = lisLanguagesMeasureItem
  end
end
