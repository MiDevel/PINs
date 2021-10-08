object frmExport: TfrmExport
  Left = 261
  Top = 368
  BorderStyle = bsSingle
  Caption = 'Export'
  ClientHeight = 280
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblExpSep: TLabel
    Left = 12
    Top = 240
    Width = 77
    Height = 13
    Caption = 'Fields separator:'
  end
  object grpFields: TGroupBox
    Left = 12
    Top = 12
    Width = 161
    Height = 213
    Caption = 'Fields to export'
    TabOrder = 0
    object outchkExpCat: TCheckBox
      Left = 12
      Top = 20
      Width = 125
      Height = 17
      Caption = 'Category'
      TabOrder = 0
    end
    object outchkExpUsr: TCheckBox
      Left = 12
      Top = 60
      Width = 125
      Height = 17
      Caption = 'User'
      TabOrder = 2
    end
    object outchkExpSys: TCheckBox
      Left = 12
      Top = 40
      Width = 125
      Height = 17
      Caption = 'System'
      TabOrder = 1
    end
    object outchkExpPsw: TCheckBox
      Left = 12
      Top = 80
      Width = 125
      Height = 17
      Caption = 'Password'
      TabOrder = 3
    end
    object outchkExpSta: TCheckBox
      Left = 12
      Top = 140
      Width = 125
      Height = 17
      Caption = 'Start date'
      TabOrder = 6
    end
    object outchkExpExp: TCheckBox
      Left = 12
      Top = 160
      Width = 125
      Height = 17
      Caption = 'Expires'
      TabOrder = 7
    end
    object outchkExpCmm: TCheckBox
      Left = 12
      Top = 100
      Width = 125
      Height = 17
      Caption = 'Comments'
      TabOrder = 4
    end
    object outchkExpInf: TCheckBox
      Left = 12
      Top = 180
      Width = 125
      Height = 17
      Caption = 'More info'
      TabOrder = 8
    end
    object outchkExpCst: TCheckBox
      Left = 12
      Top = 120
      Width = 125
      Height = 17
      Caption = 'Custom'
      TabOrder = 5
    end
  end
  object cmbExpSep: TComboBox
    Left = 124
    Top = 232
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = ';'
  end
  object grpCategories: TGroupBox
    Left = 180
    Top = 12
    Width = 169
    Height = 213
    Caption = 'Categories to export'
    TabOrder = 1
    object lisCategories: TCheckListBox
      Left = 12
      Top = 20
      Width = 145
      Height = 181
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
  end
  object OKBtn: TButton
    Left = 360
    Top = 16
    Width = 73
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 360
    Top = 44
    Width = 73
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 360
    Top = 72
    Width = 73
    Height = 25
    Caption = '&Help'
    TabOrder = 6
    OnClick = btnHelpClick
  end
  object sttBar: TStatusBar
    Left = 0
    Top = 261
    Width = 441
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object chkExpQuoteTexts: TCheckBox
    Left = 184
    Top = 236
    Width = 253
    Height = 17
    Caption = 'Quote (" ") texts?'
    TabOrder = 3
  end
end
