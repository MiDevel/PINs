object frmHotkeys: TfrmHotkeys
  Left = 530
  Top = 130
  BorderStyle = bsDialog
  Caption = 'System-wide Hotkeys'
  ClientHeight = 193
  ClientWidth = 445
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
  object Bevel1: TBevel
    Left = 8
    Top = 12
    Width = 349
    Height = 173
  end
  object lblSPaste: TLabel
    Left = 88
    Top = 60
    Width = 57
    Height = 13
    Alignment = taRightJustify
    Caption = 'Super paste'
  end
  object lblUPaste: TLabel
    Left = 63
    Top = 92
    Width = 79
    Height = 13
    Alignment = taRightJustify
    Caption = 'Paste user name'
  end
  object lblAlt: TLabel
    Left = 180
    Top = 28
    Width = 16
    Height = 13
    Caption = 'Alt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblShift: TLabel
    Left = 204
    Top = 28
    Width = 27
    Height = 13
    Caption = 'Shift'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCtrl: TLabel
    Left = 148
    Top = 28
    Width = 20
    Height = 13
    Caption = 'Ctrl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblKey: TLabel
    Left = 240
    Top = 28
    Width = 22
    Height = 13
    Caption = 'Key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblActive: TLabel
    Left = 300
    Top = 28
    Width = 37
    Height = 13
    Alignment = taCenter
    Caption = 'Active'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 24
    Top = 40
    Width = 317
    Height = 6
    Shape = bsBottomLine
  end
  object lblPPaste: TLabel
    Left = 67
    Top = 124
    Width = 75
    Height = 13
    Alignment = taRightJustify
    Caption = 'Paste password'
  end
  object lblCPaste: TLabel
    Left = 25
    Top = 156
    Width = 117
    Height = 13
    Alignment = taRightJustify
    Caption = 'Paste clipboard contents'
  end
  object chkSPaste_Ctrl: TCheckBox
    Left = 152
    Top = 60
    Width = 21
    Height = 17
    TabOrder = 0
  end
  object chkSPaste_Alt: TCheckBox
    Left = 180
    Top = 60
    Width = 21
    Height = 17
    TabOrder = 1
  end
  object chkSPaste_Shift: TCheckBox
    Left = 208
    Top = 60
    Width = 21
    Height = 17
    TabOrder = 2
  end
  object outcmbSPaste_Key: TComboBox
    Left = 240
    Top = 56
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = 'outcmbSPaste_Key'
  end
  object chkSPaste_Active: TCheckBox
    Left = 312
    Top = 60
    Width = 21
    Height = 17
    TabOrder = 4
  end
  object chkUPaste_Ctrl: TCheckBox
    Left = 152
    Top = 92
    Width = 21
    Height = 17
    TabOrder = 5
  end
  object chkUPaste_Alt: TCheckBox
    Left = 180
    Top = 92
    Width = 21
    Height = 17
    TabOrder = 6
  end
  object chkUPaste_Shift: TCheckBox
    Left = 208
    Top = 92
    Width = 21
    Height = 17
    TabOrder = 7
  end
  object outcmbUPaste_Key: TComboBox
    Left = 240
    Top = 88
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 8
    Text = 'ComboBox1'
  end
  object chkUPaste_Active: TCheckBox
    Left = 312
    Top = 92
    Width = 21
    Height = 17
    TabOrder = 9
  end
  object btnOk: TButton
    Left = 368
    Top = 12
    Width = 69
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 10
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 368
    Top = 68
    Width = 69
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object btnApply: TButton
    Left = 368
    Top = 40
    Width = 69
    Height = 25
    Caption = '&Apply'
    TabOrder = 11
    OnClick = btnApplyClick
  end
  object btnDefault: TButton
    Left = 368
    Top = 96
    Width = 69
    Height = 25
    Caption = '&Default'
    TabOrder = 13
    OnClick = btnDefaultClick
  end
  object btnHelp: TButton
    Left = 368
    Top = 128
    Width = 69
    Height = 25
    Caption = '&Help'
    TabOrder = 14
    OnClick = btnHelpClick
  end
  object chkPPaste_Ctrl: TCheckBox
    Left = 152
    Top = 124
    Width = 21
    Height = 17
    TabOrder = 15
  end
  object chkPPaste_Alt: TCheckBox
    Left = 180
    Top = 124
    Width = 21
    Height = 17
    TabOrder = 16
  end
  object chkPPaste_Shift: TCheckBox
    Left = 208
    Top = 124
    Width = 21
    Height = 17
    TabOrder = 17
  end
  object outcmbPPaste_Key: TComboBox
    Left = 240
    Top = 120
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 18
    Text = 'outcmbPPaste_Key'
  end
  object chkPPaste_Active: TCheckBox
    Left = 312
    Top = 124
    Width = 21
    Height = 17
    TabOrder = 19
  end
  object chkCPaste_Ctrl: TCheckBox
    Left = 152
    Top = 156
    Width = 21
    Height = 17
    TabOrder = 20
  end
  object chkCPaste_Alt: TCheckBox
    Left = 180
    Top = 156
    Width = 21
    Height = 17
    TabOrder = 21
  end
  object chkCPaste_Shift: TCheckBox
    Left = 208
    Top = 156
    Width = 21
    Height = 17
    TabOrder = 22
  end
  object outcmbCPaste_Key: TComboBox
    Left = 240
    Top = 152
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 23
    Text = 'outcmbPPaste_Key'
  end
  object chkCPaste_Active: TCheckBox
    Left = 312
    Top = 156
    Width = 21
    Height = 17
    TabOrder = 24
  end
end
