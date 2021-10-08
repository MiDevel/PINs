object frmGenPassword: TfrmGenPassword
  Left = 242
  Top = 249
  BorderStyle = bsSingle
  Caption = 'Password generator'
  ClientHeight = 393
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    000001000200101010000000000028010000260000002020100000000000E802
    00004E0100002800000010000000200000000100040000000000C00000000000
    0000000000001000000000000000000000000000800000800000008080008000
    00008000800080800000C0C0C000808080000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000008888000000
    0000088888800888888888888888888888888888778888888888888877880000
    0000080000880000000000EEEE00000000000EEEEEE00E0E0E0E0EEE00E00EEE
    EEEEEEEE00E0000000000EEEEEE00000000000EEEE0000000000000000000000
    0000000000000000000000000000FFFFFFFFFFC3FFFFFF81FFFF8000FFFF0000
    FFFF0000FFFFFF80FFFFFF81FFFF8000FFFF0000FFFF0000FFFF0000FFFFFF81
    FFFFFFC3FFFFFFFFFFFFFFFFFFFF280000002000000040000000010004000000
    0000800200000000000000000000000000000000000000000000000080000080
    000000808000800000008000800080800000C0C0C000808080000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000888880000000000000000000000000
    8888888880000000000000000000000888888888880000008000008880800088
    8888888888800008888888888888808888888888888000888888888888888888
    8888888888880888888888888888888888888880008888888888888888888888
    8888888000880888888888888888888888888880008800888888888888888888
    8888888888880000000000000000008888888888888000000000000000000088
    88000008888000000000000000000008007E7E70080000000000000000000000
    E7E7E7E7E0800000000000000000008E7E7E7E7E7E800000EF0007EFE00F0007
    E7E7E7F777080000000000000000007E7E7E7E00077000E7E7E7E7E7E7E7E7E7
    E7E7E00000E00E7E7E7E7E7E7E7E7E7E7E7E7000007007F7F7F7F7F7F7F7F7F7
    E7E7E07770E00000000000000000007E7E7E7E000E7800000000000000000007
    E7E7E7E7E7080000000000000000008F7E7E7E7E7E8000000000000000000000
    FFE7E7E7E0000000000000000000000080FF7F70800000000000000000000000
    0080008000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFC1FFFFFF007FFFFE003F7C5C001E0004001C00000008000001C0000
    001C8000001CC0000000FFFFC001FFFFC001FFFFE003FFFFE001F384C001E000
    4000C000000080000038000000380000000080000000FFFFC000FFFFC001FFFF
    E003FFFFF007FFFFFC1FFFFFFFFFFFFFFFFFFFFFFFFF}
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 360
    Top = 40
    Width = 77
    Height = 25
    Caption = '&Accept'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 360
    Top = 68
    Width = 77
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object grpMethod: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 113
    Caption = 'Password generating method'
    TabOrder = 0
    object rdiPswLen: TRadioButton
      Left = 12
      Top = 72
      Width = 141
      Height = 17
      Caption = 'Password &length:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = rdiPswLenClick
    end
    object rdiPswTpl: TRadioButton
      Left = 12
      Top = 32
      Width = 141
      Height = 17
      Caption = '&Template:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdiPswTplClick
    end
    object fldPswLen: TEdit
      Left = 156
      Top = 68
      Width = 33
      Height = 21
      TabOrder = 3
      Text = '9'
    end
    object fldPswTpl: TEdit
      Left = 156
      Top = 28
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'Cvccvc99'
    end
    object spnPswLen: TUpDown
      Left = 189
      Top = 68
      Width = 15
      Height = 21
      Associate = fldPswLen
      Min = 3
      Max = 56
      Position = 9
      TabOrder = 4
      Wrap = False
    end
    object btnPswTplHelp: TButton
      Left = 312
      Top = 52
      Width = 22
      Height = 21
      Hint = 'Help on building templates'
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      TabStop = False
      OnClick = btnPswTplHelpClick
    end
    object btnCommonTemplates: TButton
      Left = 312
      Top = 28
      Width = 22
      Height = 21
      Hint = 'Library of common templates'
      Caption = '&T'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      TabStop = False
      OnClick = btnCommonTemplatesClick
    end
  end
  object grpAllowedChars: TGroupBox
    Left = 8
    Top = 128
    Width = 429
    Height = 101
    Caption = 'Password can contain'
    TabOrder = 5
    object chkAllowLCase: TCheckBox
      Left = 12
      Top = 20
      Width = 189
      Height = 17
      Caption = 'Lowercase characters'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkAllowUCase: TCheckBox
      Left = 12
      Top = 44
      Width = 189
      Height = 17
      Caption = 'Uppercase characters'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkAllowDigits: TCheckBox
      Left = 196
      Top = 20
      Width = 189
      Height = 17
      Caption = 'Digits'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkAllowSymbols: TCheckBox
      Left = 196
      Top = 44
      Width = 189
      Height = 17
      Caption = 'Symbols  !@#$%^&&*_-+=?([{}])'
      TabOrder = 3
    end
    object chkAllowUserChars: TCheckBox
      Left = 12
      Top = 72
      Width = 185
      Height = 17
      Caption = 'User-defined characters:'
      TabOrder = 4
      OnClick = chkAllowUserCharsClick
    end
    object fldPswUserChars: TEdit
      Left = 196
      Top = 68
      Width = 197
      Height = 21
      TabOrder = 5
      Text = '©®§'
    end
    object btnCharMap: TBitBtn
      Left = 395
      Top = 68
      Width = 22
      Height = 21
      Hint = 'Character map'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      TabStop = False
      OnClick = btnCharMapClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000C40E0000C40E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888800008888888888807880088888888888800
        0788888000887000888888008008008870888888007880000888888000888888
        8888880088088880888888088008880808888800008888888888888008888888
        8888880080088888888888800088888888888888888888888888}
    end
  end
  object grpPasswords: TGroupBox
    Left = 8
    Top = 236
    Width = 429
    Height = 149
    Caption = 'Select from the list below the password you like most'
    TabOrder = 6
    object lblPassword: TLabel
      Left = 12
      Top = 28
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object lblCount: TLabel
      Left = 12
      Top = 100
      Width = 31
      Height = 13
      Caption = 'Count:'
    end
    object fldPsw: TEdit
      Left = 84
      Top = 21
      Width = 309
      Height = 26
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object btnClipCopy: TBitBtn
      Left = 395
      Top = 21
      Width = 22
      Height = 24
      Hint = 'Put password on clipboard'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = btnClipCopyClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADADADADADADDADADA444444444AADADAD4FFFFFFF4DDADADA4F0000
        0F4A0000004FFFFFFF4D0FFFFF4F00000F4A0F00004FFFFFFF4D0FFFFF4F00F4
        444A0F00004FFFF4F4AD0FFFFF4FFFF44ADA0F00F0444444ADAD0FFFF0F0DADA
        DADA0FFFF00DADADADAD000000DADADADADAADADADADADADADAD}
    end
    object lisAllPsw: TListBox
      Left = 84
      Top = 48
      Width = 309
      Height = 89
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 16
      ParentFont = False
      TabOrder = 2
      OnClick = lisAllPswClick
    end
    object btnClipCopyAll: TBitBtn
      Left = 395
      Top = 113
      Width = 22
      Height = 24
      Hint = 'Put all passwords on clipboard'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      TabStop = False
      OnClick = btnClipCopyAllClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADADADADADADDADADA444444444AADADAD4FFFFFFF4DDADADA4F0000
        0F4A0000004FFFFFFF4D0FFFFF4F00000F4A0F00004FFFFFFF4D0FFFFF4F00F4
        444A0F00004FFFF4F4AD0FFFFF4FFFF44ADA0F00F0444444ADAD0FFFF0F0DADA
        DADA0FFFF00DADADADAD000000DADADADADAADADADADADADADAD}
    end
    object fldPswGenCnt: TEdit
      Left = 12
      Top = 116
      Width = 49
      Height = 21
      TabOrder = 4
      Text = '20'
    end
    object spnPswGenCnt: TUpDown
      Left = 61
      Top = 116
      Width = 15
      Height = 21
      Hint = 'Count of passwords to be generated'
      Associate = fldPswGenCnt
      Min = 10
      Max = 32000
      Position = 20
      TabOrder = 5
      Wrap = False
    end
  end
  object btnHelp: TButton
    Left = 361
    Top = 96
    Width = 76
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object btnGenerate: TButton
    Left = 360
    Top = 12
    Width = 77
    Height = 25
    Caption = '&Generate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnGenerateClick
  end
  object popupTemplates: TPopupMenu
    OnPopup = popupTemplatesPopup
    Left = 276
    Top = 84
  end
end
