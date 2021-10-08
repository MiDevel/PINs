object frmImport: TfrmImport
  Left = 244
  Top = 203
  BorderStyle = bsSingle
  Caption = 'Import from a text file'
  ClientHeight = 370
  ClientWidth = 612
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 509
    Height = 37
  end
  object lblDstPreview: TLabel
    Left = 12
    Top = 256
    Width = 139
    Height = 13
    Caption = 'Import results preview (first 5):'
  end
  object lblSeparator: TLabel
    Left = 404
    Top = 20
    Width = 49
    Height = 13
    Alignment = taRightJustify
    Caption = 'Separator:'
  end
  object lblFile: TLabel
    Left = 16
    Top = 20
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object btnOpen: TSpeedButton
    Left = 356
    Top = 16
    Width = 23
    Height = 22
    Hint = 'Select another text file'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000008080
      800000808000FFFFFF00C0C0C00000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000444444444444
      4444444444444444444400000000000004440022222222220444050222222222
      2044035022222222204405350222222222040353500000000004053535353504
      4444035353535304444405350000000440004000444444444400444444444044
      4040444444444400044444444444444444444444444444444444}
    ParentShowHint = False
    ShowHint = True
    OnClick = btnOpenClick
  end
  object lblSrcPreview: TLabel
    Left = 12
    Top = 156
    Width = 93
    Height = 13
    Caption = 'Source file preview:'
  end
  object lblIgnoreFirstRow: TLabel
    Left = 516
    Top = 154
    Width = 69
    Height = 13
    Alignment = taRightJustify
    Caption = 'Ignore first row'
  end
  object grdTest: TStringGrid
    Left = 8
    Top = 272
    Width = 597
    Height = 90
    ColCount = 9
    DefaultColWidth = 63
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 6
    TabOrder = 9
  end
  object btnTest: TButton
    Left = 528
    Top = 12
    Width = 77
    Height = 25
    Caption = '&Test'
    Default = True
    TabOrder = 0
    OnClick = btnTestClick
  end
  object btnImport: TButton
    Left = 528
    Top = 40
    Width = 77
    Height = 25
    Caption = '&Import'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 528
    Top = 68
    Width = 77
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object grpFldOrder: TGroupBox
    Left = 8
    Top = 48
    Width = 509
    Height = 101
    Caption = 'Order of fields in the text file'
    TabOrder = 6
    object outLabel2: TLabel
      Left = 16
      Top = 24
      Width = 6
      Height = 13
      Caption = '1'
    end
    object outLabel3: TLabel
      Left = 184
      Top = 24
      Width = 6
      Height = 13
      Caption = '4'
    end
    object outLabel4: TLabel
      Left = 352
      Top = 24
      Width = 6
      Height = 13
      Caption = '7'
    end
    object outLabel5: TLabel
      Left = 16
      Top = 48
      Width = 6
      Height = 13
      Caption = '2'
    end
    object outLabel6: TLabel
      Left = 184
      Top = 48
      Width = 6
      Height = 13
      Caption = '5'
    end
    object outLabel7: TLabel
      Left = 352
      Top = 48
      Width = 6
      Height = 13
      Caption = '8'
    end
    object outLabel8: TLabel
      Left = 16
      Top = 72
      Width = 6
      Height = 13
      Caption = '3'
    end
    object outLabel9: TLabel
      Left = 184
      Top = 72
      Width = 6
      Height = 13
      Caption = '6'
    end
    object outLabel10: TLabel
      Left = 352
      Top = 72
      Width = 6
      Height = 13
      Caption = '9'
    end
    object outcmbFld1: TComboBox
      Left = 28
      Top = 20
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 0
    end
    object outcmbFld4: TComboBox
      Left = 196
      Top = 20
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 3
    end
    object outcmbFld7: TComboBox
      Left = 364
      Top = 20
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 6
    end
    object outcmbFld2: TComboBox
      Left = 28
      Top = 44
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 1
    end
    object outcmbFld5: TComboBox
      Left = 196
      Top = 44
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 4
    end
    object outcmbFld8: TComboBox
      Left = 364
      Top = 44
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 7
    end
    object outcmbFld3: TComboBox
      Left = 28
      Top = 68
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 2
    end
    object outcmbFld6: TComboBox
      Left = 196
      Top = 68
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 5
    end
    object outcmbFld9: TComboBox
      Left = 364
      Top = 68
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 8
    end
  end
  object fldFileName: TEdit
    Left = 52
    Top = 16
    Width = 301
    Height = 21
    TabOrder = 4
    Text = 'fldFileName'
  end
  object fldSource: TMemo
    Left = 8
    Top = 172
    Width = 597
    Height = 81
    Lines.Strings = (
      'fldSource')
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object chkIgnoreFirstRow: TCheckBox
    Left = 588
    Top = 152
    Width = 17
    Height = 17
    Alignment = taLeftJustify
    TabOrder = 7
  end
  object cmbExpSep: TComboBox
    Left = 456
    Top = 16
    Width = 49
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    Text = ';'
  end
  object btnHelp: TButton
    Left = 528
    Top = 100
    Width = 77
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'pin'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Left = 480
    Top = 140
  end
end
