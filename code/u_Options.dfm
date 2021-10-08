object frmOptions: TfrmOptions
  Left = 287
  Top = 113
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 281
  ClientWidth = 460
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
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 379
    Top = 0
    Width = 81
    Height = 281
    Align = alRight
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object OKBtn: TButton
      Left = 4
      Top = 12
      Width = 73
      Height = 25
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 4
      Top = 40
      Width = 73
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnApply: TButton
      Left = 4
      Top = 68
      Width = 73
      Height = 25
      Caption = '&Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnHelp: TButton
      Left = 4
      Top = 100
      Width = 73
      Height = 25
      Caption = '&Help'
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
  object tabOptions: TPageControl
    Left = 5
    Top = 5
    Width = 372
    Height = 272
    ActivePage = tabGeneral
    MultiLine = True
    TabOrder = 1
    object tabGeneral: TTabSheet
      Caption = 'General settings'
      object Bevel2: TBevel
        Left = 8
        Top = 8
        Width = 349
        Height = 209
      end
      object lblDblClkAction: TLabel
        Left = 20
        Top = 184
        Width = 94
        Height = 13
        Caption = 'Double-click action:'
      end
      object chkAddSamples: TCheckBox
        Left = 20
        Top = 44
        Width = 333
        Height = 17
        Caption = 'Add sample records to new empty data files'
        TabOrder = 1
      end
      object chkUseTray: TCheckBox
        Left = 20
        Top = 64
        Width = 333
        Height = 17
        Caption = 'Minimize to tray'
        TabOrder = 2
      end
      object chkActiveHyperlinks: TCheckBox
        Left = 20
        Top = 24
        Width = 333
        Height = 17
        Caption = 'Active hyperlinks within the info box'
        TabOrder = 0
      end
      object cmbDblClkAction: TComboBox
        Left = 168
        Top = 180
        Width = 177
        Height = 21
        ItemHeight = 13
        TabOrder = 5
      end
      object chkAutoExpand: TCheckBox
        Left = 20
        Top = 104
        Width = 333
        Height = 17
        Caption = 'Auto-expand focused items'
        TabOrder = 4
      end
      object chkAutoCopyPsw: TCheckBox
        Left = 20
        Top = 84
        Width = 333
        Height = 17
        Caption = 'Copy password to Clipboard on URL lanunch'
        TabOrder = 3
      end
    end
    object tabSecurity: TTabSheet
      Caption = 'Security'
      ImageIndex = 1
      object Bevel3: TBevel
        Left = 8
        Top = 8
        Width = 349
        Height = 209
      end
      object lblMaxInactivity: TLabel
        Left = 20
        Top = 188
        Width = 167
        Height = 13
        Caption = 'Inactivity time to lock the database:'
      end
      object lblMinutes: TLabel
        Left = 300
        Top = 188
        Width = 19
        Height = 13
        Caption = 'min.'
      end
      object chkShowPswCol: TCheckBox
        Left = 20
        Top = 24
        Width = 329
        Height = 17
        Caption = 'Show passwords in the grid'
        TabOrder = 0
      end
      object chkShowPswFld: TCheckBox
        Left = 20
        Top = 44
        Width = 329
        Height = 17
        Caption = 'Show passwords in the editing dialog'
        TabOrder = 1
      end
      object chkClipClrExit: TCheckBox
        Left = 20
        Top = 64
        Width = 329
        Height = 17
        Caption = 'Empty the clipboard on exit'
        TabOrder = 2
      end
      object chkClipClrMini: TCheckBox
        Left = 20
        Top = 84
        Width = 329
        Height = 17
        Caption = 'Empty the clipboard on minimize'
        TabOrder = 3
      end
      object chkLockOnMinimize: TCheckBox
        Left = 20
        Top = 104
        Width = 329
        Height = 17
        Caption = 'Lock the database on minimize'
        TabOrder = 4
      end
      object chkSaveOnMinim: TCheckBox
        Left = 20
        Top = 124
        Width = 329
        Height = 17
        Caption = 'Prompt to save any unsaved changes on minimize'
        TabOrder = 5
      end
      object fldMaxInactivity: TEdit
        Left = 248
        Top = 184
        Width = 33
        Height = 21
        TabOrder = 7
        Text = '5'
      end
      object spnMaxInactivity: TUpDown
        Left = 281
        Top = 184
        Width = 16
        Height = 21
        Associate = fldMaxInactivity
        Max = 32767
        Position = 5
        TabOrder = 8
      end
      object chkPasteInLocked: TCheckBox
        Left = 20
        Top = 144
        Width = 329
        Height = 17
        Caption = 'Password pasting active in locked mode'
        TabOrder = 6
      end
    end
    object tabStartUp: TTabSheet
      Caption = 'Startup'
      ImageIndex = 2
      object Bevel5: TBevel
        Left = 8
        Top = 8
        Width = 349
        Height = 209
      end
      object lblExpPrompt: TLabel
        Left = 39
        Top = 156
        Width = 187
        Height = 13
        Caption = 'Report also passwords that will expire in'
      end
      object lblExpDays: TLabel
        Left = 308
        Top = 156
        Width = 22
        Height = 13
        Caption = 'days'
      end
      object chkReloadData: TCheckBox
        Left = 20
        Top = 24
        Width = 333
        Height = 17
        Caption = 'Reload at startup last edited data file'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkCheckExpDate: TCheckBox
        Left = 20
        Top = 132
        Width = 333
        Height = 17
        Caption = 'Auto-check for expired passwords after opening data file'
        TabOrder = 1
      end
      object fldExpDateDays: TEdit
        Left = 264
        Top = 152
        Width = 25
        Height = 21
        TabOrder = 2
        Text = '7'
      end
      object spnExpDateDays: TUpDown
        Left = 289
        Top = 152
        Width = 16
        Height = 21
        Associate = fldExpDateDays
        Max = 365
        Position = 7
        TabOrder = 3
      end
      object chkStartExpanded: TCheckBox
        Left = 20
        Top = 48
        Width = 333
        Height = 17
        Caption = 'Expand all categories after opening data files'
        TabOrder = 4
      end
    end
    object tabSaving: TTabSheet
      Caption = 'Saving'
      ImageIndex = 3
      object Bevel1: TBevel
        Left = 8
        Top = 8
        Width = 349
        Height = 209
      end
      object lblNumMRUFil: TLabel
        Left = 23
        Top = 180
        Width = 169
        Height = 13
        Caption = 'Number of recent files on File menu:'
      end
      object chkMakeBAK: TCheckBox
        Left = 20
        Top = 48
        Width = 309
        Height = 17
        Caption = 'Make .BAK copies from last saved data files'
        TabOrder = 0
      end
      object chkSaveArchCopy: TCheckBox
        Left = 20
        Top = 72
        Width = 309
        Height = 17
        Caption = 'Save additional archival copy to:'
        TabOrder = 1
      end
      object fldSaveArchCopyTo: TEdit
        Left = 40
        Top = 92
        Width = 257
        Height = 21
        TabOrder = 2
        Text = 'a:\'
      end
      object btnSelectFolder: TBitBtn
        Left = 299
        Top = 92
        Width = 21
        Height = 21
        Hint = 'Select your archival folder'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = btnSelectFolderClick
      end
      object chkAutoSave: TCheckBox
        Left = 20
        Top = 24
        Width = 309
        Height = 17
        Caption = 'Auto save on exit'
        TabOrder = 4
      end
      object spnNumMRUFil: TUpDown
        Left = 305
        Top = 176
        Width = 16
        Height = 21
        Associate = fldNumMRUFil
        Max = 30
        TabOrder = 5
      end
      object fldNumMRUFil: TEdit
        Left = 276
        Top = 176
        Width = 29
        Height = 21
        TabOrder = 6
        Text = '0'
      end
    end
    object tabLayout: TTabSheet
      Caption = 'Layout'
      ImageIndex = 4
      object grpGrid: TGroupBox
        Left = 4
        Top = 4
        Width = 353
        Height = 213
        TabOrder = 0
        object lblPreview: TLabel
          Left = 232
          Top = 28
          Width = 38
          Height = 13
          Caption = 'Preview'
        end
        object grdSample: TElTree
          Left = 232
          Top = 44
          Width = 105
          Height = 73
          AlwaysKeepSelection = False
          ExpandOnDblClick = False
          HeaderHeight = 21
          HeaderHotTrack = False
          HeaderSections.Data = {
            F4FFFFFF03000000E8E11A0700000000FFFFFFFF000000010100636F28000000
            00000000102700000001007378B7CB0200000000010066690000000000000107
            0000000000000000000100000000000000000000080000004578744374726C73
            600000001B00000000000000080000004578744374726C7300009304C86B1D07
            20B5D3041800000084942B071000000090000000020000006800010000000000
            00000000000000E8E11A0700000000FFFFFFFF000001010100636F3200000000
            0000001027000000010073A078CB020100000001006669000000000000010700
            00000000000000000100000000000000000000080000004578744374726C7360
            0000001B00000000000000080000004578744374726C7300009304C86B1D0720
            B5D3041800000084942B07100000009000000004000000416263000100000000
            0000000000000000E8E11A0700000000FFFFFFFF000001010100636F32000000
            00000000102700000001007340ACF80102000000000066690000000000000107
            0000000000000000000100000000000000000000080000004578744374726C73
            600000001B00000000000000080000004578744374726C7300009304C86B1D07
            20B5D3041800000084942B071000000090000000040000004465660001000000
            000000000000000000}
          HideHorzScrollBar = True
          HideVertScrollBar = True
          HorzScrollBarStyles.ShowTrackHint = False
          HorzScrollBarStyles.Width = 17
          HorzScrollBarStyles.ButtonSize = 17
          LineHeight = 17
          LinesStyle = psSolid
          MultiSelect = False
          ShowColumns = True
          ShowLeafButton = False
          SortType = stCustom
          TabOrder = 2
          TabStop = True
          VertScrollBarStyles.ShowTrackHint = True
          VertScrollBarStyles.Width = 17
          VertScrollBarStyles.ButtonSize = 17
        end
        object lisLayItems: TListBox
          Left = 12
          Top = 28
          Width = 205
          Height = 141
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = btnLayChangeClick
        end
        object btnLayChange: TButton
          Left = 12
          Top = 176
          Width = 205
          Height = 25
          Caption = '&Change'
          TabOrder = 1
          OnClick = btnLayChangeClick
        end
        object fldInfoBoxSample: TmwRichEdit
          Left = 232
          Top = 120
          Width = 105
          Height = 49
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 3
        end
        object btnLayDefault: TButton
          Left = 232
          Top = 176
          Width = 105
          Height = 25
          Caption = '&Defaults'
          TabOrder = 4
          OnClick = btnLayDefaultClick
        end
      end
    end
    object tabColumns: TTabSheet
      Caption = 'Columns'
      ImageIndex = 5
      object grpViewNameColumns: TGroupBox
        Left = 4
        Top = 4
        Width = 353
        Height = 213
        Caption = 'Visibility and names of columns'
        TabOrder = 0
        object lvColViewNames: TListView
          Left = 12
          Top = 20
          Width = 329
          Height = 153
          Checkboxes = True
          Columns = <
            item
              Caption = 'Column visibility'
              Width = 160
            end
            item
              Caption = 'Custom name'
              Width = 150
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
        object chkUserColNames: TCheckBox
          Left = 12
          Top = 184
          Width = 185
          Height = 17
          Caption = 'Use custom column names'
          TabOrder = 2
        end
        object btnColRename: TButton
          Left = 200
          Top = 180
          Width = 141
          Height = 25
          Caption = 'Change column'#39's name'
          TabOrder = 1
          OnClick = btnColRenameClick
        end
      end
    end
    object tabSounds: TTabSheet
      Caption = 'Sounds'
      ImageIndex = 6
      object Bevel4: TBevel
        Left = 8
        Top = 8
        Width = 349
        Height = 209
      end
      object btnSoundWavePath: TSpeedButton
        Left = 324
        Top = 184
        Width = 21
        Height = 21
        Hint = 'Select your .wav file'
        Caption = '...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSoundWavePathClick
      end
      object lisSounds: TListBox
        Left = 24
        Top = 36
        Width = 321
        Height = 89
        ItemHeight = 13
        TabOrder = 0
        OnClick = lisSoundsClick
        OnDblClick = lisSoundsDblClick
      end
      object chkSoundOn: TCheckBox
        Left = 24
        Top = 16
        Width = 289
        Height = 17
        Caption = 'Sound enabled'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
      end
      object chkOneSoundActive: TCheckBox
        Left = 24
        Top = 128
        Width = 181
        Height = 17
        Caption = 'This sound is active'
        TabOrder = 1
      end
      object rdiSoundBuiltIn: TRadioButton
        Left = 24
        Top = 148
        Width = 161
        Height = 17
        Caption = 'Play built-in sound'
        TabOrder = 2
      end
      object rdiSoundWaveFile: TRadioButton
        Left = 24
        Top = 164
        Width = 169
        Height = 17
        Caption = 'Play this .wav file:'
        TabOrder = 3
      end
      object fldSoundWavePath: TEdit
        Left = 24
        Top = 184
        Width = 297
        Height = 21
        TabOrder = 4
      end
      object btnSoundTest: TButton
        Left = 284
        Top = 132
        Width = 61
        Height = 21
        Caption = '&Test'
        TabOrder = 5
        OnClick = btnSoundTestClick
      end
      object btnSoundSave: TButton
        Left = 284
        Top = 160
        Width = 61
        Height = 21
        Caption = '&Save'
        TabOrder = 6
        OnClick = btnSoundSaveClick
      end
    end
  end
  object dlgClo: TColorDialog
    Left = 389
    Top = 153
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 424
    Top = 153
  end
  object dlgOpen: TOpenDialog
    Left = 424
    Top = 186
  end
end
