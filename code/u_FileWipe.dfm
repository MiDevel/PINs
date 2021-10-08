object frmFileWipe: TfrmFileWipe
  Left = 264
  Top = 228
  BorderStyle = bsSingle
  Caption = 'Secure file wiping'
  ClientHeight = 361
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblWhatsThis: TLabel
    Left = 12
    Top = 4
    Width = 407
    Height = 13
    Caption = 
      'Use this dialog to permanently DELETE one or more files from you' +
      'r harddisk or a floppy'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object grpMethod: TGroupBox
    Left = 8
    Top = 200
    Width = 433
    Height = 137
    Caption = 'Wiping method'
    TabOrder = 0
    object rdi1pass: TRadioButton
      Left = 16
      Top = 24
      Width = 401
      Height = 17
      Caption = '&One pseudorandom pass (Quick, insecure)'
      TabOrder = 0
    end
    object rdiDoD3: TRadioButton
      Left = 16
      Top = 44
      Width = 401
      Height = 17
      Caption = 'DoD &3 pass (5220.22-M, sec. 8.306 d,e)'
      TabOrder = 1
    end
    object rdiDoD7: TRadioButton
      Left = 16
      Top = 64
      Width = 401
      Height = 17
      Caption = 'DoD &7 pass'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object rdiGutmann35: TRadioButton
      Left = 16
      Top = 84
      Width = 401
      Height = 21
      Caption = '&Gutmann 35 pass (Slow, the most secure)'
      TabOrder = 3
    end
    object rdiCustom: TRadioButton
      Left = 16
      Top = 108
      Width = 81
      Height = 17
      Caption = 'Custo&m:'
      TabOrder = 4
    end
    object fldCustomPattern: TEdit
      Left = 108
      Top = 104
      Width = 313
      Height = 21
      TabOrder = 5
      Text = '0,255,r,0,255,r,r,r'
      OnChange = fldCustomPatternChange
    end
  end
  object grpSource: TGroupBox
    Left = 8
    Top = 20
    Width = 433
    Height = 173
    Caption = 'What to wipe'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 1
    object Bevel1: TBevel
      Left = 12
      Top = 20
      Width = 413
      Height = 57
    end
    object Bevel2: TBevel
      Left = 12
      Top = 84
      Width = 413
      Height = 81
    end
    object lblMask: TLabel
      Left = 316
      Top = 140
      Width = 29
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = 'Mask:'
      ParentBiDiMode = False
    end
    object fldFileToWipe: TEdit
      Left = 32
      Top = 48
      Width = 361
      Height = 21
      TabOrder = 0
      Text = 'FileToWipe.txt'
    end
    object rdiWhat_File: TRadioButton
      Left = 20
      Top = 28
      Width = 161
      Height = 17
      Caption = '&Single files'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rdiWhatClick
    end
    object btnSelectFile: TBitBtn
      Left = 396
      Top = 48
      Width = 21
      Height = 21
      Hint = 'Select the file(s) you want to wipe'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnSelectFileClick
    end
    object rdiWhat_Folder: TRadioButton
      Left = 20
      Top = 92
      Width = 149
      Height = 17
      Caption = '&Folders'
      TabOrder = 3
      OnClick = rdiWhatClick
    end
    object fldFolderToWipe: TEdit
      Left = 32
      Top = 112
      Width = 361
      Height = 21
      TabOrder = 4
      Text = 'FileToWipe.txt'
    end
    object btnSelectFolder: TBitBtn
      Left = 396
      Top = 112
      Width = 21
      Height = 21
      Hint = 'Select the folder you want to wipe'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnSelectFolderClick
    end
    object chkSubfolders: TCheckBox
      Left = 32
      Top = 136
      Width = 221
      Height = 17
      Caption = 'Include subfolders'
      TabOrder = 6
    end
    object fldMask: TEdit
      Left = 348
      Top = 136
      Width = 45
      Height = 21
      TabOrder = 7
      Text = '*.*'
    end
  end
  object btnWipe: TButton
    Left = 448
    Top = 24
    Width = 81
    Height = 25
    Caption = '&Wipe'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = btnWipeClick
  end
  object btnClose: TButton
    Left = 448
    Top = 52
    Width = 81
    Height = 25
    Caption = '&Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object ProgressBar1: TProgressBar
    Left = 448
    Top = 156
    Width = 81
    Height = 13
    TabOrder = 6
    Visible = False
  end
  object ProgressBar2: TProgressBar
    Left = 448
    Top = 176
    Width = 81
    Height = 13
    TabOrder = 7
    Visible = False
  end
  object sttBar: TStatusBar
    Left = 0
    Top = 342
    Width = 535
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object btnBreak: TButton
    Left = 448
    Top = 116
    Width = 81
    Height = 25
    Caption = '&Break'
    TabOrder = 5
    Visible = False
    OnClick = btnBreakClick
  end
  object btnLogsView: TButton
    Left = 448
    Top = 264
    Width = 81
    Height = 25
    Caption = '&View logs'
    TabOrder = 9
    OnClick = btnLogsViewClick
  end
  object chkLogging: TCheckBox
    Left = 448
    Top = 320
    Width = 81
    Height = 17
    Caption = '&Logging'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object btnLogsClear: TButton
    Left = 448
    Top = 292
    Width = 81
    Height = 25
    Caption = 'Clear logs'
    TabOrder = 11
    OnClick = btnLogsClearClick
  end
  object btnHelp: TButton
    Left = 448
    Top = 80
    Width = 81
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object dlgOpenFile: TOpenDialog
    Filter = '*.*'
    Left = 452
    Top = 220
  end
end
