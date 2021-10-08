object frmCharMap: TfrmCharMap
  Left = 394
  Top = 206
  Width = 319
  Height = 353
  Caption = 'Character map'
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 290
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
  object sttBar: TStatusBar
    Left = 0
    Top = 300
    Width = 311
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 253
    Width = 311
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = pnlBottomResize
    object btnSelect: TButton
      Left = 52
      Top = 24
      Width = 63
      Height = 21
      Caption = '&Select'
      TabOrder = 0
      OnClick = btnSelectClick
    end
    object fldText: TEdit
      Left = 116
      Top = 24
      Width = 69
      Height = 21
      TabOrder = 1
      OnChange = fldTextChange
    end
    object btnClipCopy: TBitBtn
      Left = 185
      Top = 24
      Width = 24
      Height = 21
      Hint = 'Put characters on clipboard'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
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
    object pnlOneChar: TPanel
      Left = 4
      Top = 4
      Width = 41
      Height = 41
      BevelInner = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object btnInsert: TButton
      Left = 244
      Top = 2
      Width = 63
      Height = 21
      Caption = '&Insert'
      TabOrder = 5
      OnClick = btnInsertClick
    end
    object btnClose: TButton
      Left = 244
      Top = 24
      Width = 63
      Height = 21
      Caption = '&Close'
      TabOrder = 3
      OnClick = btnCloseClick
    end
  end
  object pnlGrid: TPanel
    Left = 12
    Top = 8
    Width = 269
    Height = 237
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = pnlGridResize
    object grdChars: TStringGrid
      Left = 56
      Top = 56
      Width = 165
      Height = 149
      BorderStyle = bsNone
      Color = 14086654
      ColCount = 16
      DefaultColWidth = 20
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 14
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 0
      OnDblClick = grdCharsDblClick
      OnSelectCell = grdCharsSelectCell
    end
  end
end
