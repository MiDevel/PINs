object frmExpiredDates: TfrmExpiredDates
  Left = 306
  Top = 206
  Width = 388
  Height = 230
  Caption = 'Expired passwords'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 148
    Width = 380
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = pnlButtonsResize
    object btnRescan: TButton
      Left = 84
      Top = 4
      Width = 77
      Height = 25
      Caption = '&Rescan'
      TabOrder = 1
      OnClick = btnRescanClick
    end
    object btnClose: TButton
      Left = 300
      Top = 4
      Width = 77
      Height = 25
      Caption = '&Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
    object btnEdit: TButton
      Left = 4
      Top = 4
      Width = 77
      Height = 25
      Caption = '&Edit'
      Default = True
      TabOrder = 0
      OnClick = btnEditClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 6
    Width = 4
    Height = 142
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 380
    Height = 6
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlRight: TPanel
    Left = 376
    Top = 6
    Width = 4
    Height = 142
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
  end
  object sttBar: TStatusBar
    Left = 0
    Top = 179
    Width = 380
    Height = 17
    Panels = <>
    SimplePanel = True
  end
  object lvExpItems: TListView
    Left = 4
    Top = 6
    Width = 372
    Height = 142
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = '???'
        Width = 0
      end
      item
        Alignment = taRightJustify
        Caption = '#'
        Width = 30
      end
      item
        Caption = 'Category'
        Width = 80
      end
      item
        Caption = 'System'
        Width = 80
      end
      item
        Caption = 'User'
        Width = 80
      end
      item
        Caption = 'Exp. date'
        Width = 80
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnDblClick = lvExpItemsDblClick
    OnSelectItem = lvExpItemsSelectItem
  end
end
