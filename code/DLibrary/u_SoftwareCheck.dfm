object frmSoftwareCheck: TfrmSoftwareCheck
  Left = 312
  Top = 145
  BorderStyle = bsSingle
  Caption = 'Check for a new version'
  ClientHeight = 219
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100001000400E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000077777777000000000000000000000777777777777770000000000000
    0007777788888888777770000000000000777788888888888877770000000000
    0777888878888888888877700000000077788888277888888888877700000007
    7788888722277888888888777000007F7888888222222778888887877700007F
    888888722222222778887288770007FF888888222222222227882288777007F8
    888888222222222222772278877007F888888822222222222222222887707FF8
    88888888222222222222222888777F8888888888882222222222222788777F88
    88888888888822222222222288777F8811111111111888222222222288777F88
    99999999999888882222222278777F8899999999918888772222222228777F88
    89999999911888222222222228777FF8899999999991188888888888887707F8
    899999999999911888888888877007F8889999999999999118888888877007FF
    8899999999999999911888887770007F889999999999999999918888F700007F
    F8899889999999999998888FF7000007FF89988899999999999888FF70000000
    7FF988888899999999888FF70000000007FF8888888899999988FF7000000000
    007FFF888888889998FFF7000000000000077FFF888888889FF7700000000000
    00000777FFFFFFFF77700000000000000000000077777777000000000000FFF0
    0FFFFF8001FFFE00007FFC00003FF800001FF000000FE0000007C0000003C000
    0003800000018000000180000001000000000000000000000000000000000000
    0000000000000000000000000000800000018000000180000001C0000003C000
    0003E0000007F000000FF800001FFC00003FFE00007FFF8001FFFFF00FFF}
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 48
    Top = 12
    Width = 305
    Height = 29
    Alignment = taCenter
    AutoSize = False
    Caption = 'New software versions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object imgLogo: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      055449636F6E0000010001002020100001000400E80200001600000028000000
      2000000040000000010004000000000080020000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000077777777000000000000000000000777777777777770
      0000000000000007777788888888777770000000000000777788888888888877
      7700000000000777888878888888888877700000000077788888277888888888
      8777000000077788888722277888888888777000007F78888882222227788888
      87877700007F888888722222222778887288770007FF88888822222222222788
      2288777007F8888888222222222222772278877007F888888822222222222222
      222887707FF888888888222222222222222888777F8888888888882222222222
      222788777F8888888888888822222222222288777F8811111111111888222222
      222288777F8899999999999888882222222278777F8899999999918888772222
      222228777F8889999999911888222222222228777FF889999999999118888888
      8888887707F8899999999999911888888888877007F888999999999999911888
      8888877007FF8899999999999999911888887770007F88999999999999999991
      8888F700007FF8899889999999999998888FF7000007FF899888999999999998
      88FF700000007FF988888899999999888FF70000000007FF8888888899999988
      FF7000000000007FFF888888889998FFF7000000000000077FFF888888889FF7
      70000000000000000777FFFFFFFF777000000000000000000000777777770000
      00000000FFF00FFFFF8001FFFE00007FFC00003FF800001FF000000FE0000007
      C0000003C0000003800000018000000180000001000000000000000000000000
      0000000000000000000000000000000000000000800000018000000180000001
      C0000003C0000003E0000007F000000FF800001FFC00003FFE00007FFF8001FF
      FFF00FFF}
  end
  object btnCheck: TButton
    Left = 364
    Top = 12
    Width = 93
    Height = 25
    Caption = 'Check &now'
    TabOrder = 0
    OnClick = btnCheckClick
  end
  object btnCancel: TButton
    Left = 364
    Top = 40
    Width = 93
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 52
    Width = 345
    Height = 145
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object btnGet: TButton
    Left = 364
    Top = 104
    Width = 93
    Height = 25
    Caption = '&Download'
    TabOrder = 3
    OnClick = btnGetClick
  end
  object btnCopy: TButton
    Left = 364
    Top = 132
    Width = 93
    Height = 25
    Caption = 'Copy &address'
    TabOrder = 4
    OnClick = btnCopyClick
  end
  object btnNews: TButton
    Left = 364
    Top = 76
    Width = 93
    Height = 25
    Caption = '&What'#39's new'
    TabOrder = 2
    OnClick = btnNewsClick
  end
  object sttBar: TStatusBar
    Left = 0
    Top = 200
    Width = 464
    Height = 19
    Panels = <
      item
        Width = 320
      end>
  end
  object btnProxy: TButton
    Left = 364
    Top = 172
    Width = 93
    Height = 25
    Caption = '&Proxy settings'
    TabOrder = 7
    OnClick = btnProxyClick
  end
end