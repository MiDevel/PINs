{*****************************************************************************
 *  PINs
 *  u_Export.pas - data export configuration dialog
 *
 *  Copyright (C) 1998-2004 Mirek Wójtowicz
 *
 *  Author:     Mirek Wójtowicz
 *  E-mail:     info@mirekw.com, mirwoj@life.pl
 *  Homepage:   http://www.mirekw.com/
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation;
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *****************************************************************************}

unit u_Export;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ComCtrls;

type
  TfrmExport = class(TForm)
    grpFields: TGroupBox;
    outchkExpCat: TCheckBox;
    outchkExpUsr: TCheckBox;
    outchkExpSys: TCheckBox;
    outchkExpPsw: TCheckBox;
    outchkExpSta: TCheckBox;
    outchkExpExp: TCheckBox;
    outchkExpCmm: TCheckBox;
    outchkExpInf: TCheckBox;
    outchkExpCst: TCheckBox;
    lblExpSep: TLabel;
    cmbExpSep: TComboBox;
    grpCategories: TGroupBox;
    lisCategories: TCheckListBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    btnHelp: TButton;
    sttBar: TStatusBar;
    chkExpQuoteTexts: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private { Private declarations }
    m_FileName: String;

  published { Published declarations }
    property FileName: String read m_FileName write m_FileName;

  public { Public declarations }
    procedure ExportTextFile(const sName: String);

  end;

var
  frmExport: TfrmExport;

implementation

uses
  uMWStrings, uMWTools, u_Main, IniLang, u_Columns, u_Item, u_Sounds;

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TfrmExport.FormActivate(Sender: TObject);
var
  lisCat: TStringList;
  i: Integer;
begin
  sttBar.SimpleText := FileName;

  outchkExpCat.Checked     := GlbColumns[COL_CAT].ToExport;
  outchkExpSys.Checked     := GlbColumns[COL_SYS].ToExport;
  outchkExpUsr.Checked     := GlbColumns[COL_USR].ToExport;
  outchkExpPsw.Checked     := GlbColumns[COL_PSW].ToExport;
  outchkExpCmm.Checked     := GlbColumns[COL_CMM].ToExport;
  outchkExpCst.Checked     := GlbColumns[COL_CST].ToExport;
  outchkExpSta.Checked     := GlbColumns[COL_STA].ToExport;
  outchkExpExp.Checked     := GlbColumns[COL_EXP].ToExport;
  outchkExpInf.Checked     := GlbColumns[COL_INF].ToExport;
  cmbExpSep.Text           := frmMain.ExportSep;
  chkExpQuoteTexts.Checked := frmMain.ExpQuoteTexts;

  // fill the combo with existing categories
  lisCat := TStringList.Create;
  lisCat.Sorted := True;
  lisCat.Duplicates := dupIgnore;
  for i := 0 to frmMain.grdTree.Items.Count - 1 do
    if frmMain.grdTree.Items[i].Text <> '' then
      lisCat.Add(frmMain.grdTree.Items[i].Text);
  lisCategories.Clear;
  lisCategories.Items.AddStrings(lisCat);
  lisCat.Free;

  lisCategories.Items.BeginUpdate;
  for i := 0 to lisCategories.Items.Count - 1 do
    lisCategories.Checked[i] := True;
  lisCategories.Items.EndUpdate;
end;
//------------------------------------------------------------------------------
procedure TfrmExport.OKBtnClick(Sender: TObject);
begin
  GlbColumns[COL_CAT].ToExport := outchkExpCat.Checked;
  GlbColumns[COL_SYS].ToExport := outchkExpSys.Checked;
  GlbColumns[COL_USR].ToExport := outchkExpUsr.Checked;
  GlbColumns[COL_PSW].ToExport := outchkExpPsw.Checked;
  GlbColumns[COL_CMM].ToExport := outchkExpCmm.Checked;
  GlbColumns[COL_CST].ToExport := outchkExpCst.Checked;
  GlbColumns[COL_STA].ToExport := outchkExpSta.Checked;
  GlbColumns[COL_EXP].ToExport := outchkExpExp.Checked;
  GlbColumns[COL_INF].ToExport := outchkExpInf.Checked;
  frmMain.ExportSep            := cmbExpSep.Text;
  frmMain.ExpQuoteTexts        := chkExpQuoteTexts.Checked;

  ExportTextFile(FileName);
end;
//------------------------------------------------------------------------------
procedure TfrmExport.FormShow(Sender: TObject);
begin
  outchkExpCat.Caption := GlbColumns[COL_CAT].Caption; // Category
  outchkExpSys.Caption := GlbColumns[COL_SYS].Caption; // System
  outchkExpUsr.Caption := GlbColumns[COL_USR].Caption; // User
  outchkExpPsw.Caption := GlbColumns[COL_PSW].Caption; // Password
  outchkExpCmm.Caption := GlbColumns[COL_CMM].Caption; // Comments
  outchkExpCst.Caption := GlbColumns[COL_CST].Caption; // Custom
  outchkExpSta.Caption := GlbColumns[COL_STA].Caption; // Start date
  outchkExpExp.Caption := GlbColumns[COL_EXP].Caption; // Expires
  outchkExpInf.Caption := GlbColumns[COL_INF].Caption; // More info

  cmbExpSep.Items.Clear;
  cmbExpSep.Items.Add(';');
  cmbExpSep.Items.Add(',');
  cmbExpSep.Items.Add('tab');
end;
//------------------------------------------------------------------------------
// Save data to the text file
procedure TfrmExport.ExportTextFile(const sName: String);
var
  vFile: System.TextFile;
  i, iRet, fndIdx: Integer;
  itm: TOneItem;
  sTxt, sSep: String;
  savCursor: TCursor;
  function Qte(sString: String): String;
  begin
    if frmMain.ExpQuoteTexts then
      Result := AnsiQuotedStr(sString, '"')
    else
      Result := sString;
  end;
begin
  iRet := frmMain.ConfirmPassword(frmMain.Password, ilMiscStr(rsNotSecureEnterPass, 'rsNotSecureEnterPass'));
  case iRet of
    mrNo:
      begin
        PlayOneSound(sndERROR);
        frmMain.CstMessageDlg(ilMiscStr(rsBadPasswordClose, 'rsBadPasswordClose'), mtError, [mbOK], 0);
        Close;
      end;
    mrCancel:
      ;
    mrYes:
      begin
        savCursor := Screen.Cursor;
        Screen.Cursor := crHourglass; // show hourglass cursor
        itm := TOneItem.Create;

        if frmMain.MakeBAK then // make .BAK copies
        begin
          DeleteFile(sName + '.BAK'); // remove last .BAK
          RenameFile(sName, sName + '.BAK'); // create new
        end;

        System.AssignFile(vFile, sName);
        Rewrite(vFile);

        sSep := frmMain.SepToStr(frmMain.ExportSep);
        Writeln(vFile,  iifs(GlbColumns[COL_CAT].ToExport, Qte(GlbColumns[COL_CAT].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_SYS].ToExport, Qte(GlbColumns[COL_SYS].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_USR].ToExport, Qte(GlbColumns[COL_USR].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_PSW].ToExport, Qte(GlbColumns[COL_PSW].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_CMM].ToExport, Qte(GlbColumns[COL_CMM].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_CST].ToExport, Qte(GlbColumns[COL_CST].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_STA].ToExport, Qte(GlbColumns[COL_STA].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_EXP].ToExport, Qte(GlbColumns[COL_EXP].Caption) + sSep, '') +
                        iifs(GlbColumns[COL_INF].ToExport, Qte(GlbColumns[COL_INF].Caption), ''));

        for i := 0 to frmMain.grdTree.Items.Count - 1 do
        begin
          if frmMain.GridRowAsItem(i, itm) then
          begin
            fndIdx := lisCategories.Items.IndexOf(itm.Category);
            if (fndIdx >= 0) and lisCategories.Checked[fndIdx] then
            begin
              sTxt := iifs(GlbColumns[COL_CAT].ToExport, Qte(itm.Category ) + sSep, '') +
                      iifs(GlbColumns[COL_SYS].ToExport, Qte(itm.System   ) + sSep, '') +
                      iifs(GlbColumns[COL_USR].ToExport, Qte(itm.User     ) + sSep, '') +
                      iifs(GlbColumns[COL_PSW].ToExport, Qte(itm.Password ) + sSep, '') +
                      iifs(GlbColumns[COL_CMM].ToExport, Qte(itm.Comments ) + sSep, '') +
                      iifs(GlbColumns[COL_CST].ToExport, Qte(itm.Custom   ) + sSep, '') +
                      iifs(GlbColumns[COL_STA].ToExport, Qte(itm.StartDate) + sSep, '') +
                      iifs(GlbColumns[COL_EXP].ToExport, Qte(itm.Expires  ) + sSep, '') +
                      iifs(GlbColumns[COL_INF].ToExport, Qte(ReplStr(S_CRLF, '||', itm.Info)), '');
            end;
            Writeln(vFile, sTxt);
          end;
        end;
        CloseFile(vFile);
        Screen.Cursor := savCursor;  // restore default cursor
        PlayOneSound(sndSAVE);
        frmMain.UpdateUI; // update all user interface elements
      end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmExport.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_export.htm', Handle);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TfrmExport.FormCreate(Sender: TObject);
begin
//  fillCustomIni; //call it at design-time, only when you're ready to translate
end;

end.
