{*****************************************************************************
 *  PINs
 *  u_Import.pas - Import from text files
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

unit u_Import;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons;

resourcestring
  rsIgnore='*Ignore*';
  rsNoDataImp='No data imported!';

type
  TfrmImport = class(TForm)
    grdTest: TStringGrid;
    lblDstPreview: TLabel;
    btnTest: TButton;
    btnImport: TButton;
    btnCancel: TButton;
    grpFldOrder: TGroupBox;
    outcmbFld1: TComboBox;
    outLabel2: TLabel;
    outcmbFld4: TComboBox;
    outLabel3: TLabel;
    outcmbFld7: TComboBox;
    outLabel4: TLabel;
    outcmbFld2: TComboBox;
    outLabel5: TLabel;
    outcmbFld5: TComboBox;
    outLabel6: TLabel;
    outcmbFld8: TComboBox;
    outLabel7: TLabel;
    outcmbFld3: TComboBox;
    outLabel8: TLabel;
    outcmbFld6: TComboBox;
    outLabel9: TLabel;
    outcmbFld9: TComboBox;
    outLabel10: TLabel;
    lblSeparator: TLabel;
    lblFile: TLabel;
    fldFileName: TEdit;
    btnOpen: TSpeedButton;
    OpenDialog: TOpenDialog;
    Bevel1: TBevel;
    lblSrcPreview: TLabel;
    fldSource: TMemo;
    chkIgnoreFirstRow: TCheckBox;
    cmbExpSep: TComboBox;
    lblIgnoreFirstRow: TLabel;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);

  private { Private declarations }
    m_sFileName: String;
    m_fAppend: Boolean;
    procedure ClearPreview;

  public { Public declarations }
    fAccepted: Boolean;
    procedure Init(sFileName: String; fAppend: Boolean);
  end;

var
  frmImport: TfrmImport;

implementation

uses
  uMWStrings, uMWTools, u_Item, u_Main, IniLang, u_Columns;

var
  CmbArray: array[0..8] of TComboBox;

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TfrmImport.Init(sFileName: String; fAppend: Boolean);
begin
  ClearPreview;
  m_sFileName := sFileName;
  m_fAppend   := fAppend;
  fAccepted   := False;
  fldFileName.Text := sFileName;
  fldSource.Lines.LoadFromFile(fldFileName.Text);
end;
//------------------------------------------------------------------------------
procedure FillCombo(cmb: TComboBox);
begin
  cmb.Clear;
  cmb.Items.Add(GlbColumns[COL_CAT].Caption); // Category
  cmb.Items.Add(GlbColumns[COL_SYS].Caption); // System
  cmb.Items.Add(GlbColumns[COL_USR].Caption); // User
  cmb.Items.Add(GlbColumns[COL_PSW].Caption); // Password
  cmb.Items.Add(GlbColumns[COL_CMM].Caption); // URL/Comments
  cmb.Items.Add(GlbColumns[COL_CST].Caption); // Custom
  cmb.Items.Add(GlbColumns[COL_STA].Caption); // Start date
  cmb.Items.Add(GlbColumns[COL_EXP].Caption); // Expires
  cmb.Items.Add(GlbColumns[COL_INF].Caption); // More info
  cmb.Items.Add(ilMiscStr(rsIgnore, 'rsIgnore')); // *Ignore*
end;
//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmImport.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  frmMain.ReadWindowPosition(self, False);
  for i := 0 to 8 do
    CmbArray[i] := TComboBox.Create(nil);

  CmbArray[0] := outcmbFld1;
  CmbArray[1] := outcmbFld2;
  CmbArray[2] := outcmbFld3;
  CmbArray[3] := outcmbFld4;
  CmbArray[4] := outcmbFld5;
  CmbArray[5] := outcmbFld6;
  CmbArray[6] := outcmbFld7;
  CmbArray[7] := outcmbFld8;
  CmbArray[8] := outcmbFld9;
end;
procedure TfrmImport.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmImport.FormShow(Sender: TObject);
begin
  cmbExpSep.Items.Clear;
  cmbExpSep.Items.Add(';');
  cmbExpSep.Items.Add(',');
  cmbExpSep.Items.Add('tab');
end;
//------------------------------------------------------------------------------
procedure TfrmImport.ClearPreview;
var
  row, col: Integer;
begin
  for row := 1 to 5 do
    for col := 0 to 8 do
      grdTest.Cells[col, row] := '';
end;
//------------------------------------------------------------------------------
procedure TfrmImport.btnTestClick(Sender: TObject);
var
  strList: TStringList;
  i, iRow, iCol: Integer;
  sBff, sTok, sSep: String;
begin
  ClearPreview;
  strList := TStringList.Create;
  sSep := frmMain.SepToStr(cmbExpSep.Text);
  if ReadTextFileToStrings(m_sFileName, strList) then
  begin
    for iRow := 0 to 4 do
    begin
      if (iifi(chkIgnoreFirstRow.Checked, iRow+1, iRow) < strList.Count) then
      begin
        sBff := strList.Strings[iifi(chkIgnoreFirstRow.Checked, iRow+1, iRow)];
        sBff := ReplStr(sSep + sSep, sSep + ' ' + sSep, sBff);
        i := 1;
        for iCol := 0 to 8 do
        begin
          sTok := Trim(GetNextToken(sBff, i, sSep));
          if (CmbArray[iCol].ItemIndex <= 8) then
          begin
            sTok := DequotedStr(sTok);
            grdTest.Cells[CmbArray[iCol].ItemIndex, iRow+1] := sTok;
          end;
        end;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmImport.btnImportClick(Sender: TObject);
var
  strList: TStringList;
  i, row, col: Integer;
  sBff, sTok, sSep: String;
  itm: TOneItem;
  savCursor: TCursor;
  fFirstRow: Boolean;
begin
  savCursor := Screen.Cursor;
  Screen.Cursor := crHourglass; // show hourglass cursor
  strList := TStringList.Create;
  itm := TOneItem.Create;
  if ReadTextFileToStrings(m_sFileName, strList) then
  begin
    if not m_fAppend then
    begin
      frmMain.grdTree.Items.Clear;
      frmMain.DataFile := m_sFileName;
    end;

    fFirstRow := True;
    sSep := frmMain.SepToStr(cmbExpSep.Text);
    for row := iifi(chkIgnoreFirstRow.Checked, 1, 0) to strList.Count - 1 do
    begin
      sBff := strList.Strings[row];
      sBff := ReplStr(sSep + sSep, sSep + ' ' + sSep, sBff);
      itm.Clear;
      i := 1;
      for col := 0 to 8 do
      begin
        sTok := Trim(GetNextToken(sBff, i, sSep));
        sTok := DequotedStr(sTok);
        case CmbArray[col].ItemIndex of
          0: itm.Category  := sTok;
          1: itm.System    := sTok;
          2: itm.User      := sTok;
          3: itm.Password  := sTok;
          4: itm.Comments  := sTok;
          5: itm.Custom    := sTok;
          6: itm.StartDate := sTok;
          7: itm.Expires   := sTok;
          8: itm.Info      := ReplStr('||', S_CRLF, sTok);
        end;
      end;
      frmMain.AddGridItem(itm, False, not fFirstRow);
      fFirstRow := False;
    end;
  end
  else
  begin
    frmMain.CstMessageDlg(ilMiscStr(rsNoDataImp, 'rsNoDataImp'), mtError, [mbOk], 0);
  end;
  Screen.Cursor := savCursor;  // restore default cursor
  fAccepted := True;
end;
//------------------------------------------------------------------------------
procedure TfrmImport.btnOpenClick(Sender: TObject);
begin
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
    Init(OpenDialog.FileName, m_fAppend);
end;
//------------------------------------------------------------------------------
// Initialize the dialog
procedure TfrmImport.FormActivate(Sender: TObject);
begin
  FillCombo(outcmbFld1);
  FillCombo(outcmbFld2);
  FillCombo(outcmbFld3);
  FillCombo(outcmbFld4);
  FillCombo(outcmbFld5);
  FillCombo(outcmbFld6);
  FillCombo(outcmbFld7);
  FillCombo(outcmbFld8);
  FillCombo(outcmbFld9);

  grdTest.Cells[0, 0] := GlbColumns[COL_CAT].Caption; // Category
  grdTest.Cells[1, 0] := GlbColumns[COL_SYS].Caption; // System
  grdTest.Cells[2, 0] := GlbColumns[COL_USR].Caption; // User
  grdTest.Cells[3, 0] := GlbColumns[COL_PSW].Caption; // Password
  grdTest.Cells[4, 0] := GlbColumns[COL_CMM].Caption; // URL/Comments
  grdTest.Cells[5, 0] := GlbColumns[COL_CST].Caption; // Custom
  grdTest.Cells[6, 0] := GlbColumns[COL_STA].Caption; // Start date
  grdTest.Cells[7, 0] := GlbColumns[COL_EXP].Caption; // Expires
  grdTest.Cells[8, 0] := GlbColumns[COL_INF].Caption; // More info

  cmbExpSep.Text       := frmMain.ReadString ('Import', 'Separator', ';');
  outcmbFld1.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld1', 0);
  outcmbFld2.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld2', 1);
  outcmbFld3.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld3', 2);
  outcmbFld4.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld4', 3);
  outcmbFld5.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld5', 4);
  outcmbFld6.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld6', 5);
  outcmbFld7.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld7', 6);
  outcmbFld8.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld8', 7);
  outcmbFld9.ItemIndex := frmMain.ReadInteger('Import', 'cmbFld9', 8);
  chkIgnoreFirstRow.Checked := frmMain.ReadBool('Import', 'IgnoreFirst', False);
end;
//------------------------------------------------------------------------------
// The form is about to close - save settings
procedure TfrmImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmMain.WriteString ('Import', 'Separator', cmbExpSep.Text);
  frmMain.WriteInteger('Import', 'cmbFld1', outcmbFld1.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld2', outcmbFld2.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld3', outcmbFld3.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld4', outcmbFld4.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld5', outcmbFld5.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld6', outcmbFld6.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld7', outcmbFld7.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld8', outcmbFld8.ItemIndex);
  frmMain.WriteInteger('Import', 'cmbFld9', outcmbFld9.ItemIndex);
  frmMain.WriteBool   ('Import', 'IgnoreFirst', chkIgnoreFirstRow.Checked);
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmImport.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
procedure TfrmImport.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_import.htm', Handle);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
