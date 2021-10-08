{*****************************************************************************
 *  PINs
 *  u_Options.pas - Program settings
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

unit u_Options;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Dialogs, ElTree, mwRichEdit,
  ElXPThemedControl;

const
  rsOptLayDispHoriz='Display horizontal grid lines';
  rsOptLayDispVert='Display vertical grid lines';
  rsOptLayGridBackClo='Grid background color';
  rsOptLayGridFont='Grid font';
  rsOptLayInfoClo='Info box background color';
  rsOptLayInfoFont='Info box font';
  rsOptLayInfoURLClo='Info box hyperlinks (URLs) color';
  rsSoundFiles='Sound files';
  rsSoundError='Errors and warnings';
  rsSoundOpen='Data file opened';
  rsSoundSave='Data file saved';
  rsSoundClipCopyUser='User name copied to clipboard';
  rsSoundClipCopyPasswd='Password copied to clipboard';
  rsErrorLoadDialog='Dialog failed to load';
  rsColVisibility='Column visibility';
  rsColCustName='Custom name';
  rsEnterColName='Enter your name for the column [%s]';
  rsSoundPaste='Paste data into other applications';

type
  TfrmOptions = class(TForm)
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    dlgClo: TColorDialog;
    btnApply: TButton;
    tabOptions: TPageControl;
    tabGeneral: TTabSheet;
    chkAddSamples: TCheckBox;
    chkUseTray: TCheckBox;
    chkActiveHyperlinks: TCheckBox;
    tabSecurity: TTabSheet;
    tabStartUp: TTabSheet;
    Bevel2: TBevel;
    tabSaving: TTabSheet;
    Bevel3: TBevel;
    chkShowPswCol: TCheckBox;
    chkShowPswFld: TCheckBox;
    chkClipClrExit: TCheckBox;
    chkClipClrMini: TCheckBox;
    chkLockOnMinimize: TCheckBox;
    dlgFont: TFontDialog;
    tabLayout: TTabSheet;
    grpGrid: TGroupBox;
    lblPreview: TLabel;
    grdSample: TElTree;
    lisLayItems: TListBox;
    btnLayChange: TButton;
    fldInfoBoxSample: TmwRichEdit;
    btnLayDefault: TButton;
    tabColumns: TTabSheet;
    grpViewNameColumns: TGroupBox;
    tabSounds: TTabSheet;
    btnSoundWavePath: TSpeedButton;
    lisSounds: TListBox;
    chkSoundOn: TCheckBox;
    chkOneSoundActive: TCheckBox;
    rdiSoundBuiltIn: TRadioButton;
    rdiSoundWaveFile: TRadioButton;
    fldSoundWavePath: TEdit;
    btnSoundTest: TButton;
    btnSoundSave: TButton;
    Bevel4: TBevel;
    dlgOpen: TOpenDialog;
    btnHelp: TButton;
    lvColViewNames: TListView;
    btnColRename: TButton;
    chkUserColNames: TCheckBox;
    lblDblClkAction: TLabel;
    cmbDblClkAction: TComboBox;
    chkSaveOnMinim: TCheckBox;
    chkAutoExpand: TCheckBox;
    chkAutoCopyPsw: TCheckBox;
    lblMaxInactivity: TLabel;
    fldMaxInactivity: TEdit;
    spnMaxInactivity: TUpDown;
    lblMinutes: TLabel;
    Bevel5: TBevel;
    chkReloadData: TCheckBox;
    chkCheckExpDate: TCheckBox;
    lblExpPrompt: TLabel;
    fldExpDateDays: TEdit;
    spnExpDateDays: TUpDown;
    lblExpDays: TLabel;
    Bevel1: TBevel;
    lblNumMRUFil: TLabel;
    chkMakeBAK: TCheckBox;
    chkSaveArchCopy: TCheckBox;
    fldSaveArchCopyTo: TEdit;
    btnSelectFolder: TBitBtn;
    chkAutoSave: TCheckBox;
    spnNumMRUFil: TUpDown;
    fldNumMRUFil: TEdit;
    chkStartExpanded: TCheckBox;
    chkPasteInLocked: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLayChangeClick(Sender: TObject);
    procedure btnLayDefaultClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectFolderClick(Sender: TObject);
    procedure lisSoundsClick(Sender: TObject);
    procedure lisSoundsDblClick(Sender: TObject);
    procedure btnSoundTestClick(Sender: TObject);
    procedure btnSoundSaveClick(Sender: TObject);
    procedure btnSoundWavePathClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnColRenameClick(Sender: TObject);
  private { Private declarations }
  public { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  u_Main, uMWStrings, IniLang, u_Sounds, u_Columns;

{$R *.DFM}

//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmOptions.FormCreate(Sender: TObject);
var
  itm: TElTreeItem;
  i: Integer;
begin
  frmMain.ReadWindowPosition(self, False);

  // create preview
  for i := 1 to 4 do
  begin
    itm := grdSample.Items.AddItem(Nil);
    if (itm <> nil) then
    begin
      itm.ParentStyle := False;
      itm.Bold := True;
      itm.Text := '?';
      itm.ColumnText.Add(IntToStr(i));
      itm.ColumnText.Add('Abc');
    end;
  end;
end;
procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.FormActivate(Sender: TObject);
var
  i: Integer;
  iSnd: TSounds;
  lvItem: TListItem;
  NewColumn: TListColumn;
begin
  // General
  chkAddSamples.Checked  := frmMain.AddSamples;
  chkUseTray.Checked     := frmMain.UseTray;
  chkActiveHyperlinks.Checked := frmMain.ActiveHyperlinks;
  chkSaveOnMinim.Checked := frmMain.SaveOnMinim;

  cmbDblClkAction.Clear;
  cmbDblClkAction.Items.Add(ReplStr('&', '', frmMain.optRecordEdit.Caption));
  cmbDblClkAction.Items.Add(ReplStr('&', '', frmMain.optClipCopyPsw.Caption));
  cmbDblClkAction.Items.Add(ReplStr('&', '', frmMain.optClipCopyUsr.Caption));
  cmbDblClkAction.Items.Add(ReplStr('&', '', frmMain.optHyperlink.Caption));
  cmbDblClkAction.ItemIndex := frmMain.DblClkAction - 1;
  chkAutoExpand.Checked  := frmMain.AutoExpand;

  // Security
  chkShowPswCol.Checked     := frmMain.ShowPswCol;
  chkShowPswFld.Checked     := frmMain.ShowPswFld;
  chkClipClrExit.Checked    := frmMain.ClipClrExit; // clear the clipboard on exit
  chkClipClrMini.Checked    := frmMain.ClipClrMini; // clear the clipboard on minimize
  chkLockOnMinimize.Checked := frmMain.LockOnMinimize; // lock database on minimize
  chkAutoCopyPsw.Checked    := frmMain.AutoCopyPsw;
  spnMaxInactivity.Position := frmMain.MaxInactivity div 60; // in min
  chkPasteInLocked.Checked  := frmMain.PasteInLocked;

  // StartUp
  chkReloadData.Checked     := frmMain.ReloadLast;
  chkStartExpanded.Checked  := frmMain.StartExpanded;
  chkCheckExpDate.Checked   := frmMain.CheckExpDate;
  spnExpDateDays.Position   := frmMain.ExpDateDays;

  // Saving
  chkAutoSave.Checked     := frmMain.AutoSave; // auto save on exit
  chkMakeBAK.Checked      := frmMain.MakeBAK;
  chkSaveArchCopy.Checked := frmMain.SaveArchCopy;   // make additional archival copies?
  fldSaveArchCopyTo.Text  := frmMain.SaveArchCopyTo; // archival copies folder
  spnNumMRUFil.Position   := frmMain.NumMRUFiles; // most recently used

  // Layout
  grdSample.BkColor               := frmMain.grdTree.BkColor;
  grdSample.Font.Assign(frmMain.grdTree.Font);
  grdSample.TextColor             := frmMain.grdTree.TextColor;
  fldInfoBoxSample.Color          := frmMain.fldInfo.Color;
  fldInfoBoxSample.Font.Assign(frmMain.fldInfo.Font);
  fldInfoBoxSample.HyperlinkColor := frmMain.fldInfo.HyperlinkColor;
  grdSample.HorizontalLines       := frmMain.GridLinesHoriz;
  grdSample.VerticalLines         := frmMain.GridLinesVert;

  // Columns
  chkUserColNames.Checked := frmMain.UserColNames;
  lvColViewNames.Items.Clear;
  lvColViewNames.Columns.Clear;
  NewColumn := lvColViewNames.Columns.Add;
  NewColumn.Caption := ilMiscStr(rsColVisibility, 'rsColVisibility');
  NewColumn.Width := lvColViewNames.Width div 2;
  NewColumn := lvColViewNames.Columns.Add;
  NewColumn.Caption := ilMiscStr(rsColCustName, 'rsColCustName');
  NewColumn.Width := (lvColViewNames.Width div 2) - 20;

  for i := COL_SYS to COL_IDXLAST - 2 do // without the 'Info' and 'Sps' columns
  begin
    lvItem := lvColViewNames.Items.Add;
    lvItem.Checked := frmMain.grdTree.HeaderSections[i].Visible;
    lvItem.Caption := GlbColumns[i].DftName;
    lvItem.SubItems.Add(GlbColumns[i].UserName);
  end;
  lvColViewNames.Selected := lvColViewNames.Items[0];

  // Sounds
  chkSoundOn.Checked := frmMain.SoundOn;
  lisSounds.Clear;
  lisSounds.Items.Add(ilMiscStr(rsSoundError, 'rsSoundError'));
  lisSounds.Items.Add(ilMiscStr(rsSoundOpen, 'rsSoundOpen'));
  lisSounds.Items.Add(ilMiscStr(rsSoundSave, 'rsSoundSave'));
  lisSounds.Items.Add(ilMiscStr(rsSoundClipCopyUser, 'rsSoundClipCopyUser'));
  lisSounds.Items.Add(ilMiscStr(rsSoundClipCopyPasswd, 'rsSoundClipCopyPasswd'));
  lisSounds.Items.Add(ilMiscStr(rsSoundPaste, 'rsSoundPaste'));

  lisSounds.ItemIndex := 0;
  for iSnd := sndNONE to LastSound do
  begin
    TmpSoundsAry[iSnd].Sound       := SoundsAry[iSnd].Sound;
    TmpSoundsAry[iSnd].Active      := SoundsAry[iSnd].Active;
    TmpSoundsAry[iSnd].UseInternal := SoundsAry[iSnd].UseInternal;
    TmpSoundsAry[iSnd].Path        := SoundsAry[iSnd].Path;
  end;

  lisSoundsClick(nil);
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.btnApplyClick(Sender: TObject);
var
  i: Integer;
  iSnd: TSounds;
begin
  // General
  frmMain.AddSamples     := chkAddSamples.Checked;
  frmMain.UseTray        := chkUseTray.Checked;
  frmMain.ActiveHyperlinks := chkActiveHyperlinks.Checked;
  frmMain.SaveOnMinim    := chkSaveOnMinim.Checked;
  frmMain.DblClkAction   := cmbDblClkAction.ItemIndex + 1;
  frmMain.AutoExpand     := chkAutoExpand.Checked;

  // Security
  frmMain.ShowPswCol     := chkShowPswCol.Checked;
  frmMain.ShowPswFld     := chkShowPswFld.Checked;
  frmMain.ClipClrExit    := chkClipClrExit.Checked; // clear the clipboard on exit
  frmMain.ClipClrMini    := chkClipClrMini.Checked; // clear the clipboard on minimize
  frmMain.LockOnMinimize := chkLockOnMinimize.Checked; // lock database on minimize
  frmMain.AutoCopyPsw    := chkAutoCopyPsw.Checked;
  frmMain.MaxInactivity  := spnMaxInactivity.Position * 60; // in sec
  frmMain.PasteInLocked  := chkPasteInLocked.Checked;

  // StartUp
  frmMain.ReloadLast     := chkReloadData.Checked;
  frmMain.StartExpanded  := chkStartExpanded.Checked;
  frmMain.CheckExpDate   := chkCheckExpDate.Checked;
  frmMain.ExpDateDays    := spnExpDateDays.Position;

  // Saving
  frmMain.AutoSave       := chkAutoSave.Checked; // auto save on exit
  frmMain.MakeBAK        := chkMakeBAK.Checked;
  frmMain.SaveArchCopy   := chkSaveArchCopy.Checked; // make additional archival copies?
  frmMain.SaveArchCopyTo := fldSaveArchCopyTo.Text;  // archival copies folder
  frmMain.NumMRUFiles    := spnNumMRUFil.Position; // most recently used

  // Layout
  frmMain.grdTree.BkColor        := grdSample.BkColor;
  frmMain.grdTree.Font.Assign(grdSample.Font);
  frmMain.grdTree.TextColor      := grdSample.TextColor;
  frmMain.fldInfo.Color          := fldInfoBoxSample.Color;
  frmMain.fldInfo.Font.Assign(fldInfoBoxSample.Font);
  frmMain.fldInfo.HyperlinkColor := fldInfoBoxSample.HyperlinkColor;
  frmMain.GridLinesHoriz         := grdSample.HorizontalLines;
  frmMain.GridLinesVert          := grdSample.VerticalLines;

  // Columns
  frmMain.UserColNames := chkUserColNames.Checked;
  for i := 0 to lvColViewNames.Items.Count - 1 do
  begin
    frmMain.grdTree.HeaderSections[i+COL_SYS].Visible := lvColViewNames.Items[i].Checked;
    GlbColumns[i+COL_SYS].UserName := lvColViewNames.Items[i].SubItems[0];
    frmMain.grdTree.HeaderSections[i+COL_SYS].Text := GlbColumns[i+COL_SYS].Caption;
  end;

  // Sounds
  frmMain.SoundOn := chkSoundOn.Checked;
  for iSnd := sndNONE to LastSound do
  begin
    SoundsAry[iSnd].Active      := TmpSoundsAry[iSnd].Active;
    SoundsAry[iSnd].UseInternal := TmpSoundsAry[iSnd].UseInternal;
    SoundsAry[iSnd].Path        := TmpSoundsAry[iSnd].Path;
  end;

  frmMain.UpdateUI;
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.OKBtnClick(Sender: TObject);
begin
  btnApplyClick(nil);
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.FormShow(Sender: TObject);
begin
  lisLayItems.Items.Clear;
  lisLayItems.Items.Add(ilMiscStr(rsOptLayDispHoriz  , 'rsOptLayDispHoriz'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayDispVert   , 'rsOptLayDispVert'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayGridBackClo, 'rsOptLayGridBackClo'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayGridFont   , 'rsOptLayGridFont'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayInfoClo    , 'rsOptLayInfoClo'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayInfoFont   , 'rsOptLayInfoFont'));
  lisLayItems.Items.Add(ilMiscStr(rsOptLayInfoURLClo , 'rsOptLayInfoURLClo'));

  fldInfoBoxSample.Text := 'abcABC' + S_CRLF + 'ftp://ab.com';
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.btnLayChangeClick(Sender: TObject);
begin
  try
    case lisLayItems.ItemIndex of
      0: // Display horizontal grid lines
         grdSample.HorizontalLines := not grdSample.HorizontalLines;

      1: // Display vertical grid lines
         grdSample.VerticalLines := not grdSample.VerticalLines;

      2: // Grid background color
         begin
           dlgClo.Color := grdSample.BkColor;
             if dlgClo.Execute then
               grdSample.BkColor := dlgClo.Color;
         end;

      3: // Grid font
         begin
           dlgFont.Font.Assign(grdSample.Font);
           dlgFont.Font.Color := grdSample.TextColor;
           if dlgFont.Execute then
           begin
             grdSample.Font.Assign(dlgFont.Font);
             grdSample.Font.Color := clWindowText;
             grdSample.TextColor := dlgFont.Font.Color;
           end;
         end;

      4: // Info box background color
         begin
           dlgClo.Color := fldInfoBoxSample.Color;
           if dlgClo.Execute then
             fldInfoBoxSample.Color := dlgClo.Color;
         end;

      5: // Info box font
         begin
           dlgFont.Font.Assign(fldInfoBoxSample.Font);
           if dlgFont.Execute then
           begin
             fldInfoBoxSample.Font.Assign(dlgFont.Font);
             fldInfoBoxSample.SelStart := 0;
             fldInfoBoxSample.SelLength := Length(fldInfoBoxSample.Text);
             fldInfoBoxSample.SelAttributes.Assign(fldInfoBoxSample.Font);
             fldInfoBoxSample.RescanHyperlinks;
           end;
         end;

      6: // Info box hyperlinks (URLs) color
         begin
           dlgClo.Color := fldInfoBoxSample.HyperlinkColor;
           if dlgClo.Execute then
             fldInfoBoxSample.HyperlinkColor := dlgClo.Color;
         end;
    end;
  except
    frmMain.CstShowMessage(ilMiscStr(rsErrorLoadDialog, 'rsErrorLoadDialog'));
  end;
end;
//------------------------------------------------------------------------------
// Reset all layout settings to their defaults
procedure TfrmOptions.btnLayDefaultClick(Sender: TObject);
begin
  grdSample.BkColor := DFT_BCKCLO;
  grdSample.Font.Assign(btnLayDefault.Font);
  grdSample.TextColor := clWindowText;
  fldInfoBoxSample.Color := DFT_BCKCLO;
  fldInfoBoxSample.Font.Assign(btnLayDefault.Font);
  fldInfoBoxSample.HyperlinkColor := clBlue;
  grdSample.HorizontalLines := True;
  grdSample.VerticalLines := False;

  fldInfoBoxSample.SelStart := 0;
  fldInfoBoxSample.SelLength := Length(fldInfoBoxSample.Text);
  fldInfoBoxSample.SelAttributes.Assign(fldInfoBoxSample.Font);
  fldInfoBoxSample.RescanHyperlinks;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmOptions.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
// Select the folder for archive copies
procedure TfrmOptions.btnSelectFolderClick(Sender: TObject);
begin
// QQtodo: folder
{
  if Length(fldSaveArchCopyTo.Text) > 0 then
    dlgOpenFolder.Directory := fldSaveArchCopyTo.Text;
  if dlgOpenFolder.Execute then
  begin
    fldSaveArchCopyTo.Text := dlgOpenFolder.Directory;
  end;
}
end;
//------------------------------------------------------------------------------
// Convert the list index to the TSnapSounds item
function ListIndexToSound(idx: Integer): TSounds;
begin
  Result := sndNONE;
  case idx of
    0: Result := sndERROR;
    1: Result := sndOPEN;
    2: Result := sndSAVE;
    3: Result := sndCLIPCOPYUSR;
    4: Result := sndCLIPCOPYPSW;
    5: Result := sndPASTE;
  end;
end;
//------------------------------------------------------------------------------
// One sounds list item activated, show its details
procedure TfrmOptions.lisSoundsClick(Sender: TObject);
var
  iSnd: TSounds;
begin
  iSnd := ListIndexToSound(lisSounds.ItemIndex);
  if iSnd <> sndNONE then
  begin
    chkOneSoundActive.Checked := TmpSoundsAry[iSnd].Active;
    if TmpSoundsAry[iSnd].UseInternal then
      rdiSoundBuiltIn.Checked := True
    else
      rdiSoundWaveFile.Checked := True;
    fldSoundWavePath.Text := TmpSoundsAry[iSnd].Path;
  end;
end;
//------------------------------------------------------------------------------
// Double-click - play the sound
procedure TfrmOptions.lisSoundsDblClick(Sender: TObject);
begin
  btnSoundTestClick(nil);
end;
//------------------------------------------------------------------------------
// Play the sound
procedure TfrmOptions.btnSoundTestClick(Sender: TObject);
var
  iSnd: TSounds;
begin
  iSnd := ListIndexToSound(lisSounds.ItemIndex);
  if iSnd <> sndNONE then
  begin
    SoundsAry[sndTEST].Active      := True;
    SoundsAry[sndTEST].Sound       := TmpSoundsAry[iSnd].Sound;
    SoundsAry[sndTEST].UseInternal := rdiSoundBuiltIn.Checked;
    SoundsAry[sndTEST].Path        := fldSoundWavePath.Text;
    PlayOneSound(sndTEST);
  end;
end;
//------------------------------------------------------------------------------
// Save the possibly changed parameters of the active sound
procedure TfrmOptions.btnSoundSaveClick(Sender: TObject);
var
  iSnd: TSounds;
begin
  iSnd := ListIndexToSound(lisSounds.ItemIndex);
  if iSnd <> sndNONE then
  begin
    TmpSoundsAry[iSnd].Active      := chkOneSoundActive.Checked;
    TmpSoundsAry[iSnd].UseInternal := rdiSoundBuiltIn.Checked;
    TmpSoundsAry[iSnd].Path        := fldSoundWavePath.Text;
  end;
end;
//------------------------------------------------------------------------------
// Select a wave file from the disk
procedure TfrmOptions.btnSoundWavePathClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(fldSoundWavePath.Text);
  dlgOpen.Filter := ilMiscStr(rsSoundFiles, 'rsSoundFiles') +  '(*.wav)|*.wav';
  if dlgOpen.Execute then
    fldSoundWavePath.Text := dlgOpen.FileName;
end;
//------------------------------------------------------------------------------
procedure TfrmOptions.btnHelpClick(Sender: TObject);
var
  sPage: String;
begin
  sPage := 'dlg_settings.htm';
  case tabOptions.ActivePage.PageIndex of
    0: sPage := 'dlg_settings_general.htm';
    1: sPage := 'dlg_settings_security.htm';
    2: sPage := 'dlg_settings_startup.htm';
    3: sPage := 'dlg_settings_saving.htm';
    4: sPage := 'dlg_settings_layout.htm';
    5: sPage := 'dlg_settings_columns.htm';
    6: sPage := 'dlg_settings_sounds.htm';
  end;
  frmMain.ShowHelpPage(sPage, Handle);
end;
//------------------------------------------------------------------------------
// Change the name of one of columns
procedure TfrmOptions.btnColRenameClick(Sender: TObject);
var
  sDftName, sUserName, sPmt: String;
begin
  if lvColViewNames.Selected <> nil then
  begin
    sDftName := lvColViewNames.Selected.Caption;
    sUserName := lvColViewNames.Selected.SubItems[0];
    sPmt := Format(ilMiscStr(rsEnterColName, 'rsEnterColName'), [sDftName]);
    // QQtodo: InputBox's buttons can't be translated
    sUserName := InputBox(sDftName, sPmt, sUserName);
    lvColViewNames.Selected.SubItems[0] := sUserName;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

