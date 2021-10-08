{*****************************************************************************
 *  PINs
 *  u_FileWipe.pas - Secure files wiping
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

unit u_FileWipe;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, FileCtrl, ExtCtrls, ShellAPI,
  uMWErrorLog;

const
  WIPELOGFILE = 'Wipe.log';

const
  rsSureDeleteFile='Are you sure you want to wipe the file(s)';
  rsWipeNoUndo='This operation cannot be undone!';
  rsWipeFolderWarning1='Warning!*|*This action is very dangerous!';
  rsWipeFolderWarning2='All specified files in the folder*|*[%s]*|*will be erased!';
  rsWipeContinue='Do you want to continue?';
  rsWipeFolderSure='Are you sure you know what you are doing?';
  rsDirXNotFound='Folder*|*[%s]*|*not found!';
  rsErrWipeFileOpen='Error opening the file*|*[%s]*|*for wiping.*|**|*System report: %s';
  rsErrWipeFileRename='Error renaming file*|*[%s]';
  rsErrWipeFileDelete='Error deleting file*|*[%s]';
  rsSomeDirsNotDeleted='Some subfolders couldn''t be deleted (probably they are not empty)';
  rsErrWipeNoLogFile='Log file does not exist';

type
  FWipeProgress = procedure(iPos, iSiz: Integer);

  TPassBytes = array[0..2] of Byte;
  TPassData = record
    isRandom: Boolean;
    bytes: TPassBytes;
  end;

  TfrmFileWipe = class(TForm)
    grpMethod: TGroupBox;
    rdi1pass: TRadioButton;
    rdiDoD3: TRadioButton;
    rdiDoD7: TRadioButton;
    rdiGutmann35: TRadioButton;
    rdiCustom: TRadioButton;
    fldCustomPattern: TEdit;
    grpSource: TGroupBox;
    fldFileToWipe: TEdit;
    btnWipe: TButton;
    rdiWhat_File: TRadioButton;
    btnSelectFile: TBitBtn;
    rdiWhat_Folder: TRadioButton;
    fldFolderToWipe: TEdit;
    btnSelectFolder: TBitBtn;
    chkSubfolders: TCheckBox;
    btnClose: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    dlgOpenFile: TOpenDialog;
    lblMask: TLabel;
    fldMask: TEdit;
    sttBar: TStatusBar;
    btnBreak: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    lblWhatsThis: TLabel;
    btnLogsView: TButton;
    chkLogging: TCheckBox;
    btnLogsClear: TButton;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnWipeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnBreakClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rdiWhatClick(Sender: TObject);
    procedure fldCustomPatternChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSelectFolderClick(Sender: TObject);
    procedure btnLogsViewClick(Sender: TObject);
    procedure btnLogsClearClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);

  private { Private declarations }
    m_FoldersRemoved: Boolean;
    m_LogFile: TMWErrorLog;
    procedure UpdateUI;
    procedure WipeFile(sFileName: String);
    procedure WipeFolder(sRootDir, sDir, sFileMask: String; withSubfolders: Boolean);
    procedure RemoveFolderTree(sRootDir, sDir: String);
    procedure DoWipeFile(sr: TFileStream; b: TPassData; fnc: FWipeProgress);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure WMDropFiles(hDrop: THandle; hWindow: HWnd);

  public { Public declarations }
  end;

var
  frmFileWipe: TfrmFileWipe;

implementation

uses
  uMWStrings, uMWTools, u_Main, IniLang;

{$R *.DFM}

//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmFileWipe.FormCreate(Sender: TObject);
var
  fTemp: Boolean;
  iMtd: Integer;
begin
  DragAcceptFiles(Handle, True);
  frmMain.ReadWindowPosition(self, False);

  fldFileToWipe.Text    := frmMain.ReadString('FileWipe', 'FileToWipe', 'c:\temp\mypasswords.txt');
  fldFolderToWipe.Text  := frmMain.ReadString('FileWipe', 'FolderToWipe', 'c:\temp\');
  chkSubfolders.Checked := frmMain.ReadBool('FileWipe', 'Subfolders', False);
  fldMask.Text          := frmMain.ReadString('FileWipe', 'Mask', '*.bak');
  chkLogging.Checked    := frmMain.ReadBool('FileWipe', 'Logging', True);
  fTemp                 := frmMain.ReadBool('FileWipe', 'What_File', True);
  if fTemp then
    rdiWhat_File.Checked := True
  else
    rdiWhat_Folder.Checked := True;
  fldCustomPattern.Text := frmMain.ReadString('FileWipe', 'CustomPattern', '0,255,r,0,255,r,r,r');
  iMtd := frmMain.ReadInteger('FileWipe', 'Method', 2);
  case iMtd of
    1: rdi1pass.Checked := True;
    3: rdiDoD7.Checked := True;
    4: rdiGutmann35.Checked := True;
    5: rdiCustom.Checked := True;
  else
    rdiDoD3.Checked := True;
  end;

  m_LogFile := TMWErrorLog.Create;
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.FormClose(Sender: TObject; var Action: TCloseAction);
var
  iMtd: Integer;
begin
  Application.OnMessage := Nil;
  DragAcceptFiles(Handle, False);

  frmMain.WriteString('FileWipe', 'FileToWipe', fldFileToWipe.Text);
  frmMain.WriteString('FileWipe', 'FolderToWipe', fldFolderToWipe.Text);
  frmMain.WriteBool('FileWipe', 'Subfolders', chkSubfolders.Checked);
  frmMain.WriteString('FileWipe', 'Mask', fldMask.Text);
  frmMain.WriteBool('FileWipe', 'What_File', rdiWhat_File.Checked);
  frmMain.WriteBool('FileWipe', 'Logging', chkLogging.Checked);
  frmMain.WriteString('FileWipe', 'CustomPattern', fldCustomPattern.Text);

  if rdi1pass.Checked then
    iMtd := 1
  else if rdiDoD7.Checked then
    iMtd := 3
  else if rdiGutmann35.Checked then
    iMtd := 4
  else if rdiCustom.Checked then
    iMtd := 5
  else
    iMtd := 2;
  frmMain.WriteInteger('FileWipe', 'Method', iMtd);
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.FormActivate(Sender: TObject);
begin
  Application.OnMessage := AppMessage;
  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  case Msg.Message of
    WM_DROPFILES:
                  begin
                    WMDropFiles (Msg.wParam, Msg.hWnd);
                    Handled := True;
                  end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.WMDropFiles(hDrop: THandle; hWindow: HWnd);
var
  cntFiles, nFileLength: Integer;
  pszFileName: PChar;
  i: Integer;
begin
  cntFiles := DragQueryFile(hDrop, $FFFFFFFF, Nil, 0);

  if cntFiles > 0 then
  begin
    rdiWhat_File.Checked := True;
    UpdateUI; // Update the dialog logic
    fldFileToWipe.Text := '';
    fldFileToWipe.SetFocus;
    for i := 0 to cntFiles - 1 do
    begin
      nFileLength := DragQueryFile(hDrop, i, nil, 0) + 1;
      GetMem(pszFileName, nFileLength);
      DragQueryFile(hDrop , i, pszFileName, nFileLength);

      fldFileToWipe.Text := fldFileToWipe.Text + '"' + pszFileName + '" ';

      FreeMem(pszFileName, nFileLength);
    end;
  end;

  DragFinish (hDrop);
end;
//------------------------------------------------------------------------------
// Select the file(s)
procedure TfrmFileWipe.btnSelectFileClick(Sender: TObject);
var
  i: Integer;
  sTok: String;
begin
  i := 1;
  sTok := Trim(GetNextToken(Trim(fldFileToWipe.Text), i, '"'));
  if Length(sTok) > 0 then
    dlgOpenFile.InitialDir := ExtractFilePath(sTok);
  dlgOpenFile.Options := dlgOpenFile.Options + [ofAllowMultiSelect, ofEnableSizing, ofHideReadOnly];
  if dlgOpenFile.Execute then
  begin
    fldFileToWipe.Text := '';
    for i := 0 to dlgOpenFile.Files.Count - 1 do
    begin
      fldFileToWipe.Text := fldFileToWipe.Text + '"' + dlgOpenFile.Files[i] + '" ';
    end;
  end;
end;
//------------------------------------------------------------------------------
// Select the folder
procedure TfrmFileWipe.btnSelectFolderClick(Sender: TObject);
begin
//QQtodo: folder
{
  if Length(fldFolderToWipe.Text) > 0 then
    dlgOpenFolder.Directory := fldFolderToWipe.Text;
  if dlgOpenFolder.Execute then
  begin
    fldFolderToWipe.Text := dlgOpenFolder.Directory;
  end;
}
end;
//------------------------------------------------------------------------------
// Update the dialog logic
procedure TfrmFileWipe.UpdateUI;
var
  isFile: Boolean;
begin
  isFile := rdiWhat_File.Checked;
  fldFileToWipe.Enabled := isFile;
  btnSelectFile.Enabled := isFile;

  fldFolderToWipe.Enabled := not isFile;
  btnSelectFolder.Enabled := not isFile;
  chkSubfolders.Enabled := not isFile;
  lblMask.Enabled := not isFile;
  fldMask.Enabled := not isFile;

  if isFile then
  begin
    rdiWhat_File.Font.Style   := [fsBold];
    rdiWhat_Folder.Font.Style := [];
  end
  else
  begin
    rdiWhat_File.Font.Style   := [];
    rdiWhat_Folder.Font.Style := [fsBold];
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.rdiWhatClick(Sender: TObject);
begin
  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.fldCustomPatternChange(Sender: TObject);
begin
  rdiCustom.Checked := True;
end;
//------------------------------------------------------------------------------
var
  WipeBreak: Boolean;
//------------------------------------------------------------------------------
// Interrupt!
procedure TfrmFileWipe.btnBreakClick(Sender: TObject);
begin
  WipeBreak := True;
end;
//------------------------------------------------------------------------------
// Update the file progress bar
procedure WipeProgress(iPos, iSiz: Integer);
begin
  frmFileWipe.ProgressBar1.Max := iSiz;
  frmFileWipe.ProgressBar1.Position := iPos;
  Application.ProcessMessages;
end;
//------------------------------------------------------------------------------
// Low-level function filling the open file with passed data
procedure TfrmFileWipe.DoWipeFile(sr: TFileStream; b: TPassData; fnc: FWipeProgress);
var
  bff: array of Byte;
  i, iSiz: Integer;
begin
  SetLength(bff, 1152); // must be a multiply of 3
  if b.isRandom then
  begin
    for i := 0 to Length(bff) - 1 do
      bff[i] := RandomInRange(0, 255);
  end
  else
  begin
    i := 0;
    while i <= Length(bff) - 3 do
    begin
      bff[i+0] := b.bytes[0];
      bff[i+1] := b.bytes[1];
      bff[i+2] := b.bytes[2];
      Inc(i, 3);
    end;
  end;

  // wipe the main part of the file
  sr.Seek(0, soFromBeginning);
  iSiz := sr.Size;
  while (not WipeBreak) and (sr.Position <= iSiz - Length(bff)) do
  begin
    sr.Write(bff[0], Length(bff));
    if Assigned(fnc) then
      fnc(sr.Position, iSiz);
  end;

  // wipe the rest of the file
  i := 0;
  while (not WipeBreak) and (sr.Position < iSiz) do
  begin
    sr.Write(bff[i], 1);
    Inc(i);
    if (i >= Length(bff)) then // *should* never happen...
      i := 0;
    if Assigned(fnc) then
      if i mod 10 = 0 then // just not too often
        fnc(sr.Position, iSiz);
  end;
end;
//------------------------------------------------------------------------------
function PassData: TPassData; overload;
var
  tmpDta: TPassData;
begin
  tmpDta.isRandom := True;
  Result := tmpDta;
end;
//------------------------------------------------------------------------------
function PassData(b: Byte): TPassData; overload;
var
  tmpDta: TPassData;
begin
  tmpDta.isRandom := False;
  tmpDta.bytes[0] := b;
  tmpDta.bytes[1] := b;
  tmpDta.bytes[2] := b;
  Result := tmpDta;
end;
//------------------------------------------------------------------------------
function PassData(b1, b2, b3: Byte): TPassData; overload;
var
  tmpDta: TPassData;
begin
  tmpDta.isRandom := False;
  tmpDta.bytes[0] := b1;
  tmpDta.bytes[1] := b2;
  tmpDta.bytes[2] := b3;
  Result := tmpDta;
end;
//------------------------------------------------------------------------------
// DoD: http://www.dss.mil/isec/chapter8.htm
// Gutmann: http://www.cs.auckland.ac.nz/~pgut001/
//          http://www.cs.auckland.ac.nz/~pgut001/pubs/secure_del.html
procedure TfrmFileWipe.WipeFile(sFileName: String);
var
  sr: TFileStream;
  wipeBytes: array of TPassData;
  i, i1, i2, iMax: Integer;
  tmpDta: TPassData;
  sTok, sBff, sNewName: String;
begin
  m_LogFile.WriteString('    >> File ' + sFileName + '... ', False);
  Randomize;
  sttBar.SimpleText := sFileName;
  if rdi1pass.Checked then // one random pass
  begin
    SetLength(wipeBytes, 1);
    wipeBytes[0] := PassData;
  end
  else if rdiDoD3.Checked then // DoD 3 pass
  begin
    SetLength(wipeBytes, 3);
    wipeBytes[0] := PassData(0);
    wipeBytes[1] := PassData($FF);
    wipeBytes[2] := PassData; // pseudorandom
  end
  else if rdiDoD7.Checked then // DoD 7 pass
  begin
    SetLength(wipeBytes, 7);
    wipeBytes[0] := PassData(00);
    wipeBytes[1] := PassData($FF);
    wipeBytes[2] := PassData; // pseudorandom
    wipeBytes[3] := PassData; // pseudorandom
    wipeBytes[4] := PassData; // pseudorandom
    wipeBytes[5] := PassData(0);
    wipeBytes[6] := PassData($FF);
  end
  else if rdiGutmann35.Checked then // Gutmann 35 pass
  begin
    SetLength(wipeBytes, 35);
    wipeBytes[ 0] := PassData; // pseudorandom
    wipeBytes[ 1] := PassData; // pseudorandom
    wipeBytes[ 2] := PassData; // pseudorandom
    wipeBytes[ 3] := PassData; // pseudorandom
    wipeBytes[ 4] := PassData($55);
    wipeBytes[ 5] := PassData($AA);
    wipeBytes[ 6] := PassData($92, $49, $24);
    wipeBytes[ 7] := PassData($49, $24, $92);
    wipeBytes[ 8] := PassData($24, $92, $49);
    wipeBytes[ 9] := PassData($00);
    wipeBytes[10] := PassData($11);
    wipeBytes[11] := PassData($22);
    wipeBytes[12] := PassData($33);
    wipeBytes[13] := PassData($44);
    wipeBytes[14] := PassData($55);
    wipeBytes[15] := PassData($66);
    wipeBytes[16] := PassData($77);
    wipeBytes[17] := PassData($88);
    wipeBytes[18] := PassData($99);
    wipeBytes[19] := PassData($AA);
    wipeBytes[20] := PassData($BB);
    wipeBytes[21] := PassData($CC);
    wipeBytes[22] := PassData($DD);
    wipeBytes[23] := PassData($EE);
    wipeBytes[24] := PassData($FF);
    wipeBytes[25] := PassData($92, $49, $24);
    wipeBytes[26] := PassData($49, $24, $92);
    wipeBytes[27] := PassData($24, $92, $49);
    wipeBytes[28] := PassData($6D, $B6, $DB);
    wipeBytes[29] := PassData($B6, $DB, $6D);
    wipeBytes[30] := PassData($DB, $6D, $B6);
    wipeBytes[31] := PassData; // pseudorandom
    wipeBytes[32] := PassData; // pseudorandom
    wipeBytes[33] := PassData; // pseudorandom
    wipeBytes[34] := PassData; // pseudorandom
    // shuffle fixed passes to further increase security
    for i := 1 to 10 do
    begin
      i1 := RandomInRange(4, 30);
      repeat
        i2 := RandomInRange(4, 30);
      until (i1 <> i2);
      tmpDta := wipeBytes[i1];
      wipeBytes[i1] := wipeBytes[i2];
      wipeBytes[i2] := tmpDta;
    end;
  end
  else // custom sequence
  begin
    i := 1;
    SetLength(wipeBytes, 0);
    sTok := Trim(GetNextToken(fldCustomPattern.Text, i, ','));
    while Length(sTok) > 0 do
    begin
      SetLength(wipeBytes, Length(wipeBytes) + 1);
      if (UpperCase(sTok) = 'R') then
        wipeBytes[Length(wipeBytes) - 1] := PassData
      else
        wipeBytes[Length(wipeBytes) - 1] := PassData(CvtStr2Int(sTok));
      sTok := Trim(GetNextToken(fldCustomPattern.Text, i, ','));
    end;
  end;

  iMax := Length(wipeBytes);
  ProgressBar1.Visible := True;
  ProgressBar2.Visible := True;

  ProgressBar1.Position := 0;
  ProgressBar1.Max := 0;

  ProgressBar2.Position := 0;
  ProgressBar2.Max := iMax;
  try
    for i := 0 to iMax - 1 do
    begin
      sr := TFileStream.Create(sFileName, fmOpenReadWrite + fmShareExclusive);
      DoWipeFile(sr, wipeBytes[i], WipeProgress);
      sr.Free; // close the file to force full flush
      ProgressBar2.Position := i - 1;
      if (WipeBreak) then
        Exit;
    end;

    if (not WipeBreak) then // rename and delete the file
    begin
      sBff := '';
      for i := 1 to Length(ExtractFileName(sFileName)) do
        sBff := sBff + Chr(RandomInRange(Ord('A'), Ord('Z'))); // safe new name
      sNewName := MakePath(ExtractFilePath(sFileName), sBff);
      if not RenameFile(sFileName, sNewName) then
      begin
        sBff := Format(ilMiscStr(rsErrWipeFileRename, 'rsErrWipeFileRename'), [sFileName]);
        frmMain.CstShowMessage(sBff);
        m_LogFile.WriteString('  Error! ' + sBff);
      end;
      if not DeleteFile(sNewName) then
      begin
        sBff := Format(ilMiscStr(rsErrWipeFileDelete, 'rsErrWipeFileDelete'), [sNewName]);
        frmMain.CstShowMessage(sBff);
        m_LogFile.WriteString('  Error! ' + sBff);
      end;
    end;
  except
    on E: Exception do
    begin
      sBff := Format(ilMiscStr(rsErrWipeFileOpen, 'rsErrWipeFileOpen'), [sFileName, E.Message]);
      frmMain.CstShowMessage(sBff);
      m_LogFile.WriteString('  Error! ' + sBff);
    end;
  end;

  ProgressBar1.Visible := False;
  ProgressBar2.Visible := False;
  sttBar.SimpleText := '';

  if (not WipeBreak) then
    m_LogFile.WriteString('wiped')
  else
    m_LogFile.WriteString('Break!');
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.WipeFolder(sRootDir, sDir, sFileMask: String; withSubfolders: Boolean);
var
  SearchRec: TSearchRec;
begin
  if WipeBreak then
    Exit;

  // first files
  m_LogFile.WriteString('  > Analyzing folder ' + sDir);
  if FindFirst(MakePath(sDir, sFileMask), faArchive{faReadOnly}, SearchRec) = 0 then
  begin
    repeat
      WipeFile(MakePath(sDir, SearchRec.Name));
    until (FindNext(SearchRec) <> 0) or WipeBreak;
  end;
  FindClose(SearchRec);

  // next subfolders
  if withSubfolders then
  begin
    if FindFirst(MakePath(sDir, '*.*'), faDirectory, SearchRec) = 0 then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          WipeFolder(sRootDir, MakePath(sDir, SearchRec.Name + '\'), sFileMask, withSubfolders);
      until (FindNext(SearchRec) <> 0) or WipeBreak;
      FindClose(SearchRec);
    end;
    FindClose(SearchRec);
  end;
end;
//------------------------------------------------------------------------------
// Remove the full folders tree (only folders that are empty)
procedure TfrmFileWipe.RemoveFolderTree(sRootDir, sDir: String);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(MakePath(sDir, '*.*'), faDirectory, SearchRec) = 0 then
  begin
    repeat
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        RemoveFolderTree(sRootDir, MakePath(sDir, SearchRec.Name + '\'));
        m_LogFile.WriteString('    >> Folder ' + MakePath(sDir, SearchRec.Name) + '... ', False);
        if not RemoveDirectory(PChar(MakePath(sDir, SearchRec.Name))) then
        begin
          m_FoldersRemoved := False; // some folders couldn't be deleted
          m_LogFile.WriteString('NOT deleted')
        end
        else
          m_LogFile.WriteString('deleted')
      end;
    until (FindNext(SearchRec) <> 0) or WipeBreak;
  end;
  FindClose(SearchRec);
end;
//------------------------------------------------------------------------------
// Initialize wiping
procedure TfrmFileWipe.btnWipeClick(Sender: TObject);
var
  sFileName, sRoot: String;
  sMsg, sTok: String;
  i: Integer;
begin
  if chkLogging.Checked then
  begin
    m_LogFile.Init(MakePath(frmMain.PgmDir, WIPELOGFILE), False);
  end;
  m_LogFile.Activate(chkLogging.Checked);
  m_LogFile.TimeStamp := False;

  WipeBreak := False;
  btnBreak.Visible := True;
  btnWipe.Enabled := False;
  btnClose.Enabled := False;
  m_LogFile.WriteString('=========================================');
  m_LogFile.WriteString('Wipe process started, ' + DateTimeToStr(Now));
  if rdiWhat_File.Checked then
  begin
    m_LogFile.WriteString('File(s): ' + fldFileToWipe.Text);
    sFileName := Trim(fldFileToWipe.Text);
    if Length(Trim(sFileName)) > 0 then
    begin
      sMsg := Format(ilMiscStr(rsSureDeleteFile, 'rsSureDeleteFile'), [sFileName]);
      sMsg := sMsg + Chr(13) + Chr(10) + sFileName;
      sMsg := sMsg + Chr(13) + Chr(10) + Chr(10) + ilMiscStr(rsWipeNoUndo, 'rsWipeNoUndo');
      if (frmMain.CstMessageDlg(sMsg, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
      begin
        i := 1;
        sTok := GetNextToken(sFileName, i, '"');
        while Length(sTok) > 0 do
        begin
          sTok := Trim(sTok);
          if Length(sTok) > 0 then
          begin
            if FileExists(sTok) then
              WipeFile(sTok)
            else
            begin
              sMsg := Format(ilMiscStr(rsFileXNotFound, 'rsFileXNotFound'), [sFileName]);
              frmMain.CstShowMessage(sMsg);
              m_LogFile.WriteString('  Error! ' + sMsg);
            end;
          end;
          sTok := GetNextToken(sFileName, i, '"');
        end;
      end;
    end;
  end
  else // folder
  begin
    m_LogFile.WriteString('Folder: ' + fldFolderToWipe.Text + ', mask: ' + fldMask.Text);
    sRoot := fldFolderToWipe.Text;
    if DirectoryExists(sRoot) then
    begin
      sMsg := ilMiscStr(rsWipeFolderWarning1, 'rsWipeFolderWarning1');
      sMsg := sMsg + S_CRLFLF + Format(ilMiscStr(rsWipeFolderWarning2, 'rsWipeFolderWarning2'), [sRoot]);
      sMsg := sMsg + S_CRLFLF + ilMiscStr(rsWipeContinue, 'rsWipeContinue');
      if (frmMain.CstMessageDlg(sMsg, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
      begin
        sMsg := ilMiscStr(rsWipeFolderSure, 'rsWipeFolderSure');
        sMsg := sMsg + S_CRLFLF + ilMiscStr(rsWipeNoUndo, 'rsWipeNoUndo');
        if (frmMain.CstMessageDlg(sMsg, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
        begin
          WipeFolder(sRoot, sRoot, fldMask.Text, chkSubfolders.Checked);
          if chkSubfolders.Checked then
          begin
            m_LogFile.WriteString('  > Removing folder tree ' + sRoot);
            m_FoldersRemoved := True;
            RemoveFolderTree(sRoot, sRoot);
            if (not m_FoldersRemoved) then
            begin
              sMsg := ilMiscStr(rsSomeDirsNotDeleted, 'rsSomeDirsNotDeleted');
              frmMain.CstShowMessage(sMsg);
              m_LogFile.WriteString('  Warning! ' + sMsg);
            end;
          end;
        end;
      end;
    end
    else
    begin
      sMsg := Format(ilMiscStr(rsDirXNotFound, 'rsDirXNotFound'), [sRoot]);
      frmMain.CstShowMessage(sMsg);
      m_LogFile.WriteString('  Warning! ' + sMsg);
    end;
  end;
  btnWipe.Enabled := True;
  btnClose.Enabled := True;
  btnBreak.Visible := False;
  m_LogFile.WriteString('');
end;
//------------------------------------------------------------------------------
// Execute the given program
function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..179] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;
//------------------------------------------------------------------------------
// Show the log file
procedure TfrmFileWipe.btnLogsViewClick(Sender: TObject);
begin
  if (FileExists(MakePath(frmMain.PgmDir, WIPELOGFILE))) then
    ExecuteFile('notepad.exe', MakePath(frmMain.PgmDir, WIPELOGFILE), '', SW_SHOW)
  else
    frmMain.CstShowMessage(ilMiscStr(rsErrWipeNoLogFile, 'rsErrWipeNoLogFile'));
end;
//------------------------------------------------------------------------------
// Wipe the log file
procedure TfrmFileWipe.btnLogsClearClick(Sender: TObject);
begin
  if (FileExists(MakePath(frmMain.PgmDir, WIPELOGFILE))) then
  begin
    if (frmMain.CstMessageDlg(ilMiscStr(rsAreYouSure, 'rsAreYouSure'), mtConfirmation, mbYesNoCancel, 0) = mrYes) then
    begin
      m_LogFile.Activate(False);
      WipeFile(MakePath(frmMain.PgmDir, WIPELOGFILE));
    end;
  end
  else
    frmMain.CstShowMessage(ilMiscStr(rsErrWipeNoLogFile, 'rsErrWipeNoLogFile'));
end;
//------------------------------------------------------------------------------
procedure TfrmFileWipe.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_wipe.htm', Handle);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

