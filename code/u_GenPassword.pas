{*****************************************************************************
 *  PINs
 *  u_GenPassword.pas - Passwords generator
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

unit u_GenPassword;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Menus;

const
  FILE_PSWTPL = 'Passwords.tpl';

resourcestring
  rsPswChar_lcs='a lower case character (a,b,c,...,z)';
  rsPswChar_ucs='an upper case character (A,B,C,...,Z)';
  rsPswChar_vwl='a vowel (lower, upper case: a,e,i,...,y)';
  rsPswChar_cns='a consonant (lower, upper case: b,g,w,...,z)';
  rsPswChar_dgt='a digit (0,1,2,...,9)';
  rsPswChar_smb='a symbol (!,@,#,...)';
  rsPswChar_usr='a user-defined character';
  rsErrPswNoChar='You must allow at least one type of characters...';
  rsPswGenTpl01='Very simple, not safe password';
  rsPswGenTpl02='Simple, reasonably safe password';
  rsPswGenTpl03='Quite safe password';
  rsPswGenTpl04='Military-grade password';
  rsPswGenTpl05='4-digit PIN';
  rsPswGenTpl06='6-digit PIN';
  rsErrUserCharsEmpty='User characters set is empty!';

type
  TfrmGenPassword = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    grpMethod: TGroupBox;
    rdiPswLen: TRadioButton;
    rdiPswTpl: TRadioButton;
    fldPswLen: TEdit;
    fldPswTpl: TEdit;
    spnPswLen: TUpDown;
    btnPswTplHelp: TButton;
    grpAllowedChars: TGroupBox;
    chkAllowLCase: TCheckBox;
    chkAllowUCase: TCheckBox;
    chkAllowDigits: TCheckBox;
    chkAllowSymbols: TCheckBox;
    chkAllowUserChars: TCheckBox;
    fldPswUserChars: TEdit;
    grpPasswords: TGroupBox;
    lblPassword: TLabel;
    fldPsw: TEdit;
    btnClipCopy: TBitBtn;
    lisAllPsw: TListBox;
    btnClipCopyAll: TBitBtn;
    fldPswGenCnt: TEdit;
    spnPswGenCnt: TUpDown;
    btnCharMap: TBitBtn;
    btnHelp: TButton;
    btnCommonTemplates: TButton;
    lblCount: TLabel;
    btnGenerate: TButton;
    popupTemplates: TPopupMenu;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnOkClick(Sender: TObject);
    procedure btnClipCopyClick(Sender: TObject);
    procedure btnPswTplHelpClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure rdiPswLenClick(Sender: TObject);
    procedure rdiPswTplClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkAllowUserCharsClick(Sender: TObject);
    procedure btnClipCopyAllClick(Sender: TObject);
    procedure btnCharMapClick(Sender: TObject);
    procedure lisAllPswClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnCommonTemplatesClick(Sender: TObject);
    procedure popupTemplatesPopup(Sender: TObject);

  private { Private declarations }
    m_Accepted: Boolean;
    function GetPassword: String;
    procedure UpdateUI;
    procedure optTplSelClick(Sender: TObject);

  public { Public declarations }
    property Accepted: Boolean read m_Accepted;
    property Password: String read GetPassword;
    procedure Init(canAccept: Boolean);
  end;

var
  frmGenPassword: TfrmGenPassword;

implementation

uses
  uMWStrings, uMWTools, Clipbrd, u_Main, IniLang, u_CharMap;

const
  CH_LOC = 1;
  CH_UPC = 2;
  CH_DGT = 3;
  CH_SMB = 4;
  CH_CNS = 5;
  CH_VOW = 6;
  CH_USR = 7;


var
  PswChars: array [CH_LOC..CH_USR] of array of Char;

{$R *.DFM}

// #todo Password's quality

//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmGenPassword.FormCreate(Sender: TObject);
begin
  frmMain.ReadWindowPosition(self, False);
end;
procedure TfrmGenPassword.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
// 'canAccept' - if the dialog is used for generationg a password
// that should be next placed in any field. Then both 'Accept' and
// 'Cancel' buttons should be available.
procedure TfrmGenPassword.Init(canAccept: Boolean);
var
  i: Integer;
begin
  btnCancel.Visible := canAccept;
  btnOk.Caption := iifs(canAccept,
                        ilMiscStr(rsBtnAccept, 'rsBtnAccept'),
                        ilMiscStr(rsBtnClose, 'rsBtnClose'));

  fldPsw.Text := '';
  m_Accepted := False;

  // ['a'..'z'];
  SetLength(PswChars[CH_LOC], Ord('z') - Ord('a') + 1);
  for i := Ord('a') to Ord('z') do
    PswChars[CH_LOC][i - Ord('a')] := Chr(i);

  // ['A'..'Z'];
  SetLength(PswChars[CH_UPC], Ord('Z') - Ord('A') + 1);
  for i := Ord('A') to Ord('Z') do
    PswChars[CH_UPC][i - Ord('A')] := Chr(i);

  // ['0'..'9'];
  SetLength(PswChars[CH_DGT], 10);
  for i := Ord('0') to Ord('9') do
    PswChars[CH_DGT][i - Ord('0')] := Chr(i);

  // ['!', '@', '#', '$', '%', '^', '&', '*', '_', '-', '+', '=', '(', ')', '[', ']', '{', '}'];
  SetLength(PswChars[CH_SMB], 18);
  PswChars[CH_SMB][ 0] := '!';
  PswChars[CH_SMB][ 1] := '@';
  PswChars[CH_SMB][ 2] := '#';
  PswChars[CH_SMB][ 3] := '$';
  PswChars[CH_SMB][ 4] := '%';
  PswChars[CH_SMB][ 5] := '^';
  PswChars[CH_SMB][ 6] := '&';
  PswChars[CH_SMB][ 7] := '*';
  PswChars[CH_SMB][ 8] := '_';
  PswChars[CH_SMB][ 9] := '-';
  PswChars[CH_SMB][10] := '+';
  PswChars[CH_SMB][11] := '=';
  PswChars[CH_SMB][12] := '(';
  PswChars[CH_SMB][13] := ')';
  PswChars[CH_SMB][14] := '[';
  PswChars[CH_SMB][15] := ']';
  PswChars[CH_SMB][16] := '{';
  PswChars[CH_SMB][17] := '}';

  // ['a', 'e', 'i', 'o', 'u', 'y'];
  SetLength(PswChars[CH_VOW], 6);
  PswChars[CH_VOW][0] := 'a';
  PswChars[CH_VOW][1] := 'e';
  PswChars[CH_VOW][2] := 'i';
  PswChars[CH_VOW][3] := 'o';
  PswChars[CH_VOW][4] := 'u';
  PswChars[CH_VOW][5] := 'y';

  // ['a'..'z'] - PswChars[CH_VOW];
  SetLength(PswChars[CH_CNS], 20);
  PswChars[CH_CNS][ 0] := 'b';
  PswChars[CH_CNS][ 1] := 'c';
  PswChars[CH_CNS][ 2] := 'd';
  PswChars[CH_CNS][ 3] := 'f';
  PswChars[CH_CNS][ 4] := 'g';
  PswChars[CH_CNS][ 5] := 'h';
  PswChars[CH_CNS][ 6] := 'j';
  PswChars[CH_CNS][ 7] := 'k';
  PswChars[CH_CNS][ 8] := 'l';
  PswChars[CH_CNS][ 9] := 'm';
  PswChars[CH_CNS][10] := 'n';
  PswChars[CH_CNS][11] := 'p';
  PswChars[CH_CNS][12] := 'q';
  PswChars[CH_CNS][13] := 'r';
  PswChars[CH_CNS][14] := 's';
  PswChars[CH_CNS][15] := 't';
  PswChars[CH_CNS][16] := 'v';
  PswChars[CH_CNS][17] := 'w';
  PswChars[CH_CNS][18] := 'x';
  PswChars[CH_CNS][19] := 'z';

  SetLength(PswChars[CH_USR], Length(fldPswUserChars.Text));
  for i := 1 to Length(fldPswUserChars.Text) do
    PswChars[CH_USR][i - 1] := fldPswUserChars.Text[i];
end;
//------------------------------------------------------------------------------
// Initialize the dialog
procedure TfrmGenPassword.FormActivate(Sender: TObject);
begin
  if frmMain.ReadInteger('GenPassword', 'GenMethod', 1) = 1 then
    rdiPswTpl.Checked := True
  else
    rdiPswLen.Checked := True;

  fldPswTpl.Text            := frmMain.ReadString('GenPassword' , 'Template'    , 'Cvccvc99');
  spnPswLen.Position        := frmMain.ReadInteger('GenPassword', 'PasswLength' , 9);
  chkAllowLCase.Checked     := frmMain.ReadBool('GenPassword'   , 'AllowLCase'  , True);
  chkAllowUCase.Checked     := frmMain.ReadBool('GenPassword'   , 'AllowUCase'  , True);
  chkAllowDigits.Checked    := frmMain.ReadBool('GenPassword'   , 'AllowDigits' , True);
  chkAllowSymbols.Checked   := frmMain.ReadBool('GenPassword'   , 'AllowSymbols', True);
  chkAllowUserChars.Checked := frmMain.ReadBool('GenPassword'   , 'AllowUserChars', False);
  fldPswUserChars.Text      := frmMain.ReadString('GenPassword' , 'PswUserChars', '©®§');

  fldPswGenCnt.Hint := spnPswGenCnt.Hint;

  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
// The form is about to close - save settings
procedure TfrmGenPassword.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmMain.WriteInteger('GenPassword', 'GenMethod', iifi(rdiPswTpl.Checked, 1, 2));

  frmMain.WriteString('GenPassword' , 'Template'    , fldPswTpl.Text         );
  frmMain.WriteInteger('GenPassword', 'PasswLength' , spnPswLen.Position     );
  frmMain.WriteBool('GenPassword'   , 'AllowLCase'  , chkAllowLCase.Checked  );
  frmMain.WriteBool('GenPassword'   , 'AllowUCase'  , chkAllowUCase.Checked  );
  frmMain.WriteBool('GenPassword'   , 'AllowDigits' , chkAllowDigits.Checked );
  frmMain.WriteBool('GenPassword'   , 'AllowSymbols', chkAllowSymbols.Checked);
  frmMain.WriteBool('GenPassword'   , 'AllowUserChars', chkAllowUserChars.Checked);
  frmMain.WriteString('GenPassword' , 'PswUserChars', fldPswUserChars.Text);

  fldPswUserChars.Text := frmMain.ReadString('GenPassword' , 'PswUserChars', '©®§');
end;
//------------------------------------------------------------------------------
// Update the dialog logic
procedure TfrmGenPassword.UpdateUI;
begin
  grpAllowedChars.Enabled   := rdiPswLen.Checked;
  chkAllowLCase.Enabled     := rdiPswLen.Checked;
  chkAllowUCase.Enabled     := rdiPswLen.Checked;
  chkAllowDigits.Enabled    := rdiPswLen.Checked;
  chkAllowSymbols.Enabled   := rdiPswLen.Checked;

  chkAllowUserChars.Enabled := rdiPswLen.Checked;
  fldPswUserChars.Enabled   := chkAllowUserChars.Enabled and chkAllowUserChars.Checked;
  btnCharMap.Enabled        := fldPswUserChars.Enabled;

  fldPswTpl.Enabled := rdiPswTpl.Checked;
  btnCommonTemplates.Enabled := rdiPswTpl.Checked;
  btnPswTplHelp.Enabled := rdiPswTpl.Checked;

  fldPswLen.Enabled := rdiPswLen.Checked;
  spnPswLen.Enabled := rdiPswLen.Checked;

  if rdiPswTpl.Checked then
  begin
    rdiPswTpl.Font.Style   := [fsBold];
    rdiPswLen.Font.Style := [];
  end
  else
  begin
    rdiPswTpl.Font.Style   := [];
    rdiPswLen.Font.Style := [fsBold];
  end;
end;
//------------------------------------------------------------------------------
function TfrmGenPassword.GetPassword: String;
begin
  GetPassword := fldPsw.Text;
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.btnOkClick(Sender: TObject);
begin
  m_Accepted := True;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmGenPassword.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.btnClipCopyClick(Sender: TObject);
begin
  frmMain.DoPutPasswordOnClipboard(fldPsw.Text);
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.btnClipCopyAllClick(Sender: TObject);
var
  sBff: String;
  i: Integer;
begin
  sBff := '';
  for i := 0 to lisAllPsw.Items.Count - 1 do
    sBff := sBff + lisAllPsw.Items[i] + S_CRLF;

  frmMain.DoPutPasswordOnClipboard(sBff);
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.rdiPswLenClick(Sender: TObject);
begin
  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.rdiPswTplClick(Sender: TObject);
begin
  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.chkAllowUserCharsClick(Sender: TObject);
begin
  UpdateUI; // Update the dialog logic
end;
//------------------------------------------------------------------------------
// Show a hint on a password template
procedure TfrmGenPassword.btnPswTplHelpClick(Sender: TObject);
var
  sMsg: String;
begin
  sMsg :=        'l'   + Chr(9) + '- ' + ilMiscStr(rsPswChar_lcs, 'rsPswChar_lcs') + S_CRLF;
  sMsg := sMsg + 'U'   + Chr(9) + '- ' + ilMiscStr(rsPswChar_ucs, 'rsPswChar_ucs') + S_CRLF;
  sMsg := sMsg + 'v,V' + Chr(9) + '- ' + ilMiscStr(rsPswChar_vwl, 'rsPswChar_vwl') + S_CRLF;
  sMsg := sMsg + 'c,C' + Chr(9) + '- ' + ilMiscStr(rsPswChar_cns, 'rsPswChar_cns') + S_CRLF;
  sMsg := sMsg + '9'   + Chr(9) + '- ' + ilMiscStr(rsPswChar_dgt, 'rsPswChar_dgt') + S_CRLF;
  sMsg := sMsg + '#'   + Chr(9) + '- ' + ilMiscStr(rsPswChar_smb, 'rsPswChar_smb') + S_CRLF;
  sMsg := sMsg + 'X'   + Chr(9) + '- ' + ilMiscStr(rsPswChar_usr, 'rsPswChar_usr') + S_CRLF;
  frmMain.CstShowMessage(sMsg);
end;
//------------------------------------------------------------------------------
function GetOnePswChar(smb: Char): Char;
var
  retChar: Char;
begin
  retChar := '?';
  case smb of
    'l':
         retChar := PswChars[CH_LOC][Random(Length(PswChars[CH_LOC]))];
    'U':
         retChar := PswChars[CH_UPC][Random(Length(PswChars[CH_UPC]))];
    'v':
         retChar := PswChars[CH_VOW][Random(Length(PswChars[CH_VOW]))];
    'V':
         retChar := UpCase(PswChars[CH_VOW][Random(Length(PswChars[CH_VOW]))]);
    'c':
         retChar := PswChars[CH_CNS][Random(Length(PswChars[CH_CNS]))];
    'C':
         retChar := UpCase(PswChars[CH_CNS][Random(Length(PswChars[CH_CNS]))]);
    '9':
         retChar := PswChars[CH_DGT][Random(Length(PswChars[CH_DGT]))];
    '#':
         retChar := PswChars[CH_SMB][Random(Length(PswChars[CH_SMB]))];
    'X':
         retChar := PswChars[CH_USR][Random(Length(PswChars[CH_USR]))];
  end;
  GetOnePswChar := retChar;
end;
//------------------------------------------------------------------------------
// Generate passwords
procedure TfrmGenPassword.btnGenerateClick(Sender: TObject);
var
  i, cnt, pswCnt: Integer;
  allowedSymbs: array of Char;
  sTpl, sBff: String;
begin
  Randomize;
  lisAllPsw.Clear;
  fldPsw.Text := '';
  sTpl := '';

  if rdiPswLen.Checked and chkAllowUserChars.Checked and (Length(fldPswUserChars.Text) = 0) then
  begin
    frmMain.CstMessageDlg(ilMiscStr(rsErrUserCharsEmpty, 'rsErrUserCharsEmpty'), mtError, [mbOk], 0);
    Exit;
  end;

  // prepare the user chars array
  SetLength(PswChars[CH_USR], Length(fldPswUserChars.Text));
  for i := 1 to Length(fldPswUserChars.Text) do
    PswChars[CH_USR][i - 1] := fldPswUserChars.Text[i];

  // generate a number of passwords
  for pswCnt := 1 to spnPswGenCnt.Position do
  begin
    if rdiPswLen.Checked then // only length has been given
    begin
      cnt := iifi(chkAllowLCase.Checked, 1, 0);
      Inc(cnt, iifi(chkAllowUCase.Checked, 1, 0));
      Inc(cnt, iifi(chkAllowDigits.Checked, 1, 0));
      Inc(cnt, iifi(chkAllowSymbols.Checked, 1, 0));
      Inc(cnt, iifi(chkAllowUserChars.Checked, 1, 0));
      if (cnt > 0) then
      begin
        SetLength(allowedSymbs, cnt);
        i := 0;
        if chkAllowLCase.Checked then
        begin
          allowedSymbs[i] := 'l';
          Inc(i);
        end;
        if chkAllowUCase.Checked then
        begin
          allowedSymbs[i] := 'U';
          Inc(i);
        end;
        if chkAllowDigits.Checked then
        begin
          allowedSymbs[i] := '9';
          Inc(i);
        end;
        if chkAllowSymbols.Checked then
        begin
          allowedSymbs[i] := '#';
          Inc(i);
        end;
        if chkAllowUserChars.Checked and (Length(PswChars) > 0) then
        begin
          allowedSymbs[i] := 'X';
        end;

        // try to start with a character or at least with a digit
        if chkAllowUCase.Checked then
          sTpl := 'U'
        else if chkAllowLCase.Checked then
          sTpl := 'l'
        else if chkAllowDigits.Checked then
          sTpl := '9';

        // generate a random template
        for i := Length(sTpl) + 1 to spnPswLen.Position do
          sTpl := sTpl + allowedSymbs[Random(cnt)];
      end
      else
      begin
        frmMain.CstMessageDlg(ilMiscStr(rsErrPswNoChar, 'rsErrPswNoChar'), mtError, [mbOk], 0);
        Break;
      end;
    end
    else // a template has been specified
    begin
      sTpl := fldPswTpl.Text;
    end;

    if Length(sTpl) > 0 then
    begin
      sBff := '';
      for i := 1 to Length(sTpl) do
      begin
        sBff := sBff + GetOnePswChar(sTpl[i]);
      end;
      lisAllPsw.Items.Add(sBff);
    end;
  end;

  // activate the first password
  if lisAllPsw.Items.Count > 0 then
    fldPsw.Text := lisAllPsw.Items[0];
end;
//------------------------------------------------------------------------------
// Display the character map
procedure TfrmGenPassword.btnCharMapClick(Sender: TObject);
begin
  frmCharMap.ShowInsert := True;
  frmCharMap.ShowModal;
  if Length(frmCharMap.SelectedChars) > 0 then
  begin
    fldPswUserChars.Text := fldPswUserChars.Text + frmCharMap.SelectedChars;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.lisAllPswClick(Sender: TObject);
begin
  if lisAllPsw.ItemIndex >= 0 then
    fldPsw.Text := lisAllPsw.Items[lisAllPsw.ItemIndex];
end;
//------------------------------------------------------------------------------
procedure TfrmGenPassword.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_pass_gen.htm', Handle);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


procedure TfrmGenPassword.btnCommonTemplatesClick(Sender: TObject);
begin
  popupTemplates.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;
//------------------------------------------------------------------------------
// Fill the menu with templates
procedure TfrmGenPassword.popupTemplatesPopup(Sender: TObject);
var
  mnu: TMenuItem;
  strList: TStringList;
  i, iPos: Integer;
  sBff, sNam, sTpl: String;
begin
    while popupTemplates.Items.Count > 0 do
      popupTemplates.Items.Delete(0);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl01, 'rsPswGenTpl01'); //'Very simple, not safe password';
    mnu.Hint := 'Cvcv9';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl02, 'rsPswGenTpl02'); //'Simple reasonably safe password';
    mnu.Hint := 'Cvccvc99';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl03, 'rsPswGenTpl03'); //'Quite safe password';
    mnu.Hint := 'Cv#9cUcvCl9v';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl04, 'rsPswGenTpl04'); //'Military-grade password';
    mnu.Hint := 'UlCv#9#cUcvC#l9vllV##99v9Ul#cc';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl05, 'rsPswGenTpl05'); //'4-digit PIN';
    mnu.Hint := '9999';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.OnClick := optTplSelClick;
    mnu.Caption := ilMiscStr(rsPswGenTpl06, 'rsPswGenTpl06'); //'6-digit PIN';
    mnu.Hint := '999999';
    mnu.Checked := mnu.Hint = fldPswTpl.Text;
    popupTemplates.Items.Add(mnu);

    // add user templates
    if FileExists(MakePath(frmMain.PgmDir, FILE_PSWTPL)) then
    begin
      mnu := TMenuItem.Create(Self);
      mnu.Caption := '-';
      popupTemplates.Items.Add(mnu);

      strList := TStringList.Create;
      strList.LoadFromFile(MakePath(frmMain.PgmDir, FILE_PSWTPL));

      for i := 0 to strList.Count - 1 do
      begin
        sBff := Trim(strList[i]);
        if (not StrStartsWith(sBff, ';')) then
        begin
          iPos := Pos('=', sBff);
          if (iPos > 0) then
          begin
            sNam := Trim(LeftStr(sBff, iPos - 1));
            sTpl := Trim(Copy(sBff, iPos + 1, Length(sBff)));
            if (Length(sNam) * Length(sTpl) > 0) then
            begin
              mnu := TMenuItem.Create(Self);
              mnu.OnClick := optTplSelClick;
              mnu.Caption := sNam;
              mnu.Hint := sTpl;
              mnu.Checked := mnu.Hint = fldPswTpl.Text;
              popupTemplates.Items.Add(mnu);
            end;
          end;
        end;
      end;
    end;

    // cancel
    mnu := TMenuItem.Create(Self);
    mnu.Caption := '-';
    popupTemplates.Items.Add(mnu);

    mnu := TMenuItem.Create(Self);
    mnu.Caption := ilMiscStr(rsBtnCancel, 'rsBtnCancel');
    popupTemplates.Items.Add(mnu);

end;
//------------------------------------------------------------------------------
// One template has been selected from the list
procedure TfrmGenPassword.optTplSelClick(Sender: TObject);
begin
  fldPswTpl.Text := TMenuItem(Sender).Hint;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
