{*****************************************************************************
 *  PINs
 *  u_Password.pas - Password input dialog
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

unit u_Password;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls;

type
  PassWindowAction = (PWA_ACCEPT, PWA_CANCEL, PWA_MINIMIZE);

  TfrmPassword = class(TForm)
    outlblPrompt: TLabel;
    fldPassword: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    sttBar: TStatusBar;
    btnGenPassword: TSpeedButton;
    btnCharMap: TBitBtn;
    btnHelp: TButton;
    btnMinimize: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnGenPasswordClick(Sender: TObject);
    procedure btnCharMapClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
  private { Private declarations }

  public { Public declarations }
    sPrompt: String;
    sEnteredPassword: String;
    fCancel: Boolean;
    fGenPassword: Boolean;
    fBtnMinimizeVisible: Boolean;
    pwaRet: PassWindowAction; // how did we close this dialog box?
  end;

var
  frmPassword: TfrmPassword;

implementation

uses u_GenPassword, u_CharMap, u_Main, Messages;

{$R *.DFM}

//------------------------------------------------------------------------------
// Save / restore the dialog position
// One-time initialization
procedure TfrmPassword.FormCreate(Sender: TObject);
begin
  frmMain.ReadWindowPosition(self, False);
  sPrompt := '';
  fGenPassword := True;
  pwaRet := PWA_CANCEL;
end;
procedure TfrmPassword.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
// Form is about to be displayed
procedure TfrmPassword.FormActivate(Sender: TObject);
begin
  sEnteredPassword := '';
  outlblPrompt.Caption := sPrompt;
  fldPassword.Text := '';
  frmPassword.ActiveControl := fldPassword;
  btnGenPassword.Enabled := fGenPassword;
  pwaRet := PWA_CANCEL;
  fCancel := True;
  btnMinimize.Visible := fBtnMinimizeVisible;
end;
//------------------------------------------------------------------------------
// Entered password accepted
procedure TfrmPassword.btnOKClick(Sender: TObject);
begin
  sEnteredPassword := fldPassword.Text;
  fCancel := False;
  pwaRet := PWA_ACCEPT;
end;
//------------------------------------------------------------------------------
procedure TfrmPassword.btnGenPasswordClick(Sender: TObject);
begin
  frmGenPassword.Init(True);
  frmGenPassword.ShowModal;
  if (frmGenPassword.Accepted) then
  begin
    fldPassword.Text := frmGenPassword.Password;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmPassword.btnCharMapClick(Sender: TObject);
begin
  frmCharMap.ShowInsert := True;
  frmCharMap.ShowModal;
  if Length(frmCharMap.SelectedChars) > 0 then
  begin
    fldPassword.Text := fldPassword.Text + frmCharMap.SelectedChars;
  end;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmPassword.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
procedure TfrmPassword.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_password_input.htm', Handle);
end;
//------------------------------------------------------------------------------
// Minimize the application, leave entering the password for later
procedure TfrmPassword.btnMinimizeClick(Sender: TObject);
begin
//  frmMain.WindowState := wsMinimized; // testy
//  SendMessage(frmMain.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
//  SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  pwaRet := PWA_MINIMIZE;
  Close;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

