{*****************************************************************************
 *  PINs
 *  u_About.pas - About box
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

unit u_About;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellAPI, Dialogs;

resourcestring
  rsVersion='Version';
  rsThanksGoTo='My thanks go to:';

type
  TfrmAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    lblProductName: TLabel;
    outlblVersion: TLabel;
    outlblCopyright: TLabel;
    OKButton: TButton;
    Bevel1: TBevel;
    lblHttp1: TEdit;
    lblPmtWWW1: TLabel;
    Image2: TImage;
    Bevel2: TBevel;
    Image3: TImage;
    lblPmtE_mail1: TLabel;
    lblE_mail1: TEdit;
    lblBeta: TLabel;
    lblGPL: TLabel;
    lblSource: TLabel;
    Bevel3: TBevel;
    lblHTTP_PINsGroup: TEdit;
    Image5: TImage;
    btnCredits: TButton;
    lblProductName2: TLabel;
    imgPayPal: TImage;
    Image1: TImage;
    lblPmtForum: TLabel;
    lblForum: TEdit;
    procedure btnCreditsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblE_mail1Click(Sender: TObject);
    procedure lblHttp1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lblHTTP_PINsGroupClick(Sender: TObject);
    procedure imgPayPalClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblForumClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAboutBox: TfrmAboutBox;

implementation

uses
  u_Main, uMWStrings, IniLang;

{$R *.DFM}


//------------------------------------------------------------------------------
procedure TfrmAboutBox.FormCreate(Sender: TObject);
begin
  outlblVersion.Caption  := ilMiscStr(rsVersion, 'rsVersion') + ' ' +
                            frmMain.lblVsn.GetLabelText;
  lblBeta.Visible := frmMain.IsBeta;

  if VSN_STD then
  begin
    lblProductName.Caption := 'PINs, Secure Password Manager';
    lblProductName2.Caption := 'freeware';
    lblHttp1.Text := 'http://www.mirekw.com/';
    lblE_mail1.Text := 'info@mirekw.com';
  end
  else if VSN_HSO then
  begin
    lblProductName.Caption := 'HSO PINs Secure Password Manager';
    lblProductName2.Caption := '"Security for Everyone"';
    lblHttp1.Text := 'http://hso-usa.com/';
    lblE_mail1.Text := 'info@hso-usa.com';
  end
  else
  begin
    lblProductName.Caption := 'PINs, Secure Password Manager';
    lblProductName2.Caption := 'freeware';
    lblHttp1.Text := 'http://www.mirekw.com/';
    lblE_mail1.Text := 'info@mirekw.com';
  end;

//  'http://hso-usa.com'
end;
//------------------------------------------------------------------------------
procedure TfrmAboutBox.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmAboutBox.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
// Send me an e-mail
procedure TfrmAboutBox.lblE_mail1Click(Sender: TObject);
begin
//  ShellExecute(Handle,'open','mailto:info@mirekw.com','','',sw_Normal);
  ShellExecute(Handle,'open',PChar('mailto:' + lblE_mail1.Text + '?Subject=PINs ' + frmMain.lblVsn.GetLabelText),'','',sw_Normal);
end;
//------------------------------------------------------------------------------
// Visit my home page
procedure TfrmAboutBox.lblHttp1Click(Sender: TObject);
begin
  ShellExecute(Handle,'open', PChar(lblHttp1.Text),'','',sw_Normal);
  //ShellExecute(Handle,'open','http://www.mirekw.com/index.html','','',sw_Normal);
  //HLinkNavigateString(Nil,'http://www.mirwoj.opus.chelm.pl');
end;
procedure TfrmAboutBox.lblForumClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.mirekw.com/forum.html','','',sw_Normal);
end;
procedure TfrmAboutBox.lblHTTP_PINsGroupClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://groups.yahoo.com/group/PINs_news','','',sw_Normal);
end;
procedure TfrmAboutBox.imgPayPalClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.mirekw.com/donate/donate.html ','','',sw_Normal);
end;
//------------------------------------------------------------------------------
procedure TfrmAboutBox.btnCreditsClick(Sender: TObject);
begin
  // QQtodo: add new translators
  frmMain.CstMessageDlg(ilMiscStr(rsThanksGoTo, 'rsThanksGoTo') + S_CRLFLF +
             ' - Eugene Mayevski (Mayevski@eldos.org), for his wonderful ElPack suite of components' + S_CRLF +
             ' - David Barton (davebarton@bigfoot.com), for Delphi implementation of Blowfish algorithm' + S_CRLF +
             ' - Steven Michael, Michael Embree, Dave Dickson && others for betatesting and suggestions' + S_CRLF +
             ' - Markus F.X.J. Oberhumer && Laszlo Molnar for the UPX executables packer,' + S_CRLF +
             ' - Aleksandar Savic, Bobi Markov, Carlos Eduardo Ferreira Jr, door Tony, I. Pravica, ' + S_CRLF +
             '   John Rønning jr., Jon Aske, José Filipe Silva, Lars Grove Jørgensen, Martijn Dekker, ' + S_CRLF +
             '   Maxim Ganetsky, Moreno Trinca, Morten Lunde, Pau Bosch i Crespo, Peter Bongé, ' + S_CRLF +
             '   Preveo R. D.Goran, Raimund von der Emden, Renaud Pascal, Risto Saarinen, Rézsó Gábor, ' + S_CRLF +
             '   Sachab, Svyatoslav Bunkov, Tang Jian, Thomas Breinstrup, Tsung-Che Wu, Vlad Chursin, ' + S_CRLF +
             '   Zdenìk Jantaè and ¼ubomír Èerveò for translations.',
             mtInformation,
             [mbOk],
             0);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

