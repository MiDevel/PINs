{*****************************************************************************
 *
 *  u_SoftwareCheck.pas - Check, if a newer version of a file/application is available
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

unit u_SoftwareCheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

resourcestring
  rsYourProgram='Your program';
  rsVersionAbbrev='v.';
  rsToCheckNewProgram='In order to check if a new version of %s is available, click on [Check now].';
  rsWarning='WARNING!';
  rsNeedsInternet='This option needs an access to the Internet.';
  rsUpdateExists='There exists an update for your version of the program. Details:';
  rsYourVersion='Your version';
  rsAvailableUpdate='Available update';
  rsVersion='Version';
  rsDate='Date';
  rsURL='Address';
  rsContactingServer='Connecting to server';
  rsPleaseWait='Please wait...';
  rsError='Error';
  rsErrorReadingUpdates='An error occurred while reading the list of available updates. Please try later or use different Proxy settings.';
  rsFailedReadingUpdatesList='I wasn''t able to read the list of available updates.';
  rsNeedsAuthent='Authentication required';
  rsEnterUser='Enter a user ID';
  rsEnterPass='Enter a password';
  rsAuthEntered='Authentication information in place, please retry the previous command';
  rsNoUpdatesExist='You have the latest version of the program.';
  rsNoDownloadAccel='Please note that my site does NOT allow using download accelerators. Download software using your browser.';

type
  TfrmSoftwareCheck = class(TForm)
    lblTitle: TLabel;
    btnCheck: TButton;
    btnCancel: TButton;
    memInfo: TMemo;
    btnGet: TButton;
    btnCopy: TButton;
    imgLogo: TImage;
    btnNews: TButton;
    sttBar: TStatusBar;
    btnProxy: TButton;
    procedure btnCheckClick(Sender: TObject);
    procedure nmHTTPAboutToSend(Sender: TObject);
//    procedure nmHTTPFailure(Cmd: CmdType);
    procedure nmHTTPAuthenticationNeeded(Sender: TObject);
//    procedure nmHTTPSuccess(Cmd: CmdType);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGetClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnNewsClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnProxyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

  private  { Private declarations }
    m_sThisVersion: String;
    m_sThisProgram: String;
    m_sThisID     : String; // Program's ID in the version file
    m_sVersionFile: String; // URL of the versions file

    m_InetUseProxy: Boolean;
    m_InetProxyAddress: String;
    m_InetProxyPort: Integer;

    function UpdateExists(sBff: String): Boolean;

  published  { Published declarations }
    property ThisVersion: String read m_sThisVersion write m_sThisVersion;
    property ThisProgram: String read m_sThisProgram write m_sThisProgram;
    property ThisID     : String read m_sThisID      write m_sThisID;
    property VersionFile: String read m_sVersionFile write m_sVersionFile;

    property InetUseProxy       : Boolean read m_InetUseProxy       write m_InetUseProxy;
    property InetProxyAddress   : String  read m_InetProxyAddress   write m_InetProxyAddress;
    property InetProxyPort      : Integer read m_InetProxyPort      write m_InetProxyPort;

  public  { Public declarations }
  end;

var
  frmSoftwareCheck: TfrmSoftwareCheck;

implementation

uses
  ShellAPI, Clipbrd, uMWStrings, IniLang, u_ProxySettings, u_Main, ScktComp;

var
  sDownloadPath: String;
  sNewsPath: String;
//  tmpnmHTTP: TNMHTTP;
  IsCancelled: Boolean;

{$R *.DFM}


//------------------------------------------------------------------------------
// Download the specified file from the web site
function DownloadFile(sURL: String; cliSock: TClientSocket): String;
var
  intReturnCode: Integer;
  sCmd, sRet: String;
  szBuffer: array[0..1014] of Char;
  strHost, strRemoteFileName: String;

  procedure SplitURL;
  var
    iPos: Integer;
  begin
    if StrStartsWith(sURL, 'http://') then
      sURL := Copy(sURL, 8, Length(sURL));
    iPos := Pos('/', sURL);
    if (iPos > 0) then
    begin
      strHost := Copy(sURL, 1, iPos - 1);
      strRemoteFileName := Copy(sURL, iPos, Length(sURL));
    end
    else
    begin
      strHost := sURL;
      strRemoteFileName := '';
    end;
    if strRemoteFileName[1] <> '/' then
      strRemoteFileName := '/' + strRemoteFileName;
  end;
begin
  SplitURL; // separate the given URL to host/file strings
  try
    with cliSock do
    begin
      Host := strHost;
      ClientType := ctBlocking; // wait for the result
      Port := 80;
      try
        Open;
        sCmd := 'GET ' + strRemoteFileName + '   HTTP/1.0'#13#10 + 'Host: ' + strHost + #13#10#13#10;
        intReturnCode := Socket.SendBuf(Pointer(sCmd)^, Length(sCmd)); // send query
        if intReturnCode > 0 then
        begin // receive the answer
          while (intReturnCode > 0) do // iterate until no more data
          begin
            FillChar(szBuffer, SizeOf(szBuffer), 0); // clear buffer before each iteration
            intReturnCode := Socket.ReceiveBuf(szBuffer, SizeOf(szBuffer)); // try to receive some data
            if intReturnCode > 0 then // if received a some data, then add this data to the result string
              sRet := sRet + szBuffer;
          end
        end
        else
        begin
          MessageDlg('No answer from server', mtError, [mbOk], 0); // QQ Msg
        end;
        Close;
      except
        MessageDlg('No connection', mtError, [mbOk], 0);  // QQ Msg
      end;
    end;
  finally
    ;
  end;

  Result := sRet;
end;
//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmSoftwareCheck.FormCreate(Sender: TObject);
begin
  // defaults
  ThisProgram := '???';
  ThisVersion := '???';
  ThisID      := '???';
  VersionFile := '???';

  btnGet.Visible  := False;
  btnCopy.Visible := False;
  btnNews.Visible := False;

//  fillCustomIni; // update the default language ini file
end;
procedure TfrmSoftwareCheck.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.FormActivate(Sender: TObject);
begin
  IsCancelled := False;
//  tmpnmHTTP := TNMHTTP.Create(Self);
//  tmpnmHTTP.OnAboutToSend          := nmHTTPAboutToSend;
//  tmpnmHTTP.OnFailure              := nmHTTPFailure;
//  tmpnmHTTP.OnAuthenticationNeeded := nmHTTPAuthenticationNeeded;
//  tmpnmHTTP.OnSuccess              := nmHTTPSuccess;

  memInfo.Text := S_CRLF + Format(ilMiscStr(rsToCheckNewProgram, 'rsToCheckNewProgram'), [ThisProgram]) + S_CRLF + S_CRLF +
                  ilMiscStr(rsWarning, 'rsWarning') + S_CRLF +
                  ilMiscStr(rsNeedsInternet, 'rsNeedsInternet') + S_CRLF + S_CRLF +
                  ilMiscStr(rsNoDownloadAccel, 'rsNoDownloadAccel');

  sttBar.Panels[0].Text := ilMiscStr(rsYourProgram, 'rsYourProgram') + ': ' +
                           ThisProgram + ' ' +
                           ilMiscStr(rsVersionAbbrev, 'rsVersionAbbrev') +
                           ThisVersion;
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  tmpnmHTTP.Free;
end;
//------------------------------------------------------------------------------
function TfrmSoftwareCheck.UpdateExists(sBff: String): Boolean;
var
  sMsg: String;
  sVsn, sDate: String;
begin
  UpdateExists := False;
  sVsn  := GetValueStartingWith(sBff, 'VSN=' , ';');
  sDate := GetValueStartingWith(sBff, 'DATE=', ';');
  sDownloadPath := GetValueStartingWith(sBff, 'PATH=', ';');
  sNewsPath     := GetValueStartingWith(sBff, 'NEWS=', ';');

  if (Length(sDownloadPath) > 0) and (CompareVersions(ThisVersion, sVsn) < 0) then
  begin
    UpdateExists := True;
    sMsg := ilMiscStr(rsUpdateExists, 'rsUpdateExists') + S_CRLF + S_CRLF;
    sMsg := sMsg + ilMiscStr(rsYourVersion, 'rsYourVersion') + ':' + S_CRLF;
    sMsg := sMsg + '  ' + ThisProgram + ' ' +
                   ilMiscStr(rsVersionAbbrev, 'rsVersionAbbrev') + ' ' +
                   ThisVersion + S_CRLF + S_CRLF;
    sMsg := sMsg + ilMiscStr(rsAvailableUpdate, 'rsAvailableUpdate') + ':' + S_CRLF;
    sMsg := sMsg + '  ' + ilMiscStr(rsVersion, 'rsVersion') + ': ' + S_TAB + sVsn  + S_CRLF;
    sMsg := sMsg + '  ' + ilMiscStr(rsDate, 'rsDate') + ': ' + S_TAB + sDate + S_CRLF;
    sMsg := sMsg + '  ' + ilMiscStr(rsURL, 'rsURL')  + ': ' + S_TAB + sDownloadPath + S_CRLF;

    memInfo.Text := sMsg;

    btnGet.Visible  := True;
    btnCopy.Visible := True;
    btnNews.Visible := Length(sNewsPath) > 0;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.btnCheckClick(Sender: TObject);
var
  sServer, sBff: String;
  i, iPos: Integer;
  fFound: Boolean;
  cliSock: TClientSocket;
  strList: TStringList;
begin
  btnCheck.Enabled := False;
  try
    cliSock := TClientSocket.Create(Self);
    strList := TStringList.Create;

    btnNews.Visible := False;
    btnGet.Visible := False;
    btnCopy.Visible := False;

    if InetUseProxy then // Use proxy
    begin
//      tmpnmHTTP.Proxy := InetProxyAddress;
//      tmpnmHTTP.Port := InetProxyPort;
    end
    else
    begin
//      tmpnmHTTP.Proxy := '';
    end;

    iPos := Pos('/', Copy(VersionFile, 8, Length(VersionFile))); // skip 'http://'
    if iPos > 0 then
      sServer := Copy(VersionFile, 1, iPos + 7)
    else
      sServer := '';

    memInfo.Text := S_CRLF + ilMiscStr(rsContactingServer, 'rsContactingServer') + ' ' + sServer + S_CRLF + S_CRLF + ilMiscStr(rsPleaseWait, 'rsPleaseWait');
    Application.ProcessMessages;
//    tmpnmHTTP.Get(VersionFile);
    strList.Text := DownloadFile(VersionFile, cliSock);
    fFound := False;
    for i := 0 to strList.Count - 1 do
    begin
      sBff := strList.Strings[i];
      if StrStartsWith(sBff, ThisID) then
        if UpdateExists(sBff) then
        begin
          fFound := True;
          Break;
        end;
    end;
    if not fFound then
      memInfo.Text := S_CRLF + ilMiscStr(rsNoUpdatesExist, 'rsNoUpdatesExist');

    btnNews.Visible := False;
    btnGet.Visible := False;
    btnCopy.Visible := False;

  except
    memInfo.Text := S_CRLF + ilMiscStr(rsError, 'rsError');
    if not IsCancelled then
      ShowMessage(ilMiscStr(rsErrorReadingUpdates, 'rsErrorreadingUpdates'));
  end;
  cliSock.Free;
  btnCheck.Enabled := True;
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.btnGetClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(sDownloadPath), '', '', sw_Normal);
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.btnNewsClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(sNewsPath), '', '', sw_Normal);
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.btnCopyClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(sDownloadPath));
end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.nmHTTPAboutToSend(Sender: TObject);
begin
//  tmpnmHTTP.SendHeader.Values['User-Agent'] := 'Mozilla/4.0';
end;
//------------------------------------------------------------------------------
//procedure TfrmSoftwareCheck.nmHTTPFailure(Cmd: CmdType);
//begin
//  if not IsCancelled then
//    memInfo.Text := S_CRLF + ilMiscStr(rsFailedReadingUpdatesList, 'rsFailedReadingUpdatesList');
//end;
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.nmHTTPAuthenticationNeeded(Sender: TObject);
var
  AnID, APass: String;
begin
  InputQuery(ilMiscStr(rsNeedsAuthent, 'rsNeedsAuthent'), ilMiscStr(rsEnterUser, 'rsEnterUser'), AnID);
  InputQuery(ilMiscStr(rsNeedsAuthent, 'rsNeedsAuthent'), ilMiscStr(rsEnterPass, 'rsEnterPass'), APass);
//  tmpnmHTTP.HeaderInfo.UserId := AnID;
//  tmpnmHTTP.HeaderInfo.Password := APass;
  ShowMessage(ilMiscStr(rsAuthEntered, 'rsAuthEntered'));
end;
//------------------------------------------------------------------------------
{
procedure TfrmSoftwareCheck.nmHTTPSuccess(Cmd: CmdType);
var
  strList: TStringList;
  sBff: String;
  i: Integer;
  fFound: Boolean;
begin
  strList := TStringList.Create;
  strList.Text := tmpnmHTTP.Body;
  fFound := False;
  for i := 0 to strList.Count - 1 do
  begin
    sBff := strList.Strings[i];
    if StrStartsWith(sBff, ThisID) then
      if UpdateExists(sBff) then
      begin
        fFound := True;
        Break;
      end;
  end;
  if not fFound then
    memInfo.Text := S_CRLF + ilMiscStr(rsNoUpdatesExist, 'rsNoUpdatesExist');
end;
}
//------------------------------------------------------------------------------
procedure TfrmSoftwareCheck.btnCancelClick(Sender: TObject);
begin
  IsCancelled := True;
//  tmpnmHTTP.Abort;
  Close;
end;
//------------------------------------------------------------------------------
// Configure Proxy access
procedure TfrmSoftwareCheck.btnProxyClick(Sender: TObject);
begin
  frmProxySettings.InetProxyAddress := InetProxyAddress;
  frmProxySettings.InetProxyPort    := InetProxyPort;
  frmProxySettings.InetUseProxy     := InetUseProxy;
  if frmProxySettings.ShowModal = mrOk then
  begin
    InetProxyAddress := frmProxySettings.InetProxyAddress;
    InetProxyPort    := frmProxySettings.InetProxyPort;
    InetUseProxy     := frmProxySettings.InetUseProxy;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

