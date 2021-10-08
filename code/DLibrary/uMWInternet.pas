{*****************************************************************************
 *
 *  uMWInternet.pas - Internet-related tools, useful in many projects
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

unit uMWInternet;

interface

uses
  Windows, SysUtils, ScktComp;

  procedure LaunchHyperlink(hdl: THandle; sURL: String);
  function  DownloadAsString(sHost, sRemoteFileName: String; csSocket: TClientSocket; var Success: Boolean): String;
  procedure LaunchWWWPage(hdl: THandle; sURL: String);


implementation

uses
  ShellApi, Dialogs, uMWStrings;

//------------------------------------------------------------------------------
// Launch a hyperlink.
//   hdl - calling form handle
//   sURL - URL string, e.g., 'http://www.msn.com'
procedure LaunchHyperlink(hdl: THandle; sURL: String);
begin
  if Pos('ftp://', LowerCase(sURL)) > 0 then // FTP
  begin
    ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);
    Exit;
  end;

  if Pos('file://', LowerCase(sURL)) > 0 then // file
  begin
    ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);
    Exit;
  end;

  if Pos('http://', LowerCase(sURL)) > 0 then // HTTP
  begin
    ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);
    Exit;
  end;

  if Pos('https://', LowerCase(sURL)) > 0 then // HTTPS
  begin
    ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);
    Exit;
  end;

  if Pos('@', sURL) > 0 then // e-mail
  begin
    ShellExecute(hdl, 'open', PChar('mailto:' + sURL),'','',sw_Normal);
    Exit;
  end;

  if StrStartsWith(LowerCase(sURL), 'www') then // HTTP
  begin
    ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);
    Exit;
  end;

  // something unrecognized. Let's hope system knows what to do with it...
  ShellExecute(hdl, 'open', PChar(sURL),'','',sw_Normal);

//  CstShowMessage('Don''t know how to launch hyperlink' + S_CRLF + sURL);
end;
//------------------------------------------------------------------------------
// Retrieve a file as a string
// Example:
//   sBff := DownloadAsString('http://www.domain.com', '/index.html', cs);
function DownloadAsString(sHost, sRemoteFileName: String; csSocket: TClientSocket; var Success: Boolean): String;
var
  intReturnCode: Integer;
  s, sRet: string;
  szBuffer: array[0..128] of Char;
begin
  Success := True;
  sRet := '';
  Result := sRet;
  if sRemoteFileName[1] <> '/' then
    sRemoteFileName := '/' + sRemoteFileName;

  try
    with csSocket do
    begin
      Host := sHost;
      ClientType := ctBlocking;
      Port := 80;
      try
        Open;
        s := 'GET ' + sRemoteFileName + '   HTTP/1.0'#13#10 +
             'Host: ' + sHost + #13#10#13#10;
        intReturnCode := Socket.SendBuf(Pointer(s)^, Length(s)); // send query

        if intReturnCode > 0 then
        begin
          while (intReturnCode > 0) do // iterate until no more data
          begin
            FillChar(szBuffer, SizeOf(szBuffer), 0); // clear buffer before each iteration
            intReturnCode := Socket.ReceiveBuf(szBuffer, SizeOf(szBuffer)); // try to receive some data
            if intReturnCode > 0 then // append this data to the result string
              sRet := sRet + szBuffer;
          end
        end
        else
        begin
          MessageDlg('No answer from server', mtError, [mbOk], 0);
          Success := False;
        end;
        Close;

      except
        MessageDlg('No connection', mtError, [mbOk], 0);
        Success := False;
      end;
    end;
  finally
    ;
  end;
  Result := sRet;
end;
//------------------------------------------------------------------------------
// Launch the specified URL in the default browser
procedure LaunchWWWPage(hdl: THandle; sURL: String);
begin
  ShellExecute(hdl, 'open', PChar(sURL), '', '', sw_Normal);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
