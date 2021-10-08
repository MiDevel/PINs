{*****************************************************************************
 *
 *  u_ProxySettings.pas - Configure Proxy parameters
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

unit u_ProxySettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfrmProxySettings = class(TForm)
    chkUseProxy: TCheckBox;
    fldName: TEdit;
    fldPort: TEdit;
    lblName: TLabel;
    lblPort: TLabel;
    lblInfo: TLabel;
    spnPort: TUpDown;
    btnAccept: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private { Private declarations }
    m_InetUseProxy: Boolean;
    m_InetProxyAddress: String;
    m_InetProxyPort: Integer;

  published  { Published declarations }
    property InetUseProxy    : Boolean read m_InetUseProxy     write m_InetUseProxy;
    property InetProxyAddress: String  read m_InetProxyAddress write m_InetProxyAddress;
    property InetProxyPort   : Integer read m_InetProxyPort    write m_InetProxyPort;

  public { Public declarations }
  end;

var
  frmProxySettings: TfrmProxySettings;

implementation

uses
  IniLang, u_Main;

{$R *.DFM}

//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmProxySettings.FormCreate(Sender: TObject);
begin
  frmMain.ReadWindowPosition(self, False);
end;
procedure TfrmProxySettings.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmProxySettings.FormActivate(Sender: TObject);
begin
  fldName.Text        := InetProxyAddress;
  spnPort.Position    := InetProxyPort;
  chkUseProxy.Checked := InetUseProxy;
end;
//------------------------------------------------------------------------------
procedure TfrmProxySettings.btnAcceptClick(Sender: TObject);
begin
  InetProxyAddress := fldName.Text;
  InetProxyPort    := spnPort.Position;
  InetUseProxy     := chkUseProxy.Checked;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
