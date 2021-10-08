{*****************************************************************************
 *  PINs
 *  u_HotKeys.pas - hot keys configuration dialog
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

unit u_HotKeys;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls;

resourcestring
  rsHotKeyRegError='Not all hotkeys could be registered - check their settings in Tools/Hotkeys!';

const
  HKEY_OFS = 150;

  DFTKEY_SPASTE = 'act=1,ctr=1,alt=0,shf=0,key=Y';
  DFTKEY_UPASTE = 'act=0,ctr=1,alt=0,shf=1,key=F11';
  DFTKEY_PPASTE = 'act=0,ctr=1,alt=0,shf=1,key=F12';
  DFTKEY_CPASTE = 'act=0,ctr=1,alt=0,shf=1,key=F10';

type
  TOneHotkey = class
    m_ctrl, m_alt, m_shift: Boolean;
    m_active, m_success: Boolean;
    m_key: String;
    m_id: Integer; // key will be registered with this id
    function  GetAsString: String;
    procedure SetFromString(sStr: String);
    function  RegisterIt(id: Integer; opt: TMenuItem): Boolean;
    procedure UnRegisterIt;
  end;

  TfrmHotkeys = class(TForm)
    lblSPaste: TLabel;
    lblUPaste: TLabel;
    chkSPaste_Ctrl: TCheckBox;
    lblAlt: TLabel;
    lblShift: TLabel;
    lblCtrl: TLabel;
    lblKey: TLabel;
    chkSPaste_Alt: TCheckBox;
    chkSPaste_Shift: TCheckBox;
    outcmbSPaste_Key: TComboBox;
    lblActive: TLabel;
    chkSPaste_Active: TCheckBox;
    chkUPaste_Ctrl: TCheckBox;
    chkUPaste_Alt: TCheckBox;
    chkUPaste_Shift: TCheckBox;
    outcmbUPaste_Key: TComboBox;
    chkUPaste_Active: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnDefault: TButton;
    btnHelp: TButton;
    lblPPaste: TLabel;
    chkPPaste_Ctrl: TCheckBox;
    chkPPaste_Alt: TCheckBox;
    chkPPaste_Shift: TCheckBox;
    outcmbPPaste_Key: TComboBox;
    chkPPaste_Active: TCheckBox;
    lblCPaste: TLabel;
    chkCPaste_Ctrl: TCheckBox;
    chkCPaste_Alt: TCheckBox;
    chkCPaste_Shift: TCheckBox;
    outcmbCPaste_Key: TComboBox;
    chkCPaste_Active: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private { Private declarations }

  public { Public declarations }

  end;

var
  frmHotkeys: TfrmHotkeys;
  HKSPaste, HKUPaste, HKPPaste, HKCPaste: TOneHotkey;

  function  RegisterHotkeys(fWarn: Boolean): Boolean;
  procedure UnRegisterHotkeys;

implementation

uses
  u_Main, uMWTools, uMWStrings, IniLang;

{$R *.DFM}


//------------------------------------------------------------------------------
// Register hotkeys
function RegisterHotkeys(fWarn: Boolean): Boolean;
var
  ok: Boolean;
  okC1, okC2, okC3, okC4: Boolean;
begin
  okC1 := HKSPaste.RegisterIt(HKEY_OFS + 1, nil);
  okC2 := HKUPaste.RegisterIt(HKEY_OFS + 2, nil);
  okC3 := HKPPaste.RegisterIt(HKEY_OFS + 3, nil);
  okC4 := HKCPaste.RegisterIt(HKEY_OFS + 4, nil);

  ok := okC1 and okC2 and okC3 and okC4;
  if not (ok) then
    if (fWarn) then
      frmMain.CstShowMessage(ilMiscStr(rsHotKeyRegError, 'rsHotKeyRegError'));
  RegisterHotkeys := ok;
end;
//------------------------------------------------------------------------------
// Free hotkeys
procedure UnRegisterHotkeys;
begin
  HKSPaste.UnRegisterIt;
  HKUPaste.UnRegisterIt;
  HKPPaste.UnRegisterIt;
  HKCPaste.UnRegisterIt;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// TOneHotkey class handling

//------------------------------------------------------------------------------
// Get the hotkey parameters as a string
// Example: act=1,ctr=1,alt=0,shf=1,key=P
function TOneHotkey.GetAsString: String;
var
  sBff: String;
begin
  sBff :=        'act=' + IntToStr(BoolToInt(m_active)) + ',';
  sBff := sBff + 'ctr=' + IntToStr(BoolToInt(m_ctrl))   + ',';
  sBff := sBff + 'alt=' + IntToStr(BoolToInt(m_alt))    + ',';
  sBff := sBff + 'shf=' + IntToStr(BoolToInt(m_shift))  + ',';
  sBff := sBff + 'key=' + m_key;
  GetAsString := sBff;
end;
//------------------------------------------------------------------------------
// Extract the hotkey parameters from a string
procedure TOneHotkey.SetFromString(sStr: String);
begin
  m_active := GetValueStartingWith(sStr, 'act=', ',') <> '0';
  m_ctrl   := GetValueStartingWith(sStr, 'ctr=', ',') <> '0';
  m_alt    := GetValueStartingWith(sStr, 'alt=', ',') <> '0';
  m_shift  := GetValueStartingWith(sStr, 'shf=', ',') <> '0';
  m_key    := GetValueStartingWith(sStr, 'key=', ',');
end;
//------------------------------------------------------------------------------
// Register the hotkey using the passed id
function TOneHotkey.RegisterIt(id: Integer; opt: TMenuItem): Boolean;
var
  flags: Integer;     // used for shortcut registering
  state: TShiftState; // used for the menu option shortcut
  vkey:  Integer;     // virtual key code
begin
  m_success := True; // success
  flags := 0;
  state := [];
  vkey  := 0;
  m_id  := id;
  if m_active then
  begin
    if Length(m_key) > 0 then
    begin
      if Length(m_key) = 1 then
        vkey := Ord(m_key[1])
      else
      begin
             if m_key = 'F1'  then vkey := VK_F1
        else if m_key = 'F2'  then vkey := VK_F2
        else if m_key = 'F3'  then vkey := VK_F3
        else if m_key = 'F4'  then vkey := VK_F4
        else if m_key = 'F5'  then vkey := VK_F5
        else if m_key = 'F6'  then vkey := VK_F6
        else if m_key = 'F7'  then vkey := VK_F7
        else if m_key = 'F8'  then vkey := VK_F8
        else if m_key = 'F9'  then vkey := VK_F9
        else if m_key = 'F10' then vkey := VK_F10
        else if m_key = 'F11' then vkey := VK_F11
        else if m_key = 'F12' then vkey := VK_F12
//        else if m_key = 'Print' then vkey := VK_PRINT;
      end;
    end;
    if m_ctrl  then begin Inc(flags, MOD_CONTROL); state := state + [ssCtrl]; end;
    if m_alt   then begin Inc(flags, MOD_ALT); state := state + [ssAlt]; end;
    if m_shift then begin Inc(flags, MOD_SHIFT); state := state + [ssShift]; end;
    m_success := RegisterHotKey(frmMain.Handle, id, flags, vkey);

    if (opt <> nil) then
    begin
      if m_success then
        opt.ShortCut := ShortCut(vkey, state)
      else
        opt.ShortCut := 0;
    end;
  end
  else
  begin
    if (opt <> nil) then
      opt.ShortCut := 0;
  end;

  RegisterIt := m_success;
end;
//------------------------------------------------------------------------------
// Unregister the hotkey using the passed id
procedure TOneHotkey.UnRegisterIt;
begin
  UnRegisterHotKey(frmMain.Handle, m_id);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Form handling
//
//------------------------------------------------------------------------------
// First-time initialization
procedure TfrmHotkeys.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  outcmbSPaste_Key.Clear;
  outcmbUPaste_Key.Clear;
  outcmbPPaste_Key.Clear;

  for i := Ord('A') to Ord('Z') do
  begin
    outcmbSPaste_Key.Items.Add(Chr(i));
    outcmbUPaste_Key.Items.Add(Chr(i));
    outcmbPPaste_Key.Items.Add(Chr(i));
  end;
  for i := Ord('0') to Ord('9') do
  begin
    outcmbSPaste_Key.Items.Add(Chr(i));
    outcmbUPaste_Key.Items.Add(Chr(i));
    outcmbPPaste_Key.Items.Add(Chr(i));
  end;
  for i := 1 to 12 do
  begin
    outcmbSPaste_Key.Items.Add('F' + IntToStr(i));
    outcmbUPaste_Key.Items.Add('F' + IntToStr(i));
    outcmbPPaste_Key.Items.Add('F' + IntToStr(i));
  end;

  frmMain.ReadWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmHotkeys.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmHotkeys.FormActivate(Sender: TObject);
begin
  with HKSPaste do
  begin
    chkSPaste_Ctrl.Checked   := m_ctrl;
    chkSPaste_Alt.Checked    := m_alt;
    chkSPaste_Shift.Checked  := m_shift;
    outcmbSPaste_Key.Text    := m_key;
    chkSPaste_Active.Checked := m_active;
    if m_success then
      lblSPaste.Font.Color := clBlack
    else
      lblSPaste.Font.Color := clRed;
  end;

  with HKUPaste do
  begin
    chkUPaste_Ctrl.Checked   := m_ctrl;
    chkUPaste_Alt.Checked    := m_alt;
    chkUPaste_Shift.Checked  := m_shift;
    outcmbUPaste_Key.Text    := m_key;
    chkUPaste_Active.Checked := m_active;
    if m_success then
      lblUPaste.Font.Color := clBlack
    else
      lblUPaste.Font.Color := clRed;
  end;

  with HKPPaste do
  begin
    chkPPaste_Ctrl.Checked   := m_ctrl;
    chkPPaste_Alt.Checked    := m_alt;
    chkPPaste_Shift.Checked  := m_shift;
    outcmbPPaste_Key.Text    := m_key;
    chkPPaste_Active.Checked := m_active;
    if m_success then
      lblPPaste.Font.Color := clBlack
    else
      lblPPaste.Font.Color := clRed;
  end;


  with HKCPaste do
  begin
    chkCPaste_Ctrl.Checked   := m_ctrl;
    chkCPaste_Alt.Checked    := m_alt;
    chkCPaste_Shift.Checked  := m_shift;
    outcmbCPaste_Key.Text    := m_key;
    chkCPaste_Active.Checked := m_active;
    if m_success then
      lblCPaste.Font.Color := clBlack
    else
      lblCPaste.Font.Color := clRed;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmHotkeys.btnOkClick(Sender: TObject);
begin
  btnApplyClick(Nil);
end;
//------------------------------------------------------------------------------
procedure TfrmHotkeys.btnDefaultClick(Sender: TObject);
begin
  HKSPaste.SetFromString(DFTKEY_SPASTE);
  HKUPaste.SetFromString(DFTKEY_UPASTE);
  HKPPaste.SetFromString(DFTKEY_PPASTE);
  HKCPaste.SetFromString(DFTKEY_CPASTE);

  FormActivate(Nil);
end;
//------------------------------------------------------------------------------
procedure TfrmHotkeys.btnApplyClick(Sender: TObject);
begin
  with HKSPaste do
  begin
    m_ctrl   := chkSPaste_Ctrl.Checked;
    m_alt    := chkSPaste_Alt.Checked;
    m_shift  := chkSPaste_Shift.Checked;
    m_key    := outcmbSPaste_Key.Text;
    m_active := chkSPaste_Active.Checked;
  end;

  with HKUPaste do
  begin
    m_ctrl   := chkUPaste_Ctrl.Checked;
    m_alt    := chkUPaste_Alt.Checked;
    m_shift  := chkUPaste_Shift.Checked;
    m_key    := outcmbUPaste_Key.Text;
    m_active := chkUPaste_Active.Checked;
  end;

  with HKPPaste do
  begin
    m_ctrl   := chkPPaste_Ctrl.Checked;
    m_alt    := chkPPaste_Alt.Checked;
    m_shift  := chkPPaste_Shift.Checked;
    m_key    := outcmbPPaste_Key.Text;
    m_active := chkPPaste_Active.Checked;
  end;

  with HKCPaste do
  begin
    m_ctrl   := chkCPaste_Ctrl.Checked;
    m_alt    := chkCPaste_Alt.Checked;
    m_shift  := chkCPaste_Shift.Checked;
    m_key    := outcmbCPaste_Key.Text;
    m_active := chkCPaste_Active.Checked;
  end;

  UnRegisterHotkeys;
  RegisterHotkeys(True);
  FormActivate(Nil);
end;
//------------------------------------------------------------------------------
// Show help on printing
procedure TfrmHotkeys.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_hotkeys.htm', Handle);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

begin
  // Create hotkeys variables
  HKSPaste := TOneHotkey.Create;
  HKUPaste := TOneHotkey.Create;
  HKPPaste := TOneHotkey.Create;
  HKCPaste := TOneHotkey.Create;
end.
