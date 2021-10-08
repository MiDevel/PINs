{*****************************************************************************
 *  PINs
 *  u_Sounds.pas - sounds handling
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

unit u_Sounds;

interface

type
  TSounds = (sndNONE, sndTEST, sndERROR, sndOPEN, sndSAVE, sndCLIPCOPYUSR,
             sndCLIPCOPYPSW, sndPASTE);

const
  FirstSound = sndERROR;
  LastSound  = sndPASTE;

type  
  TOneSound = class
  private { Private declarations }
    m_Active: Boolean;      // is this particilar sound on
    m_Sound: String;        // internal sound (from resources)
    m_UseInternal: Boolean; // use wave from resources
    m_Path: String;         // path to external wave file

  public { Public declarations }
    constructor Create;
    function  GetAsString: String;
    procedure SetFromString(sStr: String);

  published { Published declarations }
    property Active: Boolean read m_Active write m_Active;
    property Sound: String read m_Sound write m_Sound;
    property UseInternal: Boolean read m_UseInternal write m_UseInternal;
    property Path: String read m_Path write m_Path;
  end;

  TSoundsAry = array[sndNONE..LastSound] of TOneSound;

  procedure InitializeSounds;
  procedure PlayOneSound(iSnd: TSounds);

var
  SoundsAry: TSoundsAry; // the global array of available sound events
  TmpSoundsAry: TSoundsAry; // used for counds configuration

implementation

uses
  Windows, mmSystem, SysUtils,
  u_Main, uMWTools, uMWStrings;

//------------------------------------------------------------------------------
constructor TOneSound.Create;
begin
  m_Active      := True; // is this particilar sound on
  m_UseInternal := True; // use wave from resources
  m_Path        := '';   // path to external wave file
  m_Sound       := '';   // internal sound
end;
//------------------------------------------------------------------------------
// Get the sound parameters as a string
// Example: act=1,builtin=1,path=c:\wav\test.wav
function  TOneSound.GetAsString: String;
var
  sBff: String;
begin
  sBff :=        'act='     + IntToStr(BoolToInt(Active))      + ',';
  sBff := sBff + 'builtin=' + IntToStr(BoolToInt(UseInternal)) + ',';
  sBff := sBff + 'path='    + Path;
  Result := sBff;
end;
//------------------------------------------------------------------------------
// Extract the sound parameters from a string
procedure TOneSound.SetFromString(sStr: String);
begin
  Active      := GetValueStartingWith(sStr, 'act='    , ',') <> '0';
  UseInternal := GetValueStartingWith(sStr, 'builtin=', ',') <> '0';
  Path        := GetValueStartingWith(sStr, 'path='   , ',');
end;
//------------------------------------------------------------------------------
// Initialize the sounds system
procedure InitializeSounds;
var
  iSnd: TSounds;
begin
  for iSnd := sndNONE to LastSound do
  begin
    SoundsAry[iSnd] := TOneSound.Create;
    TmpSoundsAry[iSnd] := TOneSound.Create;
  end;

  SoundsAry[sndNONE].Sound  := '';
  SoundsAry[sndTEST].Sound  := '';
  SoundsAry[sndERROR].Sound := 'SND_ERROR';
  SoundsAry[sndOPEN].Sound  := 'SND_OPEN';
  SoundsAry[sndSAVE].Sound  := 'SND_SAVE';
  SoundsAry[sndCLIPCOPYUSR].Sound := 'SND_CLIPCOPYUSR';
  SoundsAry[sndCLIPCOPYPSW].Sound := 'SND_CLIPCOPYPSW';
  SoundsAry[sndPASTE].Sound := 'SND_PASTE';

  SoundsAry[sndOPEN].Active := False;
end;
//------------------------------------------------------------------------------
// Plays a sound (.wav) from the program resources
procedure PlayResSound(sName: PChar);
var
  h: THandle;
  p: Pointer;
begin
  h := FindResource(hInstance, sName, 'WAV');
  h := LoadResource(hInstance, h);
  p := LockResource(h);
  sndPlaySound(p, SND_MEMORY or SND_ASYNC);
  UnLockResource(h);
  FreeResource(h);
end;
//------------------------------------------------------------------------------
// Interface function allowing to play one of sounds
procedure PlayOneSound(iSnd: TSounds);
begin
  if frmMain.SoundOn and (SoundsAry[iSnd] <> nil)then
  begin
    if SoundsAry[iSnd].m_Active then
    begin
      if SoundsAry[iSnd].m_UseInternal then
      begin
        PlayResSound(PChar(SoundsAry[iSnd].Sound));
      end
      else
      begin
        if FileExists(SoundsAry[iSnd].m_Path) then
          PlaySound(PChar(SoundsAry[iSnd].Path), 0, SND_FILENAME);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
