{*****************************************************************************
 *
 *  uMWErrorLog.pas - Safe logging to a disk file
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

unit uMWErrorLog;

interface

uses
  Windows, SysUtils;

type
  TMWErrorLog = class
  private { Private declarations }
    m_sFileName: String;
    m_vFile: System.TextFile;
    m_fActive: Boolean;
    m_fTimeStamp: Boolean; // When the time stamp is on, the time is inserted at the begining of the each line

  published { Published declarations }
    property TimeStamp: Boolean read m_fTimeStamp write m_fTimeStamp;

  public { Public declarations }
    constructor Create;
    procedure Init(sFilNam: String; fCreateNew: Boolean = True);
    procedure WriteString(sText: String; fLF: Boolean = True);
    procedure WriteBoolean(sText: String; fOn: Boolean; fLF: Boolean = True);
    procedure WriteInteger(sText: String; nVal: Integer; fLF: Boolean = True);
    procedure WritePoint(sText: String; pnt: TPoint; fLF: Boolean = True);
    procedure Activate(fOn: Boolean);
    Function  IsActive(): Boolean;
    Function  SetTimeStamp(fFlg: Boolean): Boolean;
  end;

implementation


//------------------------------------------------------------------------------
constructor TMWErrorLog.Create;
begin
  m_sFileName := '';
end;
//------------------------------------------------------------------------------
// Initialize logging.
procedure TMWErrorLog.Init(sFilNam: String; fCreateNew: Boolean = True);
begin
  m_sFileName := sFilNam;
  System.AssignFile(m_vFile, m_sFileName);
  if fCreateNew or (not FileExists(sFilNam)) then
    Rewrite(m_vFile) // create a new file
  else
    Reset(m_vFile); // open the file if exists
  CloseFile(m_vFile); // Done for now
  m_fActive := False;
  m_fTimeStamp := False;
end;
//------------------------------------------------------------------------------
// Add a text line.
//   fLF - output CRLF at the end.
procedure TMWErrorLog.WriteString(sText: String; fLF: Boolean);
var
  timCrr :  TDateTime;
  nHrs   :  Word;
  nMin   :  Word;
  nSec   :  Word;
  nMls   :  Word;
begin
  if (m_fActive) then
  begin
    System.AssignFile(m_vFile, m_sFileName);
    Reset(m_vFile); // open the original file
    Append(m_vFile);
    if (m_fTimeStamp) then // append time to the error log
    begin
      timCrr := Time();
      DecodeTime(timCrr, nHrs, nMin, nSec, nMls);
      sText := IntToStr(nHrs) + ':' + IntToStr(nMin) + ':' + IntToStr(nSec) + '.' + IntToStr(nMls) + '; ' +  sText;
    end;
    Write(m_vFile, sText);
    if fLF then
      WriteLn(m_vFile, '');
    CloseFile(m_vFile); // Done for now
  end;
end;
//------------------------------------------------------------------------------
// Add a line containing a BOOL value, preceeded with a text.
//   fLF - output CRLF at the end.
procedure TMWErrorLog.WriteBoolean(sText: String; fOn: Boolean; fLF: Boolean);
begin
  if (m_fActive) then
  begin
    System.AssignFile(m_vFile, m_sFileName);
    Reset(m_vFile); // open the original file
    Append(m_vFile);
    if fOn then
      Write(m_vFile, sText + 'True')
    else
      Write(m_vFile, sText + 'False');

    if fLF then
      WriteLn(m_vFile, '');
    CloseFile(m_vFile); // Done for now
  end;
end;
//------------------------------------------------------------------------------
// Add a line containing an int value, preceeded with a text.
//   fLF - output CRLF at the end.
procedure TMWErrorLog.WriteInteger(sText: String; nVal: Integer; fLF: Boolean);
begin
  if (m_fActive) then
  begin
    System.AssignFile(m_vFile, m_sFileName);
    Reset(m_vFile); // open the original file
    Append(m_vFile);
    Write(m_vFile, sText + IntToStr(nVal));

    if fLF then
      WriteLn(m_vFile, '');
    CloseFile(m_vFile); // Done for now
  end;
end;
//------------------------------------------------------------------------------
// Add a line containing two point coordinates, preceeded with a text.
//   fLF - output CRLF at the end.
procedure TMWErrorLog.WritePoint(sText: String; pnt: TPoint; fLF: Boolean = True);
begin
  if (m_fActive) then
  begin
    System.AssignFile(m_vFile, m_sFileName);
    Reset(m_vFile); // open the original file
    Append(m_vFile);
    Write(m_vFile, sText + '(' + IntToStr(pnt.x) + ',' + IntToStr(pnt.y) + ')');

    if fLF then
      WriteLn(m_vFile, '');
    CloseFile(m_vFile); // Done for now
  end;
end;
//------------------------------------------------------------------------------
// Allows to activate and deactivate storing informations to the log-file
procedure TMWErrorLog.Activate(fOn : Boolean);
begin
  m_fActive := fOn;
end;
//------------------------------------------------------------------------------
// Returns TRUE if storing informations to the log-file is active
Function TMWErrorLog.IsActive(): Boolean;
begin
  Result := m_fActive;
end;
//------------------------------------------------------------------------------
// Sets the time stamp status. When the time stamp is on,
// the time is inserted at the begining of the each line.
Function TMWErrorLog.SetTimeStamp(fFlg: Boolean): Boolean;
begin
  Result := m_fTimeStamp;
  m_fTimeStamp := fFlg;
end;
//------------------------------------------------------------------------------


end.
