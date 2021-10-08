{*****************************************************************************
 *
 *  uMWForms.pas - Various forms utilities
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

unit uMWForms;

interface

uses
  Forms, SysUtils, Registry, IniFiles;

procedure ReadWindowPos(regFile: TRegIniFile; frm: TForm; nam: String; fRestoreSize: Boolean); overload;
procedure ReadWindowPos(iniFile: TMemIniFile; frm: TForm; nam: String; fRestoreSize: Boolean); overload;
procedure SaveWindowPos(regFile: TRegIniFile; frm: TForm; nam: String; fSaveSize: Boolean);  overload;
procedure SaveWindowPos(iniFile: TMemIniFile; frm: TForm; nam: String; fSaveSize: Boolean);  overload;

implementation

uses
  uMWStrings;

//------------------------------------------------------------------------------
// Restore the form placement stored as a string
procedure FormPosFromString(sPosStr: String; frm: TForm; fRestoreSize: Boolean);
var
  x, y, wdt, hgt: Integer;
  i: Integer;
  sTok: String;
begin
  i := 1;
  sTok := GetNextToken(sPosStr, i, ',');
  x := CvtStr2Int(sTok);
  if x > 0 then
  begin
    sTok := GetNextToken(sPosStr, i, ',');
    y := CvtStr2Int(sTok);
    if y >= 0 then
    begin
      // check if after eventual resolution change the window is not out of the screen
      if (x > Screen.Width - 20) then
        x := Screen.Width - 20;
      if (y > Screen.Height - 20) then
        y := Screen.Height - 20;
      frm.Left := x;
      frm.Top := y;
      // if the size should be restored as well
      if fRestoreSize then
      begin
        sTok := GetNextToken(sPosStr, i, ',');
        wdt := CvtStr2Int(sTok);
        if wdt > 0 then
        begin
          sTok := GetNextToken(sPosStr, i, ',');
          hgt := CvtStr2Int(sTok);
          if hgt > 0 then
          begin
            frm.Width := wdt;
            frm.Height := hgt;
          end;
        end;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
// Restore the window placement
procedure ReadWindowPos(regFile: TRegIniFile; frm: TForm; nam: String; fRestoreSize: Boolean);
begin
  FormPosFromString(regFile.ReadString('Dialogs', nam, '-1,-1'), frm, fRestoreSize);
end;
procedure ReadWindowPos(iniFile: TMemIniFile; frm: TForm; nam: String; fRestoreSize: Boolean);
begin
  FormPosFromString(iniFile.ReadString('Dialogs', nam, '-1,-1'), frm, fRestoreSize);
end;
//------------------------------------------------------------------------------
// Store the window placement
procedure SaveWindowPos(regFile: TRegIniFile; frm: TForm; nam: String; fSaveSize: Boolean);
var
  sBff: String;
begin
  if frm.WindowState = wsNormal then
  begin
    if fSaveSize then
      sBff := IntToStr(frm.Left) + ',' + IntToStr(frm.Top) +
              ',' + IntToStr(frm.Width) + ',' + IntToStr(frm.Height)
    else
      sBff := IntToStr(frm.Left) + ',' + IntToStr(frm.Top);
    regFile.WriteString ('Dialogs', nam, sBff);
  end;
end;
procedure SaveWindowPos(iniFile: TMemIniFile; frm: TForm; nam: String; fSaveSize: Boolean);
var
  sBff: String;
begin
  if frm.WindowState = wsNormal then
  begin
    if fSaveSize then
      sBff := IntToStr(frm.Left) + ',' + IntToStr(frm.Top) +
              ',' + IntToStr(frm.Width) + ',' + IntToStr(frm.Height)
    else
      sBff := IntToStr(frm.Left) + ',' + IntToStr(frm.Top);
    iniFile.WriteString ('Dialogs', nam, sBff);
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
