{*****************************************************************************
 *  PINs
 *  u_Item.pas - TOneItem class, items editor
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

unit u_Columns;

interface

uses
  SysUtils;

const
  COL_DMY = 0;
  COL_IDXFIRST = 1;
  COL_CAT =  1;
  COL_SYS =  2;
  COL_USR =  3;
  COL_PSW =  4;
  COL_CMM =  5;
  COL_CST =  6;
  COL_STA =  7;
  COL_EXP =  8;
  COL_INF =  9;
  COL_SPS = 10;
  COL_IDXLAST = 10;

type
  TOneColumn = class
  private { Private declarations }
    m_DftName: String;
    m_UserName: String;
    m_Idf: String; // 3-character long identifier: CAT, SYS, ...
    m_GridIndex: Integer;
    m_ToExport: Boolean; // should be exported?
    function GetCaption: String;

  published  { Published declarations }
    property DftName  : String  read m_DftName   write m_DftName;
    property UserName : String  read m_UserName  write m_UserName;
    property Caption  : String  read GetCaption;
    property Idf      : String  read m_Idf       write m_Idf;
    property GridIndex: Integer read m_GridIndex write m_GridIndex;
    property ToExport : Boolean read m_ToExport  write m_ToExport;

  public { Public declarations }
  end;

var
  GlbColumns: array[COL_IDXFIRST..COL_IDXLAST] of TOneColumn;

implementation

uses
  u_Main;

//------------------------------------------------------------------------------
// Return the display name
function TOneColumn.GetCaption: String;
begin
  if frmMain.UserColNames then // user names allowed?
  begin
    if Length(Trim(m_UserName)) > 0 then
      Result := Trim(m_UserName)
    else // user name not specified
      Result := Trim(m_DftName)
  end
  else
    Result := Trim(m_DftName);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
