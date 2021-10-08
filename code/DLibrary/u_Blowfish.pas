{*****************************************************************************
 *  PINs
 *  u_Blowfish.pas - Blowfish encoding/decoding interface
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

unit u_Blowfish;

interface

function EncodeBlowfish(sTxt, sPsw: String; iVsn: Integer): String;
function DecodeBlowfish(sTxt, sPsw: String; iVsn: Integer): String;

implementation

uses
  Sysutils, Blowfish;

var
  Cipher: TDCP_blowfish;
  Key: array[0..55] of byte; // key can be any size upto 448bits with blowfish (56bytes)

//------------------------------------------------------------------------------
// Take the user password, fill the Key buffer to its full length
procedure SetKey(sKey: String; iVsn: Integer);
var
  i: Integer;
begin
  case iVsn of
    0..300:
        begin
          sKey := '&' + sKey + '~';
          for i := 1 to Sizeof(Key) do
          begin
            if i <= Length(sKey) then
              Key[i-1] := Ord(sKey[i])
            else
              Key[i-1] := i + 65; // dummy fill
          end;
        end;
    else
        begin
          if Length(sKey) = 0 then
            sKey := 'A';
          for i := 0 to Sizeof(Key) - 1 do
            Key[i] := Ord(sKey[(i mod Length(sKey)) + 1]);
        end;
  end;
end;
//------------------------------------------------------------------------------
function EncodeBlowfish(sTxt, sPsw: String; iVsn: Integer): String;
var
  sRet: String;
begin
  sRet := sTxt;
  if Length(sTxt) > 0 then
  begin
    Cipher := TDCP_blowfish.Create(nil);
    SetKey(sPsw, iVsn);
    Cipher.Init(Key, Sizeof(Key)*8, nil);  // remember key size is in BITS
    Cipher.EncryptCBC(sRet[1], sRet[1], Length(sRet)); // when encrypting strings you must point the algorithm to the first character
    Cipher.Burn;
    Cipher.Free;
  end;
  Result := sRet;
end;
//------------------------------------------------------------------------------
function DecodeBlowfish(sTxt, sPsw: String; iVsn: Integer): String;
var
  sRet: String;
begin
  sRet := sTxt;
  if Length(sTxt) > 0 then
  begin
    Cipher := TDCP_blowfish.Create(nil);
    SetKey(sPsw, iVsn);
    Cipher.Init(Key, Sizeof(Key)*8, nil);  // remember key size is in BITS
    Cipher.DecryptCBC(sRet[1], sRet[1], Length(sRet));
    Cipher.Burn;
    Cipher.Free;
  end;
  Result := sRet;
end;
//------------------------------------------------------------------------------

end.
