{*****************************************************************************
 *
 *  uMWStrings.pas - Various String utilities
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

unit uMWStrings;

interface

uses
  Graphics, Windows, Classes

{$IFDEF VER140}  // D6
  ,Variants
{$ENDIF}

{$IFDEF VER150}  // D7
  ,Variants
{$ENDIF}
  ;

const
  S_CRLF = Chr(13) + Chr(10);             // newline
  S_CRLFLF = Chr(13) + Chr(10) + Chr(10); // 2 x newline
  S_TAB  = Chr(9);                        // tab

  function  StrStartsWith(sStr, sSubstr: String): Boolean;
  function  StrEndsWith(sStr, sSubstr: String): Boolean;
  function  GetNextToken(sStr: String; var iPos: Integer; sXcl: String): String;
//  function  GetTokenStartingWith(sStr, sPtt, sXcl: String; var iPos: Integer): String;
  function  GetTokenStartingWith(sStr, sPtt, sXcl: String; var iPos: Integer; bIgnoreCase: Boolean = True): String;
  function  GetValueStartingWith(sStr, sPtt, sXcl: String; sDft: String = ''): String;
  function  GetTokenStartingWithOneOf(sStr, sSet, sXcl: String; var iPos: Integer): String;
  function  GetTokenValue(sStr, sSep: String): String;
  function  CvtStr2Int(sBff: String): Integer;
  function  CvtStr2Dbl(sBff: String): Double;
  function  CvtDbl2Str(dVal: Double): String;
  function  CvtBol2Str(fVal: Boolean): String;
  function  CvtBinStr2HexStr(sBin: String): String;
  function  CvtHexStr2BinStr(sHex: String): String;
  function  CvtStr2HexStr(sStr: String): String;
  function  CvtHexStr2Str(sStr: String): String;
  function  CvtInt2BinStr(iVal: Int64): String;
  function  CvtStr2IntStr(sBff: String): String;
  function  CvtStr2DblStr(sBff, sSep: String): String;
  function  ReplStr(const sOld, sNew, sStr: String): String;
  function  XORString(sStr: String; iXor: Integer): String;
  function  LeftStr(sStr: String; iCnt: Integer): String;
  function  RightStr(sStr: String; iCnt: Integer): String;
  function  ShorterStr(sStr: String; iCnt: Integer): String;
  function  LPad(sStr: String; num: Integer; chPad: Char): String;
  function  RPad(sStr: String; num: Integer; chPad: Char): String;
  function  DelLedChr(const sStr: String; cChar: Char): String;
  function  DelTrlChr(const sStr: String; cChar: Char): String;
  function  ExtractFileNam(const sName: String): String;
  function  SwapFileExt(const sName: String; const sNewExt: String): String;
  function  MakePath(sPath, sFile: String): String;
  function  ValidPath(const sPath: String): String;
  function  ValidFileName(const sName: String): String;
  function  CompareMenuItems(txt1, txt2: String): Boolean;
  function  FontToString(font: TFont): String;
  Procedure StringToFont(sFntStr: String; var font: TFont);
  function  GetHTMLRGBString(clo: TColor): String;
  function  Nzs(sTxt: Variant): String;
  function  Nzi(vNum: Variant): Integer;
  function  DDMMYYYYToDate(vDate: String): TDateTime;
  function  YYYYMMDDToDate(vDate: String): TDateTime;
  function  ReadTextFileToStrings(sFilNam: String; var strLis: TStringList): Boolean;
  function  CutStringAtChar(sStr: String; sChr: Char): String;
  function  NoLastBackslash(sBff: String): String;
  procedure SplitSizeString(sText: String; var dx: Integer; var dy: Integer);
  function  CntOfChr(sChr: String; sStr: String): Integer;
  function  ExtractURLs(sString: String;  var strLis: TStringList): Boolean;
  function  GetCRC(str: String): String; overload;
  function  GetCRC(strLis: TStringList; iSta: Integer): String; overload;
  function  PosOfs(substr: String; str: String; ofs: Integer): Integer;
  function  IsStrValidDate(str: string): Boolean;
  function  CvtStr2Date(const sDate: String): TDateTime;
  function  CompareVersions(sThisVsn, sBaseVsn: String): Int64;
  function  DequotedStr(sStr: String): String;
  function  CRLFTo(sStr: String): String;
  function  ToCRLF(sStr: String): String;
  function  MkeSQLDateString(dte : TDateTime): String;
  function  NiceTimeString(iSeconds: Integer): String;


implementation

uses
  SysUtils; //, StrUtils;

//------------------------------------------------------------------------------
// Check, if the given string starts with this substring.
// Check ignores case.
function StrStartsWith(sStr, sSubstr: String): Boolean;
begin
  sStr := AnsiUpperCase(sStr);
  sSubstr := AnsiUpperCase(sSubstr);

  Result := Pos(sSubstr, sStr) = 1;
end;
//------------------------------------------------------------------------------
// Check, if the given string ends with this substring.
// Check ignores case.
function StrEndsWith(sStr, sSubstr: String): Boolean;
begin
  sStr := AnsiUpperCase(sStr);
  sSubstr := AnsiUpperCase(sSubstr);

  Result := RightStr(sStr, Length(sSubstr)) = sSubstr;
end;
//------------------------------------------------------------------------------
// Get the next token not including any of chars in sXcl.
// The start position is given in iPos.
// On return iPos stores the next position after the token.
//
// Example:
// var
//    iPos: Integer;  sTok: String;
// begin
//   iPos := 1;
//   sTok := Trim(GetNextToken(sList, iPos, ','));
//   while Length(sTok) > 0 do
//   begin
//     ShowMessage(sTok);
//     sTok := Trim(GetNextToken(sList, iPos, ','));
//   end;
// end;
function GetNextToken(sStr: String; var iPos: Integer; sXcl: String): String;
var
  sRet: String;
  i: Integer;
begin
  sRet  := '';

  if iPos > 0 then
  begin
    i := iPos;

    // skip the separator
    while (i <= Length(sStr)) and (Pos(sStr[i], sXcl) > 0) do
      Inc(i);

    // read the token
    while (i <= Length(sStr)) and (Pos(sStr[i], sXcl) = 0) do
    begin
      sRet := sRet + sStr[i];
      Inc(i);
    end;

    iPos := i;
  end;

  GetNextToken := sRet;
end;
//------------------------------------------------------------------------------
// Get the token starting with the specified pattern.
// iPos specifies the start of the string.
// On exit iPos returns the token's position.
// Token can be delimited with any character in sXcl.
// Example:
// ('a=15;b=-23;c=3', 'b', ';', 1) => 'b=-23'.
function GetTokenStartingWith(sStr, sPtt, sXcl: String; var iPos: Integer; bIgnoreCase: Boolean = True): String;
var
  sRet: String;
  iSta, iEnd: Integer;
begin
  if iPos <= 0 then iPos := 1;
  if bIgnoreCase then
    iSta := Pos(UpperCase(sPtt), UpperCase(Copy(sStr, iPos, Length(sStr)))) // start position
  else
    iSta := Pos(sPtt, Copy(sStr, iPos, Length(sStr))); // start position
  if iSta > 0 then // the token exists
  begin
    iSta := iSta + iPos - 1; // start position
    iEnd := iSta + Length(sPtt); // skip the pattern
    while (iEnd <= Length(sStr)) and (Pos(sStr[iEnd], sXcl) = 0) do
      Inc(iEnd);
    sRet := Copy(sStr, iSta, iEnd - iSta);
  end;
  iPos := iSta;
  Result := sRet;
end;
//------------------------------------------------------------------------------
// Get the value part of the token starting with the specified pattern.
// iPos specifies the start of the string. On exit it returns the token's
// position.
// Token can be delimited with any character in sXcl.
// Example:
// ('a=15;b=-23;c=3', 'b=', ';') => '-23'.
function GetValueStartingWith(sStr, sPtt, sXcl: String; sDft: String = ''): String;
var
  sRet: String;
  iPos: Integer;
begin
  iPos := 1;
  sRet := GetTokenStartingWith(sStr, sPtt, sXcl, iPos);
  if Length(sRet) > 0 then
  begin
    // take the part after the pattern
    sRet := Copy(sRet, Length(sPtt) + 1, Length(sRet));
  end
  else // empty result, possibly value not found
  begin
    if (iPos = 0) then // token not found, return the default value
      sRet := sDft;
  end;

  Result := Trim(sRet);
end;
//------------------------------------------------------------------------------
// Get the value part of the token.
// Example:
// ('a=15', '=') => '15'.
function GetTokenValue(sStr, sSep: String): String;
var
  sRet: String;
  iPos: Integer;
begin
  iPos := Pos(sSep, sStr);
  if (iPos> 0) then
  begin
    sRet := Copy(sStr, iPos + 1, Length(sStr));
  end;
  Result := sRet;
end;
//------------------------------------------------------------------------------
// Get the token starting with one character of the specified set.
// iPos specifies the start of the string.
// On exit iPos returns the token's position.
// Token can be delimited with any character in sXcl.
// Example:
// ('ext.235; ext.357', '0123456789', ';' > '235'.
function GetTokenStartingWithOneOf(sStr, sSet, sXcl: String; var iPos: Integer): String;
var
  sRet: String;
  iSta: Integer;
begin
  if iPos <= 0 then iPos := 1;

  sRet := GetNextToken(sStr, iPos, sXcl);

  // looks for any character given in sSet
  iSta := 1;
  while (iSta <= Length(sRet)) and (Pos(sRet[iSta], sSet) = 0) do
    Inc(iSta);
  
  if iSta <= Length(sRet) then // the token exists
    sRet := Copy(sRet, iSta, Length(sRet))
  else
    sRet := '';
  
  Result := sRet;
end;
//------------------------------------------------------------------------------
// Convert the string to an integer in a safe way.
// Return 0 if the string cannot be converted.
function CvtStr2Int(sBff: String): Integer;
var
  n: Integer;
begin
  n := 0;
  sBff := Trim(sBff);
  try
    if Length(sBff) > 0 then
      n := StrToIntDef(sBff, 0);
  except
    n := 0;
  end;
  CvtStr2Int := n;
end;
//------------------------------------------------------------------------------
// Convert the string to a double in a safe way.
// Return 0 if the string cannot be converted.
// Warning!
// Function assumes that DecimalSeparator is set to '.' !!!
function CvtStr2Dbl(sBff: String): Double;
var
  val: Double;
begin
  val := 0;
  sBff := Trim(sBff);
  try
    if Length(sBff) > 0 then
    begin
      sBff := ReplStr(',', '.', sBff);
      val := StrToFloat(sBff);
    end;
  except
    val := 0;
  end;
  CvtStr2Dbl := val;
end;
//------------------------------------------------------------------------------
// Return the XORed version of a string
function XORString(sStr: String; iXor: Integer): String;
var
  i: Integer;
begin
  for i := 1 to Length(sStr) do
    sStr[i] := Chr((Ord(sStr[i]) xor iXor));
  Result := sStr;
end;
//------------------------------------------------------------------------------
// Convert the double to a string that contains '.' as a decimal separator.
function CvtDbl2Str(dVal: Double): String;
var
  sBff: String;
begin
  sBff := FloatToStr(dVal);
  sBff := ReplStr(',', '.', sBff);
  CvtDbl2Str := sBff;
end;
//------------------------------------------------------------------------------
// Convert the boolean to '0' when false and '1' when true
function CvtBol2Str(fVal: Boolean): String;
begin
  if (fVal) then
    CvtBol2Str := '1'
  else
    CvtBol2Str := '0';
end;
//------------------------------------------------------------------------------
// Convert the given 64-bit Integer to a binary string.
// iLen - the requested length of the string, 0 if any.
function CvtInt2BinStr(iVal: Int64): String;
var
  sHexStr: String;
begin
  sHexStr := IntToHex(iVal, 1);
  CvtInt2BinStr := CvtHexStr2BinStr(sHexStr);
end;
//------------------------------------------------------------------------------
// Convert the given string to the hexadecimal representation
function CvtStr2HexStr(sStr: String): String;
var
  i: Integer;
  sRet: String;
begin
  for i := 1 to Length(sStr) do
  begin
    sRet := sRet + IntToHex(Ord(sStr[i]), 2);
  end;
  CvtStr2HexStr := sRet;
end;
//------------------------------------------------------------------------------
// Convert the given hexadecimal representation to the string
function CvtHexStr2Str(sStr: String): String;
var
  i: Integer;
  sHex, sRet: String;
begin
  for i := 1 to Length(sStr) div 2 do
  begin
    sHex := '$' + Char(Ord(sStr[i*2 - 1])) + Char(Ord(sStr[i*2]));
    sRet := sRet + Char(StrToInt(sHex));
  end;
  CvtHexStr2Str := sRet;
end;
//------------------------------------------------------------------------------
// Convert a string to a string that is for sure a valid string
// representation of an integer value.
function CvtStr2IntStr(sBff: String): String;
var
  n: Integer;
begin
  n := CvtStr2Int(sBff);
  CvtStr2IntStr := IntToStr(n);
end;
//------------------------------------------------------------------------------
// Convert a string to a string that is for sure a valid string
// representation of a floating point value.
// sSep - a separator to be used, "." or ",".
function CvtStr2DblStr(sBff, sSep: String): String;
var
  f: Double;
begin
  f := CvtStr2Dbl(sBff);
  sBff := FloatToStr(f);
  if sSep = '.' then
    sBff := ReplStr(',', '.', sBff)
  else
    sBff := ReplStr('.', ',', sBff);
  CvtStr2DblStr := sBff;
end;
//------------------------------------------------------------------------------
// Convert the binary string to hex string
function CvtBinStr2HexStr(sBin: String): String;
var
  i, iVal: Integer;
  sTok, sHexStr: String;
begin
  i := Length(sBin);
  if i mod 4 <> 0 then
    LPad(sBin, 4 - (i mod 4), '0');
  sHexStr := '';
  for i := 1 to Length(sBin) div 4 do
  begin
    sTok := Copy(sBin, Length(sBin) - i * 4 + 1, 4);
    iVal := 0;
    if sTok[1] = '1' then Inc(iVal, 8);
    if sTok[2] = '1' then Inc(iVal, 4);
    if sTok[3] = '1' then Inc(iVal, 2);
    if sTok[4] = '1' then Inc(iVal, 1);
    sHexStr := IntToHex(iVal, 1) + sHexStr;
  end;
  sHexStr := DelLedChr(sHexStr, '0');
  CvtBinStr2HexStr := sHexStr;
end;
//------------------------------------------------------------------------------
// Convert the binary string to hex string
function CvtHexStr2BinStr(sHex: String): String;
var
  sBinBff: String;
  cChar: Char;
  i: Integer;
begin
  sBinBff := '';
  sHex := UpperCase(sHex);
  for i := 1 to Length(sHex) do
  begin
    cChar := sHex[i];
    case cChar of
      '0': sBinBff := sBinBff + '0000';
      '1': sBinBff := sBinBff + '0001';
      '2': sBinBff := sBinBff + '0010';
      '3': sBinBff := sBinBff + '0011';
      '4': sBinBff := sBinBff + '0100';
      '5': sBinBff := sBinBff + '0101';
      '6': sBinBff := sBinBff + '0110';
      '7': sBinBff := sBinBff + '0111';
      '8': sBinBff := sBinBff + '1000';
      '9': sBinBff := sBinBff + '1001';
      'A': sBinBff := sBinBff + '1010';
      'B': sBinBff := sBinBff + '1011';
      'C': sBinBff := sBinBff + '1100';
      'D': sBinBff := sBinBff + '1101';
      'E': sBinBff := sBinBff + '1110';
      'F': sBinBff := sBinBff + '1111';
    end;
  end;
  sBinBff := DelLedChr(sBinBff, '0');
  CvtHexStr2BinStr := sBinBff;
end;
//------------------------------------------------------------------------------
// Replace given tokens inside the string.
// Function is case-sensitive.
function ReplStr(const sOld, sNew, sStr: String): String;
var
  sNewStr: String;
  sLeft, sRight: String;
  iPos, iLast: Integer;
  iCnt, iLen: Integer;
begin
  sNewStr := sStr;
  iLast := 0;
  iLen := Length(sStr);
  iCnt := 0;
  iPos := Pos(sOld, sNewStr);
  while (iPos > iLast) and (iCnt <= iLen) do
  begin
    Inc(iCnt);
    sLeft := Copy(sNewStr, 1, iPos - 1);
    sRight := Copy(sNewStr, iPos + Length(sOld), Length(sNewStr));
    sNewStr := sLeft + sNew + sRight;
    iLast := iPos; // to avoid endless loops
    iPos := Pos(sOld, sNewStr);
  end;
  ReplStr := sNewStr;
end;
//------------------------------------------------------------------------------
// Return the left part of the string
function LeftStr(sStr: String; iCnt: Integer): String;
begin
  LeftStr := Copy(sStr, 1, iCnt)
end;
//------------------------------------------------------------------------------
// Return the right part of the string
function RightStr(sStr: String; iCnt: Integer): String;
begin
  if Length(sStr) >= iCnt then
    RightStr := Copy(sStr, Length(sStr) - iCnt + 1, iCnt)
  else
    RightStr := '';
end;
//------------------------------------------------------------------------------
// Return the shortened version the string (a version of a Left function).
// iCnt specified how many characters to trim.
// Example:
//   ShorterStr('Autoexec.bat', 4) => 'Autoexec'
function ShorterStr(sStr: String; iCnt: Integer): String;
begin
  if Length(sStr) > iCnt then
    ShorterStr := Copy(sStr, 1, Length(sStr) - iCnt)
  else
    ShorterStr := '';
end;
//------------------------------------------------------------------------------
// If 'sStr' is shorter than 'num', pad it left with the 'chPad'
function LPad(sStr: String; num: Integer; chPad: Char): String;
var
  i, iLen: Integer;
begin
  iLen := Length(sStr);
  if iLen < num then
  begin
    for i := 1 to num - iLen do
      sStr := chPad + sStr;
  end;
  LPad := sStr;
end;
//------------------------------------------------------------------------------
// If 'sStr' is shorter than 'num', pad it right with the 'chPad'
function RPad(sStr: String; num: Integer; chPad: Char): String;
var
  i, iLen: Integer;
begin
  iLen := Length(sStr);
  if iLen < num then
  begin
    for i := 1 to num - iLen do
      sStr := sStr + chPad;
  end;
  RPad := sStr;
end;
//------------------------------------------------------------------------------
// Remove all leading characters 'cChar' from the string
function DelLedChr(const sStr: String; cChar: Char): String;
var
  sBff: String;
begin
  sBff := sStr;
  while (Length(sBff) > 0) and (sBff[1] = cChar) do
    sBff := Copy(sBff, 2, Length(sBff) - 1);
  DelLedChr := sBff
end;
//------------------------------------------------------------------------------
// Remove all trailing characters 'cChar' from the string
function DelTrlChr(const sStr: String; cChar: Char): String;
var
  sBff: String;
begin
  sBff := sStr;
  while (Length(sBff) > 0) and (sBff[Length(sBff)] = cChar) do
    sBff := Copy(sBff, 1, Length(sBff) - 1);
  DelTrlChr := sBff
end;
//------------------------------------------------------------------------------
// Extract only the name part of the filespec, without an extension
function ExtractFileNam(const sName: String): String;
var
  sNam, sExt: String;
begin
  sNam := ExtractFileName(sName);
  sExt := ExtractFileExt(sNam);
  if Length(sExt) > 0 then // an extension exists, remove it
    sNam := ShorterStr(sNam, Length(sExt));
  ExtractFileNam := sNam;
end;
//------------------------------------------------------------------------------
// Function replaces the existing extension in the file name to the
// specified one.
// Example:
//   SwapFileExt('c:\text.txt', 'doc') => 'c:\text.doc'
function SwapFileExt(const sName: String; const sNewExt: String): String;
var
  sExt: String;
  sNam: String;
begin
  sNam := Trim(sName);
  if UpperCase(ExtractFileExt(sNam)) <> UpperCase('.' + sNewExt) then
  begin
    sExt := ExtractFileExt(sNam);
    if Length(sExt) > 0 then
    begin
      sNam := ShorterStr(sNam, Length(sExt)); // remove extension
    end;
    sNam := sNam + '.' + sNewExt;
  end;
  SwapFileExt := sNam;
end;
//------------------------------------------------------------------------------
// Function concatenates a path and a file name taking care of
// a single '\' inbetween.
function MakePath(sPath, sFile: String): String;
begin
  if RightStr(sPath, 1) <> '\' then
    sPath := sPath + '\';
  sPath := sPath + sFile;
  MakePath := sPath;
end;
//------------------------------------------------------------------------------
// The function verifies and corrects the specified file name
function ValidPath(const sPath: String): String;
var
  sRetPath: String;
begin
  // QQtodo detect other possible problems
  sRetPath := Trim(sPath);
  if Pos('\\', sRetPath) > 0 then
    sRetPath := ReplStr('\\', '\', sRetPath);
  if Pos('//', sRetPath) > 0 then
    sRetPath := ReplStr('//', '/', sRetPath);
  Result := sRetPath;
end;
//------------------------------------------------------------------------------
// The function verifies and corrects the specified file name
{
Under MS Windows, file and directory names must not contain
\ / : * ? " < > |
and should not contain
& ; , ^ ( )
because they are used as NT 4.0 command-line paramters.
}
function ValidFileName(const sName: String): String;
var
  sRetName: String;
begin
  // QQtodo detect other possible problems
  sRetName := Trim(sName);
  if Pos(':', sRetName) > 0 then
    sRetName := ReplStr(':', '_', sRetName);
  if Pos('/', sRetName) > 0 then
    sRetName := ReplStr('/', '_', sRetName);
  if Pos('\', sRetName) > 0 then
    sRetName := ReplStr('\', '_', sRetName);
  Result := sRetName;
end;
//------------------------------------------------------------------------------
// Compare two menu items that can possibly differ in & characters
function CompareMenuItems(txt1, txt2: String): Boolean;
begin
  txt1 := ReplStr('&', '', txt1);
  txt2 := ReplStr('&', '', txt2);
  CompareMenuItems := AnsiCompareText(txt1, txt2) = 0;
end;
//------------------------------------------------------------------------------
// Split the text like 50x20 into 50 and 20
procedure SplitSizeString(sText: String; var dx: Integer; var dy: Integer);
var
  sTokX, sTokY: String;
  i: Integer;
begin
  sText := StringReplace(sText, '&', '', [rfReplaceAll]); // just in case it's from menu

  i := 1;
  sTokX := GetNextToken(sText, i, 'x ');
  sTokY := GetNextToken(sText, i, 'x ');
  dx := CvtStr2Int(sTokX);
  dY := CvtStr2Int(sTokY);
end;
//------------------------------------------------------------------------------
// Returns the string that represents given font parameters
// Syntax:
// SyntaxVersion=1,"Name",Charset,Color,Height,Pitch,Bold,Italic,Underline,Strikeout
function FontToString(font: TFont): String;
var
  sTmp: String;
  sFntStr: String;
begin
  sFntStr := '1';                                                   // syntax version
  sFntStr := sFntStr + ',' + font.Name;                             // font name
  sFntStr := sFntStr + ',' + IntToStr(font.Charset);                // character set
  sFntStr := sFntStr + ',' + ColorToString(font.Color);             // color
  sFntStr := sFntStr + ',' + IntToStr(font.Height);                 // text height in pixel
  sFntStr := sFntStr + ',' + IntToStr(ord(font.Pitch));             // pitch (fpDefault, fpVariable, fpFixed);
  if (fsBold in font.Style) then sTmp := '1' else sTmp := '0';      // check if bold
  sFntStr := sFntStr + ',' + sTmp;                                  // bold 1|0
  if (fsItalic in font.Style)then sTmp := '1' else sTmp := '0';     // check if italic
  sFntStr := sFntStr + ',' + sTmp;                                  // italic 1|0
  if (fsUnderline in font.Style) then sTmp := '1' else sTmp := '0'; // check if underline
  sFntStr := sFntStr + ',' + sTmp;                                  // underline 1|0
  if (fsStrikeOut in font.Style) then sTmp := '1' else sTmp := '0'; // check if strikeout
  sFntStr := sFntStr + ',' + sTmp;                                  // strikeout 1|0
  FontToString := sFntStr;
end;
//------------------------------------------------------------------------------
// Sets the font parameters represented by parameters given in string
// Syntax:
// SyntaxVersion,"Name",Charset,Color,Height,Pitch,Bold,Italic,Underline,Strikeout
Procedure StringToFont(sFntStr: String; var font: TFont);
var
   sFntNam : String;  // the name of the font
   nChrSet : Integer; // the characterset number
   sFntClo : String;  // color definition string
   nFntHgh : Integer; // the font height
   nPchOrd : Integer; // the pitch ordinal number (fpDefault, fpVariable, fpFixed);
   bBldFlg : Boolean; // bold flag
   bItlFlg : Boolean; // italic flag
   bUdlFlg : Boolean; // underline flag
   bStoFlg : Boolean; // strike-out flag
   nStv    : Integer; // syntax version
   nPos    : Integer; // position in string;
begin
   nPos    := 1;
   nStv    := StrToInt(GetNextToken(sFntStr, nPos, ','));
   if (nStv = 1) then // first syntax version
   begin
     sFntNam := GetNextToken(sFntStr, nPos, ',');
     nChrSet := StrToInt(GetNextToken(sFntStr, nPos, ','));
     sFntClo := GetNextToken(sFntStr, nPos, ',');
     nFntHgh := StrToInt(GetNextToken(sFntStr, nPos, ','));
     nPchOrd := StrToInt(GetNextToken(sFntStr, nPos, ','));
     bBldFlg := (GetNextToken(sFntStr, nPos, ',') = '1');
     bItlFlg := (GetNextToken(sFntStr, nPos, ',') = '1');
     bUdlFlg := (GetNextToken(sFntStr, nPos, ',') = '1');
     bStoFlg := (GetNextToken(sFntStr, nPos, ',') = '1');
   end
   else
   begin
     sFntNam := '';
     nChrSet := 0;
     sFntClo := '';
     nFntHgh := 0;
     nPchOrd := 0;
     bBldFlg := False;
     bItlFlg := False;
     bUdlFlg := False;
     bStoFlg := False;
   end;
   font.Name    := sFntNam;
   font.Charset := nChrSet;
   font.Color   := StringToColor(sFntClo);
   font.Height  := nFntHgh;
   case nPchOrd of
     0: font.Pitch := fpDefault;
     1: font.Pitch := fpVariable;
     2: font.Pitch := fpFixed;
   else
     font.Pitch := fpDefault;
   end;
   font.Style := [];
   if bBldFlg then
     font.Style := font.Style + [fsBold];
   if bItlFlg then
     font.Style := font.Style + [fsItalic];
   if bUdlFlg then
     font.Style := font.Style + [fsUnderline];
   if bStoFlg then
     font.Style := font.Style + [fsStrikeOut];
end;
//------------------------------------------------------------------------------
// Make an RGB string out of the given color
function GetHTMLRGBString(clo: TColor): String;
var
  ir, ig, ib: Integer;
  lClo: Longint;
begin
  lClo := ColorToRGB(clo); // make RGB value first, symbolic colors are not RGB!!!
  ir := GetRValue(lClo);
  ig := GetGValue(lClo);
  ib := GetBValue(lClo);
  GetHTMLRGBString := IntToHex(ir, 2) + IntToHex(ig, 2) + IntToHex(ib, 2);
end;
//------------------------------------------------------------------------------
// Return a string that for sure is not Null
// Useful when retrieving strings from database fields.
function Nzs(sTxt: Variant): String;
begin
  if VarIsNull(sTxt) then
    Nzs := ''
  else
    Nzs := sTxt;
end;
//------------------------------------------------------------------------------
// Return an integer that for sure is not Null
// Useful when retrieving strings from database fields.
function Nzi(vNum: Variant): Integer;
begin
  if vNum = Null then
    Nzi := 0
  else
    Nzi := vNum;
end;
//------------------------------------------------------------------------------
// Convert the date string in the fixed dd.mm.yyyy format to a date.
// Function properly recognizes also dates with no separators (27021999)
function DDMMYYYYToDate(vDate: String): TDateTime;
var
  i: Integer;
  sYear, sMonth, sDay: String;
  iYear, iMonth, iDay: Integer;
  sDate: String;
begin
  DDMMYYYYToDate := Date;

  if Length(vDate) >= 4 then // no shorter date makes sense
  begin
    sDate := ReplStr('-', '.', vDate);
    sDate := ReplStr('/', '.', sDate);
    sDate := ReplStr(',', '.', sDate);
    sDate := ReplStr(' ', '.', sDate);

    if Pos('.', sDate) > 0 then // contains separators
    begin
      i := 1;
      sDay := GetNextToken(sdate, i, '.');
      sMonth := GetNextToken(sdate, i, '.');
      sYear := GetNextToken(sdate, i, '.');

      iDay := CvtStr2Int(sDay);
      iMonth := CvtStr2Int(sMonth);
      iYear := CvtStr2Int(sYear);
    end
    else // date with no separators, e.g., 18031999
    begin
      iYear  := CvtStr2Int(Copy(sdate, 5, 4));
      iMonth := CvtStr2Int(Copy(sDate, 3, 2));
      iDay   := CvtStr2Int(Copy(sDate, 1, 2));
    end;

    if iYear < 50   then iYear := iYear + 2000;
    if iYear < 1900 then iYear := iYear + 1900;

    if iMonth < 1 then iMonth := 1
    else if iMonth > 12 then iMonth := 12;
    if iDay < 1 then iDay := 1
    else if iDay > 31 then iDay := 31;

    DDMMYYYYToDate := EncodeDate(iYear, iMonth, iDay);
  end;
end;
//------------------------------------------------------------------------------
// Convert the date string in the fixed yyyy.mm.dd format to a date.
// Function properly recognizes also dates with no separators (19990501)
function YYYYMMDDToDate(vDate: String): TDateTime;
var
  i: Integer;
  sYear, sMonth, sDay: String;
  iYear, iMonth, iDay: Integer;
  sDate: String;
begin
  YYYYMMDDToDate := Date;

  if Length(vDate) >= 4 then // no shorter date makes sense
  begin
    sDate := ReplStr('-', '.', vDate);
    sDate := ReplStr('/', '.', sDate);
    sDate := ReplStr(',', '.', sDate);
    sDate := ReplStr(' ', '.', sDate);

    if Pos('.', sDate) > 0 then // contains separators
    begin
      i := 1;
      sYear := GetNextToken(sdate, i, '.');
      sMonth := GetNextToken(sdate, i, '.');
      sDay := GetNextToken(sdate, i, '.');

      iDay := CvtStr2Int(sDay);
      iMonth := CvtStr2Int(sMonth);
      iYear := CvtStr2Int(sYear);
    end
    else // date with no separators, e.g., 18031999
    begin
      iYear  := CvtStr2Int(Copy(sdate, 1, 4));
      iMonth := CvtStr2Int(Copy(sDate, 5, 2));
      iDay   := CvtStr2Int(Copy(sDate, 7, 2));
    end;

    if iYear < 50   then iYear := iYear + 2000;
    if iYear < 1900 then iYear := iYear + 1900;

    if iMonth < 1 then iMonth := 1
    else if iMonth > 12 then iMonth := 12;
    if iDay < 1 then iDay := 1
    else if iDay > 31 then iDay := 31;

    YYYYMMDDToDate := EncodeDate(iYear, iMonth, iDay);
  end;
end;
//------------------------------------------------------------------------------
// Read the text file into the list of strings
function ReadTextFileToStrings(sFilNam: String; var strLis: TStringList): Boolean;
var
  fOk: Boolean;
begin
  fOk := False; // Let's be pesimistic (realistic?)
  strLis.Clear; // prepare the list of strings
  if FileExists(sFilNam) then
  begin
    try
      strLis.LoadFromFile(sFilNam);
    except
      ;
    end;
    fOk := strLis.Count > 0;
  end;

  ReadTextFileToStrings := fOk;
end;
//------------------------------------------------------------------------------
// Cut the string at the first occurence of the specified character
// Example:
//   CutStringAtChar('Tralala/Bum/Oj', '/') => 'Tralala'
function CutStringAtChar(sStr: String; sChr: Char): String;
var
  nPos: Integer;
  sRet: String;
begin
  sRet := sStr;
  nPos := Pos(sChr, sRet);
  if (nPos > 0) then
    sRet := LeftStr(sStr, nPos - 1);

  CutStringAtChar := sRet;
end;
//------------------------------------------------------------------------------
// Remove trailing backslash (if it exists)
function NoLastBackslash(sBff: String): String;
begin
  if Copy(sBff, Length(sBff), 1) = '\' then
    sBff := Copy(sBff, 1, Length(sBff) - 1);
  NoLastBackslash := sBff;
end;
//------------------------------------------------------------------------------
// Calculate the count of appearances of sChr in sStr.
function CntOfChr(sChr: String; sStr: String): Integer;
var
  iCnt, iPos, iAbs: Integer;
  sBff: String;
begin
  iCnt := 0;
  sBff := Copy(sStr, 1, Length(sStr));
  iPos := Pos(sChr, sBff);
  iAbs := iPos;
  while iPos > 0 do
  begin
    Inc(iCnt);
    sBff := Copy(sStr, iAbs + Length(sChr), Length(sBff));
    iPos := Pos(sChr, sBff);
    Inc(iAbs, iPos);
  end;

  CntOfChr := iCnt;
end;
//------------------------------------------------------------------------------
// Detect and extract all kinds of URLs from the given string.
// URLs are returned in the strLis. Each URL is preceeded with
// its offset within the sString, e.g., "17,http://www.msn.com"
//
// URL specs:
//   http://www.netspace.org/users/dwb/url-guide.html
//   http://www.cis.ohio-state.edu/cgi-bin/rfc/rfc1738.html
function ExtractURLs(sString: String; var strLis: TStringList): Boolean;
type
  TURLChars = set of Char;

var
  URLChars: TURLChars;

  procedure AddURL(str: String; ofs: Integer);
  begin
    str := DelTrlChr(str, ',');
    str := DelTrlChr(str, '.');
    strLis.Add(IntToStr(ofs) + ',' + str);
  end;

  procedure ExtractURL(sStr: String; sIdf: String; isLeft: Boolean; chars: TURLChars; cNotLeft: Char);
  var
    p, pl, pr: Integer;
    sURL: String;
  begin
    p := Pos(sIdf, LowerCase(sStr)); // find identifier: http://, @, ftp://, etc.
    while p > 0 do
    begin
      pl := p;
      pr := p + Length(sIdf) - 1;
      sURL := '';
      if isLeft then // only e-mails have the left part (left to '@')
        while (pl > 1) and (sStr[pl-1] in chars) do
          Dec(pl);
      while (pr < Length(sStr)) and (sStr[pr+1] in chars) do
        Inc(pr);
      if ((not isLeft) or (pl < p)) and (pr > p + Length(sIdf) - 1) then
      begin
        if (cNotLeft = ' ') then // no left character restriction
        begin
          sURL := Copy(sStr, pl, pr - pl + 1);
          AddURL(sURL, pl);
        end
        else // restricted left character
        begin
          if (p = 1) or (sStr[p-1] <> cNotLeft) then
          begin
            sURL := Copy(sStr, pl, pr - pl + 1);
            AddURL(sURL, pl);
          end;
        end;
      end;
      p := PosOfs(sIdf, LowerCase(sStr), p + 1);
    end;
  end;

begin
  strLis.Clear;

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '#', '@', '&', '%'];
  ExtractURL(sString, 'http://',  False, URLChars, ' '); // HyperText Transfer Protocol (HTTP)
  ExtractURL(sString, 'https://', False, URLChars, ' ');
  ExtractURL(sString, 'www.',     False, URLChars, '/');

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '#', '@', '&', '%', ';'];
  ExtractURL(sString, 'ftp://',   False, URLChars, ' '); // File Transfer Protocol (FTP)
  ExtractURL(sString, 'ftps://',  False, URLChars, ' ');

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '#', '@', '&', '%'];
  ExtractURL(sString, 'telnet://', False, URLChars, ' '); // Telnet to Remote Host (Telnet)
  ExtractURL(sString, 'tn3270://', False, URLChars, ' '); // Telnet to Remote Hosts Requiring 3270 Emulation (TN3270)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '#', '@', '&', '%', '!', '$'];
  ExtractURL(sString, 'gopher://', False, URLChars, ' '); // Gopher Protocol (Gopher)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '#', '@', '&', '%'];
  ExtractURL(sString, 'wais://', False, URLChars, ' ');  // Wide Area Information Search (WAIS)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '%'];
  ExtractURL(sString, 'news:', False, URLChars, ' '); // Usenet News (News)
  ExtractURL(sString, 'nntp://', False, URLChars, ' '); // USENET News Using NNTP Access (NNTP)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '%'];
  ExtractURL(sString, '@', True, URLChars, ' '); // Electronic Mail (Mailto)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, '-', '_', '+'];
  ExtractURL(sString, 'prospero://', True, URLChars, ' '); // Pospero Directory Service (Prospero)

  URLChars := ['a'..'z', '.', 'A'..'Z', '0'..'9', #128..#255, ':', '-', '_', '+', '?', '=', '~', '/', ',', '\', '#', '@', '&', '%', '+'];
  ExtractURL(sString, 'file://',  False, URLChars, ' '); // Host-Specific File Names (File)
{
  p := Pos('www.', LowerCase(sTxt));
  while p > 0 do
  begin
    pr := p + 3;
    sURL := '';
    while (pr < Length(sTxt)) and (sTxt[pr+1] in URLChars) do
      Inc(pr);
    if (pr > p + 3) then
    begin
      sURL := Copy(sTxt, p, pr - p + 1);
      AddURL(sURL);
    end;
    sTxt := Copy(sTxt, 1, p-1) + Copy(sTxt, pr+1, Length(sTxt)); // remove found token
    p := Pos('www.', LowerCase(sTxt));
  end;
}
  ExtractURLs := (strLis.Count > 0);
end;
//------------------------------------------------------------------------------
function GetCRC(str: String): String;
var
  i, l: Integer;
begin
  l := 0;
  for i := 1 to Length(str) do
    Inc(l, Ord(str[i]));
  Result := IntToHex(l mod 256, 2);
end;
//------------------------------------------------------------------------------
function GetCRC(strLis: TStringList; iSta: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := iSta to strLis.Count - 1 do
    Result := Result + GetCRC(strLis.Strings[i]);
end;
//------------------------------------------------------------------------------
// The same as Pos, but doesn't have to start from the beginning of the str.
// Example:
//   PosOfs('the', 'the smell of the sun', 5) => 14
function PosOfs(substr: String; str: String; ofs: Integer): Integer;
var
  iPos: Integer;
begin
  str := Copy(str, ofs, Length(str) - ofs + 1);
  iPos := Pos(substr, str);
  if iPos > 0 then
    Result := iPos + ofs - 1
  else
    Result := 0;
end;
//------------------------------------------------------------------------------
// Check, if the given string is a valid date (according to system date format setting)
{$HINTS OFF}  // ignore 'value assigned to dt not used' hint
function IsStrValidDate(str: string): Boolean;
var
  dt: TDateTime;
begin
  Result := True;
  try
    dt := StrToDate(str);
  except
    Result := False;
  end;
end;
{$HINTS ON}
//------------------------------------------------------------------------------
// Convert the given string to a date
function CvtStr2Date(const sDate: String): TDateTime;
begin
  if IsStrValidDate(sDate) then
    Result := StrToDate(sDate)
  else
    // #todo3 recognize various strange date formats
    Result := 0;
end;
//------------------------------------------------------------------------------
// Check, if sThisVsn is newer than sBaseVsn.
// Return value:
//    0 - the same version
//   >0 - we have a newer version (???)
//   <0 - we have an older version
// Version strings are in format Major.Minor.Release.Build
// Example: "2.57.0.117" or "2.57.0.117 Beta"
function CompareVersions(sThisVsn, sBaseVsn: String): Int64;
var
  iThisMaj, iThisMin, iThisRel, iThisBld: Int64;
  iBaseMaj, iBaseMin, iBaseRel, iBaseBld: Int64;
  icThis, icBase: Int64;
  sTok: String;
  i: Integer;
begin
  sThisVsn := Trim(sThisVsn);
  sBaseVsn := Trim(sBaseVsn);
  iThisMaj := 0; iThisMin := 0; iThisRel := 0; iThisBld := 0;
  iBaseMaj := 0; iBaseMin := 0; iBaseRel := 0; iBaseBld := 0;

  i := 1;
  sTok := GetNextToken(sThisVsn, i, '.');
  if Length(sTok) > 0 then
  begin
    iThisMaj := CvtStr2Int(sTok);
    sTok := GetNextToken(sThisVsn, i, '.');
    if Length(sTok) > 0 then
    begin
      iThisMin := CvtStr2Int(sTok);
      sTok := GetNextToken(sThisVsn, i, '.');
      if Length(sTok) > 0 then
      begin
        iThisRel := CvtStr2Int(sTok);
        sTok := GetNextToken(sThisVsn, i, '., ');
        if Length(sTok) > 0 then
        begin
          iThisBld := CvtStr2Int(sTok);
        end;
      end;
    end;
  end;

  i := 1;
  sTok := GetNextToken(sBaseVsn, i, '.');
  if Length(sTok) > 0 then
  begin
    iBaseMaj := CvtStr2Int(sTok);
    sTok := GetNextToken(sBaseVsn, i, '.');
    if Length(sTok) > 0 then
    begin
      iBaseMin := CvtStr2Int(sTok);
      sTok := GetNextToken(sBaseVsn, i, '.');
      if Length(sTok) > 0 then
      begin
        iBaseRel := CvtStr2Int(sTok);
        sTok := GetNextToken(sBaseVsn, i, '., ');
        if Length(sTok) > 0 then
        begin
          iBaseBld := CvtStr2Int(sTok);
        end;
      end;
    end;
  end;

  icThis := iThisMaj*10000000 + iThisMin*100000 + iThisRel*1000 + iThisBld;
  icBase := iBaseMaj*10000000 + iBaseMin*100000 + iBaseRel*1000 + iBaseBld;

  Result := icThis - icBase;
end;
//------------------------------------------------------------------------------
// The function removes the quote characters from the beginning and end of
// a quoted string, and reduces pairs of quote characters within the string
// to a single quote character.
// If the first character in the string is not a quote character, the original
// string is returned.
function DequotedStr(sStr: String): String;
var
  p: PChar;
begin
  Result := sStr;
  if (sStr <> '') then // dequote
  begin
    p := PChar(sStr);
    if p^ = '"' then
      Result := AnsiExtractQuotedStr(p, '"');
  end;
end;
//------------------------------------------------------------------------------
// Change all CRLF to '||'
function CRLFTo(sStr: String): String;
var
  sNewStr: String;
begin
  sNewStr := ReplStr(Chr(13) + Chr(10), '||', sStr);
  sNewStr := ReplStr(Chr(13), '||', sNewStr);
  sNewStr := ReplStr(Chr(10), '||', sNewStr);
  Result := sNewStr;
end;
//------------------------------------------------------------------------------
function  ToCRLF(sStr: String): String;
var
  sNewStr: String;
begin
  sNewStr := ReplStr('||', Chr(13) + Chr(10), sStr);
  Result := sNewStr;
end;
//------------------------------------------------------------------------------
// Convert a date to the strange American format #mm/dd/yy# used in SQL
// statements
function MkeSQLDateString(dte : TDateTime): String;
var
  sDteStr: String;
begin
  // sDteStr := Month(dte) & "/" & Day(dte) & "/" & Year(dte)
  DateTimeToString(sDteStr, 'm"/"d"/"yyyy', dte);
  Result := '#' + sDteStr + '#';
end;
//------------------------------------------------------------------------------
// Return the nicely formatted string representing the given amount of seconds
function NiceTimeString(iSeconds: Integer): String;
var
  iTot, iSec, iMin, iHor: Integer;
begin
  iTot :=  iSeconds;
  iSec := iTot mod 60;
  iTot := iTot div 60;
  iMin := iTot mod 60;
  iTot := iTot div 60;
  iHor := iTot mod 60;

  if (iHor > 0) then
    Result := Format('%d:%.2d:%.2d', [iHor, iMin, iSec])
  else
    Result := Format('%d:%.2d', [iMin, iSec]);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

begin
  DecimalSeparator := '.';
end.




