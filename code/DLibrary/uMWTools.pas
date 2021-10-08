{*****************************************************************************
 *
 *  uMWTools.pas - General-purpose tools, useful in many projects
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

unit uMWTools;

interface

uses
  Windows, Messages, SysUtils, Dialogs, StdCtrls, ComCtrls, Registry,
  Math, Graphics;

  procedure SwapInts(var i1, i2: Integer);
  function  IntToBool(iNum: Integer): Boolean;
  function  BoolToInt(fNum: Boolean): Integer;
  procedure AddComboHistory(cmbBox: TComboBox; str: String; maxCnt: Integer);
  function  InRaster(iVal: Integer; iRas: Integer; fRas: Boolean): Integer;
  procedure RegApplication(sDsc, sPth: String; iIco: Integer);
  procedure RegFileType(sExt, sDsc, sPth: String; iIco: Integer);
  function  CtrlDown: Boolean;
  function  ShiftDown: Boolean;
  function  AltDown: Boolean;
  function  GetFirstFileInFolder(sGivenPath: String): String;
  function  GetNextFileInFolder(sGivenPath: String): String;
  function  Sign(iVal: Integer): Integer;
  function  BoundInt(iMin: Integer; var iVal: Integer; iMax: Integer): Integer;
  function  BoundByte(iMin: Byte; var iVal: Byte; iMax: Byte): Byte;
  function  BoundDbl(iMin: Double; var iVal: Double; iMax: Double): Double;
  function  PowerXY(x, y: Extended): Extended;
  function  ExecAndWait(const Filename, Params: String; WindowState: Word): Boolean;
//  function  WinExecAndWait(Path: PChar; Visibility: Word): Word;
  function  MakeRect(Pt1: TPoint; Pt2: TPoint): TRect;
  function  RoundIt(dVal: Double; iDgt: Integer): Double;
  function  iifs(fCon: Boolean; sTrue, sFalse: String): String;
  function  iifi(fCon: Boolean; iTrue, iFalse: Integer): Integer;
  procedure Sec2HorMin(totSec: Integer; var hor, min: Integer);
  function  RandomInRange(intLow, intHi: Integer): Integer;
  procedure SetListViewTopItem(idx: Integer; LV: TListView);
  function  HasLargeFonts: Boolean;
  function  RgbToGray(RGBColor: TColor): TColor;
  function  ColorToHTMLString(Color: TColor): String;
  function  HTMLStringToColor(sColor: string): TColor;
  function  BrowseDialog(hdl: THandle; const Title: String; const Flag: Integer): String;
  function  SelectDirectory(hdl: THandle; const Caption, InitialDir: String; const Root: WideString; ShowStatus: Boolean; out Directory: String): Boolean;
  procedure PlayBeep(ActionType: TMsgDlgType);
  function  GetWindowTextString(hWindow: hWnd): String;
  function  GetDateTimeStamp: String;
  function  GetDateStamp: String;
  function  GetTimeStamp: String;
  function  GetOSVersion(var MajorVersion, MinorVersion, Build: DWORD): String;
  procedure DelayMSec(msec: Integer);

implementation

uses
  Forms, ShlObj, uMWStrings, ActiveX;

//------------------------------------------------------------------------------
// Swap two integers
procedure SwapInts(var i1, i2: Integer);
var
  iTmp: Integer;
begin
  iTmp := i1;
  i1   := i2;
  i2   := iTmp;
end;
//------------------------------------------------------------------------------
// Convert integers to Boolean. 0 is False, the rest is True.
function IntToBool(iNum: Integer): Boolean;
begin
  if iNum = 0 then
    IntToBool := False
  else
    IntToBool := True;
end;
//------------------------------------------------------------------------------
// Convert Booleans to integers. False is 0, True is 1.
function BoolToInt(fNum: Boolean): Integer;
begin
  if fNum then
    BoolToInt := 1
  else
    BoolToInt := 0;
end;
//------------------------------------------------------------------------------
// Add given text to the combo history
procedure AddComboHistory(cmbBox: TComboBox; str: String; maxCnt: Integer);
var
  i: Integer;
begin
  for i := 0 to cmbBox.Items.Count - 1 do
  begin
    if cmbBox.Items[i] = str then
    begin
      cmbBox.Items.Delete(i);
    end;
  end;
  cmbBox.Items.Insert(0, str);
  cmbBox.Text := str;
end;
//------------------------------------------------------------------------------
// Set the given value to the nearest raster point.
// Raster calculation is relative to 0 point.
function InRaster(iVal: Integer; iRas: Integer; fRas: Boolean): Integer;
begin
  if fRas then // raster active?
  begin
    iVal := iVal + (iRas div 2);
    InRaster := iVal - (iVal mod iRas);

//    if (iVal mod iRas) <> 0 then
//    begin
//      iVal := iVal + (iRas div 2); // rounding
//      InRaster := (iVal div iRas) * iRas;
//    end;
  end
  else
    InRaster := iVal;
end;
//------------------------------------------------------------------------------
// Check, if [Ctrl] key is down
function CtrlDown: Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Control] And 128) <> 0);
end;
//------------------------------------------------------------------------------
// Check, if [Shift] key is down
function ShiftDown: Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;
//------------------------------------------------------------------------------
// Check, if [Alt] key is down
function AltDown: Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Menu] and 128) <> 0);
end;
//------------------------------------------------------------------------------
// Register the application
// sDsc: application's description (e.g., 'MyApp.Document')
// sPth: application's path
// iIco: icon index within .EXE file, usually 0
procedure RegApplication(sDsc, sPth: String; iIco: Integer);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CLASSES_ROOT;
  reg.LazyWrite := False;

  // Register application
  reg.CloseKey;
  reg.OpenKey('\' + sDsc + '\Shell\Open\Command', True);
  reg.WriteString('', '"' + sPth + '" "%1"');

  reg.CloseKey;
  reg.OpenKey('\' + sDsc + '\DefaultIcon', True);
  reg.WriteString('', sPth + ',0');

  reg.CloseKey;
  reg.Free;
end;
//------------------------------------------------------------------------------
// Register the file extension
// sExt: extension (e.g., '.MCL' or 'MCL')
// sDsc: application's description (e.g., 'MyApp.Document')
// sPth: application's path
// iIco: icon index within .EXE file, usually 0
procedure RegFileType(sExt, sDsc, sPth: String; iIco: Integer);
var
  reg: TRegistry;
begin
  if Length(sExt) > 0 then
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := False;

    if sExt[1] <> '.' then
      sExt := '.' + sExt;

    // Register extension
    reg.OpenKey('\' + sExt, True);
    reg.WriteString('', sDsc); // file description

    // Register application
    reg.CloseKey;
    reg.OpenKey('\' + sDsc + '\Shell\Open\Command', True);
    reg.WriteString('', '"' + sPth + '" "%1"');

    reg.CloseKey;
    reg.OpenKey('\' + sDsc + '\DefaultIcon', True);
    reg.WriteString('', sPth + ',0');

    reg.CloseKey;
    reg.Free;

    // Update system to notify of association change
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_FLUSH, PChar(''), PChar(''));
  end;
end;
//------------------------------------------------------------------------------
// Return the first file in the folder of the given file
function GetFirstFileInFolder(sGivenPath: String): String;
var
  SearchRec: TSearchRec;
  sFirstPath: String;
begin
  if FindFirst(ExtractFilePath(sGivenPath) + '*.*', faReadOnly + faArchive, SearchRec) = 0 then
    sFirstPath := ExtractFilePath(sGivenPath) + SearchRec.Name;

  GetFirstFileInFolder := sFirstPath;
end;
//------------------------------------------------------------------------------
// Return the next file after the one passed as a parameter
function GetNextFileInFolder(sGivenPath: String): String;
var
  SearchRec: TSearchRec;
  fFound: Boolean;
  sGivenName, sThisName: String;
  sNextPath: String;
begin
  fFound := False;
  sGivenName := AnsiUpperCase(ExtractFileName(sGivenPath));
  if FindFirst(ExtractFilePath(sGivenPath) + '*.*', faReadOnly + faArchive, SearchRec) = 0 then
  begin
    repeat
      sThisName := AnsiUpperCase(SearchRec.Name);
      if sThisName = sGivenName then // given file found
      begin
        fFound := True;
        break;
      end;
    until FindNext(SearchRec) <> 0
  end;

  // last file found, try to open the next file
  if fFound then
    if FindNext(SearchRec) = 0 then
      sNextPath := ExtractFilePath(sGivenPath) + SearchRec.Name;

  FindClose(SearchRec);
  GetNextFileInFolder := sNextPath;
end;
//------------------------------------------------------------------------------
// Return the sign (+1/-1) of the given integer value
function Sign(iVal: Integer): Integer;
begin
  if iVal >= 0 then
    Sign := 1
  else
    Sign := -1;
end;
//------------------------------------------------------------------------------
// Correct the given int value so that it's not smaller than iMin
// and not bigger than iMax.
function BoundInt(iMin: Integer; var iVal: Integer; iMax: Integer): Integer;
var
  iTmp: Integer;
begin
  if iMin > iMax then
  begin
    iTmp := iMin;
    iMin := iMax;
    iMax := iTmp;
  end;
  if iVal < iMin then iVal := iMin;
  if iVal > iMax then iVal := iMax;
  BoundInt := iVal;
end;
//------------------------------------------------------------------------------
// Correct the given byte value so that it's not smaller than iMin
// and not bigger than iMax.
function BoundByte(iMin: Byte; var iVal: Byte; iMax: Byte): Byte;
var
  iTmp: Byte;
begin
  if iMin > iMax then
  begin
    iTmp := iMin;
    iMin := iMax;
    iMax := iTmp;
  end;
  if iVal < iMin then iVal := iMin;
  if iVal > iMax then iVal := iMax;
  BoundByte := iVal;
end;
//------------------------------------------------------------------------------
// Correct the given double value so that it's not smaller than iMin
// and not bigger than iMax.
function BoundDbl(iMin: Double; var iVal: Double; iMax: Double): Double;
var
  iTmp: Double;
begin
  if iMin > iMax then
  begin
    iTmp := iMin;
    iMin := iMax;
    iMax := iTmp;
  end;
  if iVal < iMin then iVal := iMin;
  if iVal > iMax then iVal := iMax;
  BoundDbl := iVal;
end;
//------------------------------------------------------------------------------
// Missing Power function
function PowerXY(x, y: Extended): Extended;
begin
  PowerXY := exp(y*ln(x));
end;
//------------------------------------------------------------------------------
// A function to execute another application and wait for it to close before
// continuing.
// WindowState is one of the SW_xxx constants.  Look up ShowWindow in the API
// help for a list.
function ExecAndWait(const Filename, Params: String; WindowState: Word): Boolean;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  // Enclose filename in quotes to take care of long filenames with spaces.
  CmdLine := '"' + Filename + '"' + Params;

  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do
  begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;

  Result := CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE,
     CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, NIL,
     PChar(ExtractFilePath(Filename)), SUInfo, ProcInfo);

  // Wait for it to finish
  if Result then
  begin
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
    // Clean up the handles
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end;
end;
//------------------------------------------------------------------------------
// Another version of ExecAndWait.
// If the function is succesful it will return zero.
{function WinExecAndWait(Path: PChar; Visibility: Word): Word;
var
  InstanceID: THandle;
  Msg: TMsg;
begin
  InstanceID := WinExec(Path, Visibility);
  if InstanceID < 32 then // a value less than 32 indicates an Exec error
    WinExecAndWait := InstanceID
  else
    repeat
      while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
      begin
        if Msg.Message = WM_QUIT then Halt(Msg.WParam);
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    until GetModuleUsage(InstanceID) = 0;
  WinExecAndWait := 0;
end;}
//------------------------------------------------------------------------------
function MakeRect(pt1: TPoint; pt2: TPoint): TRect;
begin
  if pt1.x < pt2.x then
  begin
    Result.Left  := pt1.x;
    Result.Right := pt2.x;
  end
  else
  begin
    Result.Left  := pt2.x;
    Result.Right := pt1.x;
  end;

  if pt1.y < pt2.y then
  begin
    Result.Top    := pt1.y;
    Result.Bottom := pt2.y;
  end
  else
  begin
    Result.Top    := pt2.y;
    Result.Bottom := pt1.y;
  end;
end;
//------------------------------------------------------------------------------
// Round the given value to a number having specified decimals
function RoundIt(dVal: Double; iDgt: Integer): Double;
var
  OneHalf, Power10 : Extended;
begin
  if iDgt > 19 then iDgt := 19;
  if iDgt < 0 then iDgt := 0;
  Power10 := Power(10, iDgt);
  if dVal < 0 then
    OneHalf := -0.5
  else
    OneHalf := 0.5;
  RoundIt := (Trunc(dVal * Power10 + OneHalf)) / Power10;
end;
//------------------------------------------------------------------------------
// Missing iif() known from Visual Basic - return a string
function iifs(fCon: Boolean; sTrue, sFalse: String): String;
begin
  if fCon then
    iifs := sTrue
  else
    iifs := sFalse;
end;
//------------------------------------------------------------------------------
// Missing iif() known from Visual Basic - return an Integer
function iifi(fCon: Boolean; iTrue, iFalse: Integer): Integer;
begin
  if fCon then
    iifi := iTrue
  else
    iifi := iFalse;
end;
//------------------------------------------------------------------------------
// Convert the specified count of seconds to hours and minutes
procedure Sec2HorMin(totSec: Integer; var hor, min: Integer);
var
  dMin, dHor: Double;
begin
  dMin := totSec / 60.0;
  dHor := Trunc(dMin / 60.0);
  dMin := dMin - (dHor * 60.0);
  hor  := Trunc(dHor);
  min  := Trunc(dMin);
end;
//------------------------------------------------------------------------------
function RandomInRange(intLow, intHi: Integer): Integer;
var
  iTmp, iRS: Integer;
begin
  if intHi < intLow then
  begin
    iTmp   := intLow;
    intLow := intHi;
    intHi  := iTmp;
  end;
  iRS := intHi - intLow + 1;
  Result := Trunc(iRS * Random + intLow);
end;
//------------------------------------------------------------------------------
// Shows the contents of the ListView starting from the given item.
// By Marcin Bacik
procedure SetListViewTopItem(idx: Integer; LV: TListView);
 var
   iH, iVisibleCount: integer;
begin
  // No items, no work...
  if LV.Items.Count = 0 then Exit;

  // reduce idx if too big
  if idx > LV.Items.Count -1 then idx := LV.Items.Count -1;

  // item height
  iH := LV.Items[0].DisplayRect(drBounds).Bottom -  LV.Items[0].DisplayRect(drBounds).Top;

  // how many can fit?
  iVisibleCount := (LV.ClientRect.Bottom - LV.ClientRect.Top) div iH;

  // in ViewStyle=vsReport skip the header
  if LV.ShowColumnHeaders and (LV.ViewStyle = vsReport) then Dec( iVisibleCount );

  // in these views TopItem is not handled
  if (LV.ViewStyle in [vsSmallIcon, vsIcon]) then
  begin
   LV.Items[idx].MakeVisible(False);
   Exit;
  end;

 // jump to TopItem
 if LV.TopItem.Index > idx then
  LV.Items[idx].MakeVisible(False)
 else
  begin
   // topitem + visible count, -1, because our item must be visible
   idx := idx + (iVisibleCount -1);
   // don't go too far
   if idx > LV.Items.Count -1 then
     idx := LV.Items.Count-1;
   LV.Items[idx].MakeVisible(False);
  end;
end;
//------------------------------------------------------------------------------
// Returns a boolean indicating whether or not the user has
// Large Fonts enabled in Display Properties.
function HasLargeFonts: Boolean;
var
  liPPI  : longInt; // holds pixels per inch for canvas
  hCanvas: HDC;     // device context for current canvas
const
  LGFPPI = 120; // PPI when LgFonts active
begin
  hCanvas := getDC(0); // get current canvas
  try
    liPPI := getDeviceCaps(hCanvas, LOGPIXELSX);
  finally
    releaseDC(0, hCanvas); // prevent leaks
  end;
  Result := (liPPI = LGFPPI); // compare and return
end;
//------------------------------------------------------------------------------
// Conver the given RGB color to corresponding gray color
function RgbToGray(RGBColor : TColor): TColor;
var
  Gray: Byte;
begin
  Gray := Round((0.30 * GetRValue(RGBColor)) +
                (0.59 * GetGValue(RGBColor)) +
                (0.11 * GetBValue(RGBColor)));
  Result := RGB(Gray, Gray, Gray);
end;
//------------------------------------------------------------------------------
function ColorToHTMLString(Color: TColor): String;
var
  RGB: Longint;
begin
  RGB := ColorToRGB( Color );
  Result := '$' + IntToHex(GetRValue(RGB), 2) +
                  IntToHex(GetGValue(RGB), 2) +
                  IntToHex(GetBValue(RGB), 2);
end;
//------------------------------------------------------------------------------
function HTMLStringToColor(sColor: string): TColor;
begin
  sColor := DelLedChr(sColor, '$');
  Result := RGB(
              StrToInt('$'+Copy(sColor, 1, 2)),
              StrToInt('$'+Copy(sColor, 3, 2)),
              StrToInt('$'+Copy(sColor, 5, 2))
            );
end;
//------------------------------------------------------------------------------
// BIF_BROWSEFORCOMPUTER  Only returns computers. If the user selects anything other than a computer, the OK button is grayed.
// BIF_BROWSEFORPRINTER   Only returns printers. If the user selects anything other than a printer, the OK button is grayed.
// BIF_RETURNONLYFSDIRS   Only returns file system directories. If the user selects folders that aren't part of the file system, the OK button is grayed.
// BIF_BROWSEINCLUDEFILES The browse dialog will display files as well as folders
// Example:
//   var
//     sTitle, sFolder: String;
//     iFlag: Integer;
//   begin
//     sTitle := 'Choose a ' + rgBrowseFor.Items[rgBrowseFor.ItemIndex];
//     case rgBrowseFor.ItemIndex of
//       0:  iFlag :=  BIF_RETURNONLYFSDIRS;
//       1:  iFlag :=  BIF_BROWSEINCLUDEFILES;
//       2:  iFlag :=  BIF_BROWSEFORCOMPUTER;
//       3:  iFlag :=  BIF_BROWSEFORPRINTER;
//     end;
//     sFolder := BrowseDialog(Application.Handle, sTitle, iFlag);
//     if sFolder <> '' then
//       edSelected.text := sFolder
//     else
//       edSelected.text := 'Nothing selected';
function BrowseDialog(hdl: THandle; const Title: String; const Flag: Integer): String;
var
  lpItemID:    PItemIDList;
  BrowseInfo:  TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  TempPath:    array[0..MAX_PATH] of char;
begin
  Result := '';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := hdl;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;
//------------------------------------------------------------------------------
// http://www.experts-exchange.com/Programming/Programming_Languages/Delphi/Q_10707601.html
// callback function used in SelectDirectory to set the status text and choose an initial dir
function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
 Path: array[0..MAX_PATH] of Char;
begin
 case uMsg of
   BFFM_INITIALIZED:
     begin
       // Initialization has been done, now set our initial directory which is passed in lpData
       // (and set btw. the status text too).
       // Note: There's no need to cast lpData to a PChar since the following call needs a LPARAM parameter anyway.
       SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
       SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, lpData);
     end;
   BFFM_SELCHANGED: // Set the status window to the currently selected path.
     begin
       if SHGetPathFromIDList(Pointer(lParam), Path) then
         SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Integer(@Path));
     end;
 end;
 Result := 0;
end;
//----------------------------------------------------------------------------------------------------------------------
// Another browse-for-folder function with the ability to select an intial directory
// (other SelectDirectory functions are in FileCtrl.pas).
//
//  Example:
//    sInitDir := 'd:\Pgm\Borland';
//    if SelectDirectory(Application.Handle, 'Here caption', sInitDir, '', False, sRet) then
//      lblSelection.Caption := sRet;
function SelectDirectory(hdl: THandle; const Caption, InitialDir: String; const Root: WideString; ShowStatus: Boolean; out Directory: String): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList,
  ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  Windows: Pointer;
  Path: String;

begin
  Result := False;
  Directory := '';
  Path := InitialDir;
  if (Length(Path) > 0) and (Path[Length(Path)] = '\') then
    Delete(Path, Length(Path), 1);
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
     SHGetDesktopFolder(IDesktopFolder);
     IDesktopFolder.ParseDisplayName(hdl, nil, PWideChar(Root), Eaten, RootItemIDList, Flags);
     with BrowseInfo do
     begin
       hwndOwner := hdl;
       pidlRoot := RootItemIDList;
       pszDisplayName := Buffer;
       lpszTitle := PChar(Caption);
       ulFlags := BIF_RETURNONLYFSDIRS;
       if ShowStatus then ulFlags := ulFlags or BIF_STATUSTEXT;
       lParam := Integer(PChar(Path));
       lpfn := BrowseCallbackProc;
     end;

     // make the browser dialog modal
     Windows := DisableTaskWindows(hdl);
     try
       ItemIDList := ShBrowseForFolder(BrowseInfo);
     finally
       EnableTaskWindows(Windows);
     end;

     Result :=  ItemIDList <> nil;
     if Result then
     begin
       ShGetPathFromIDList(ItemIDList, Buffer);
       ShellMalloc.Free(ItemIDList);
       Directory := Buffer;
     end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;
//------------------------------------------------------------------------------
// Play one of standard Windows sounds:
// mtConfirmation, mtInformation, mtWarning, mtError, or mtCustom
procedure PlayBeep(ActionType: TMsgDlgType);
var mb: dWord;
begin
  case ActionType of
    mtInformation: mb := MB_ICONASTERISK; //SystemAsterisk
    mtWarning: mb := MB_ICONEXCLAMATION; //SystemExclamation
    mtError: mb := MB_ICONHAND; //SystemHand
    mtConfirmation: mb := MB_ICONQUESTION; //SystemQuestion
    mtCustom: mb := MB_OK; //SystemDefault
  else
    mb:= $0FFFFFFFF; //Standard beep using the computer speaker
  end;
  MessageBeep(mb);
end;
//------------------------------------------------------------------------------
// Get the text of the specified window
function GetWindowTextString(hWindow: hWnd): String;
var
  lpWinText: PChar;
begin
  Result := '';
  if (hWindow <> 0) then
  begin
    GetMem(lpWinText, 255);
    try
      GetWindowText(hWindow, lpWinText, 255);
      Result := lpWinText;
    finally
      FreeMem(lpWinText, sizeof(lpWinText^));
    end;
  end;
(*
//  CharArray: Array [0..MAX_PATH] of Char;
//  dwResult: DWORD;
//  if (SendMessageTimeout(hWindow, WM_GETTEXT, Sizeof(CharArray), integer(@CharArray), SMTO_ABORTIFHUNG or SMTO_BLOCK, 1000, dwResult) <> 0) then
//    Result := CharArray;
*)
end;
//------------------------------------------------------------------------------
// Get current date and time as a universal string
function GetDateTimeStamp: String;
begin
  Result := FormatDateTime('yyyy.mm.dd hh:nn:ss', Now());
end;
//------------------------------------------------------------------------------
// Get current date as a universal string
function GetDateStamp: String;
begin
  Result := FormatDateTime('yyyy.mm.dd', Now());
end;
//------------------------------------------------------------------------------
// Get current time as a universal string
function GetTimeStamp: String;
begin
  Result := FormatDateTime('hh:nn:ss', Now());
end;
//------------------------------------------------------------------------------
// Determine the Windows version.
// Return value: one of '3.1', '98', 'NT', '2000/NT', 'XP', or '?'.
//
// Example:
// var
//   vsnMaj, vsnMin, vsnBld: DWORD;
// begin
//   if (GetOsVersion(vsnMaj, vsnMin, vsnBld) = '98') then
//
function GetOSVersion(var MajorVersion, MinorVersion, Build: DWORD): String;
var
  VersionInfo: TOSVersionInfo;
begin
  Result := '?';

  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  with VersionInfo do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s:        Result := '3.1';
      VER_PLATFORM_WIN32_WINDOWS: Result := '98';
      VER_PLATFORM_WIN32_NT:
            begin
              Case dwMajorVersion of
                5: Result := '2000/NT';
              else
                Result := 'NT';
              end;
              if dwBuildNumber >= 2500 then
                Result := 'XP'
            end;
    end;
    MajorVersion := dwMajorVersion;
    MinorVersion := dwMinorVersion;
    Build := dwBuildNumber;
  end;
end;
//------------------------------------------------------------------------------
// Wait the specified amount of milliseconds
procedure DelayMSec(msec: Integer);
var
  tim1, tim2, freq: Int64;
begin
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(tim1); // get initial counter
  QueryPerformanceCounter(tim2); // current timer

  while ((1000*(tim2 - tim1) / freq) < msec) do // wait
  begin
    Application.ProcessMessages;
    QueryPerformanceCounter(tim2); // update current counter
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.


