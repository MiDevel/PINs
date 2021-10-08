{*****************************************************************************
 *  PINs
 *  u_Main.pas - Main form
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


unit u_Main;

// Revision history
// ================
//
// 4.60.0.91 Beta, 25 Apr 2004
//   - New security option "Super paste enabled when database is locked"
//   - New pasting option - paste clipboard contents. Useful especially for pasting text to Java applications that do not support clipboard handling.
//   - Option "Change file password (F4)" moved to more logical "Files" menu.
//   - New option "Lock the database now (F9)".
//   - Source code moved to Delphi 7.
//   - Due to moving to a new version of the compiler some components have been changed.
//
// 4.50.0.86, 19 Mar 2003
//   - Release version.
//   - Setup is available now in English, German, French, Polish, Russian and Spanish.
//
// 4.50.0.85 Beta RC1, 11 Mar 2003
//   - The program tells clipboard extenders to ignore passwords being
//     copied to the clipbaord (it works with ClipMate and Yankee Clipper,
//     possibly with other extenders too.)
//   - The password input dialog allows minimizing the application when the
//     database has been locked.
//   - New Info box popup menu offering two options: Copy selection to the
//     clipboard and Toggle activity of embedded hyperlinks.
//   - The comma (,) is accepted as a valid URL character.
//   - Passwords generator - repaired crash on empty set of user characters.
//   - Corrected problem with SuperPasting passwords containing '{' characters.
//   - Corrected problem with items with an empty category.
//   - The program is available now in 25 language versions.
//
// 4.50.0.82 Beta, 07 Sep 2002
//   - Export to text file - command configuration moved from general
//     settings to a dialog opened every time an export is being performed.
//   - Export to text file - possibility to specify which categories should be exported.
//   - Export to text file - possibility to export quoted texts.
//   - Import from text file - properly imports quoted texts.
//   - A new configuration option - expand all categories after opening a data file.
//
// 4.50.0.81 Beta, 03 Sep 2002
//   - Repaired saving of settings to an .ini file.
//   - Repaired problem with redundant error messages in SuperPaste option.
//
// 4.50.0.79 Beta, 12 Jul 2002
//   - Possibility to paste user name and/or password into external applications
//     using system-wide hotkeys.
//   - "SuperPaste" option allowing pasting into external applications
//     user-defined data, together with keys like <Tab> or <Enter>.
//   - Hotkeys editor.
//
// 4.20.0.76, 29 Jun 2002
//   - Release version.
//   - Multilingual setup.
//
// 4.20.0.75, 24 Jun 2002
//   - Exporting data to a text file requires entering a master password.
//   - Repaired data sorting.
//   - Possibility of reordering columns by dragging their headers
//     to new positions.
//   - A new security option: lock database after defined time of inactivity.
//   - A new configuration option - auto-expand focused categories.
//   - Possibility to run from a write-protected floppy or a CD-ROM.
//   - Repaired problem with importing the "More info" field.
//
// 4.10.0.70 Beta, 18 Jun 2002
//   - Full program documentation (Help).
//   - Accept files dropped on the program window.
//   - Possibility to define user names for all columns (except for the category).
//   - User names for columns can be switch off without erasing them.
//   - Possibility to define the action performed after double-clicking an item:
//     edit, copy password, copy user name or launch a URL.
//   - Passwords generator - a list of predefined popular templates.
//   - Passwords generator - when generating a number of passwords of the specified
//     length, a new template is generated for every password.
//   - Possibility of predefining user password templates in Passwords.tpl file.
//   - An option for automatic checking if a new version of the program
//     is available.
//   - "Stay on top" option, available in menu "View" (shortcut <Ctrl+F5>).
//   - Configurable count of Most Recently Used database files.
//   - Repaired bug causing dialog windows not to restore their positions on the screen.
//
// 4.00.0.62, 08 Mar 2002
//   - Release version
//
// 4.00.0.61 Beta 3, 03 Mar 2002
//   - New option - auto saving on exit.
//   - Improved detection of minimizing/restoring the application.
//   - Fixed saving/restoring of dates columns width.
//   - Found records are always put into the visible area of the grid.
//   - Extracted last(?) missing international messages.
//   - Fixed international tooltips for file selection buttons.
//
// 4.00.0.46 Beta 2, 16 Feb 2002
//   - Possibility to configure the visibility of particular columns.
//   - Removed dates panel introduced in Beta 1.
//   - Windows XP Themes Manager compatibility.
//   - Configurable sounds.
//   - Items editor - calendar gadget is properly initialized with date.
//   - Possibility to reenter invalid password up to 3 times.
//   - Copy user name/password to Clipboard available in the right-click menu.
//   - "Exit" button on the main toolbar
//   - Repaired sorting on dates columns.
//
// 4.00.0.43 Beta 1, 23 Jan 2002
//   - A new module for safe files and folders wiping. Support for Gutmann, DoD and custom wiping methods.
//   - A new column with custom user-defined data.
//   - A new security setting: Empty the clipboard on exit.
//   - A new security setting: Empty the clipboard on minimize.
//   - A new setting: Copy password to clipboard on URL launching.
//   - A new security setting: Create an additional archival copy of saved data files in a specified location.
//   - A new security setting: Lock the database on minimize.
//   - Password generator - possibility to use user-defined symbols.
//   - Password generator - possibility to generate any number of passwords at once.
//   - An option for clearing the clipboard (keyboard shortcut <Ctrl-Q>).
//   - Much improved loading time. Large data files load even 8 times faster.
//   - All program settings can be saved to and restored from an INI file.
//   - Configurable fonts, styles, and colors.
//   - A character map tool allowing entering characters not available on the keyboard.
//   - <Esc> key allows to close all dialog windows.
//   - Scroll tracking.
//   - Dialog windows restore their locations and sizes from previous sessions.
//   - When exporting data to text file, multiline descriptions are output in one line.
//   - <Tab> can be used as fields' separator in text files.
//   - Both date fields moved to the dates panel.
//   - Default open/save folder is the PINs installation folder.
//   - View menu - a new option for showing/hiding the dates panel.
//   - Removed bug causing wrong import of text files containing empty fields.
//   - Minor changes and improvements.
//
// 3.00, 16 Dec 2001
//   - URLs launching directly from the memo box.
//   - Option for hiding passwords in the editing dialog.
//   - Minimizing to system tray.
//   - Possibility to create multilingual versions.
//   - New function "Copy user name to clipboard" (<Ctrl+U>).
//   - New hyperlink types: file, telnet, news, gopher, weis, ftps.
//   - Resizable memo box.
//   - Improved logic of opening, appending and importing.
//   - Swapped the order of the "User" and "System" fields.
//   - Improved detection of URLs.
//   - URL found in the URL/Comment field is the default one for
//     launching with the <Ctrl+H> command.
//
// 2.50, 10 Oct 2001
//   - Embedded hyperlinks launching.
//   - Unlimited size of additional description.
//   - Remapped duplicated hotkeys.
//   - Minor improvements.
//
// 2.00, 01 Oct 2001
//   - First public version, using Blowfish crypting.
//
// 1.00, 12.05.1999
//   - Initial version, for internal use only.
//

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, ToolWin,
  Registry, IniFiles, ImgList, MRUFList, ElTree, ElHeader,
  uMWErrorLog, ShellAPI, u_Item, mwRichEdit, VerView, MsgDlg,
  ElXPThemedControl, CoolTrayIcon, ActnList;

const

  // status bar panels
  PNL_HINT   = 0;
  PNL_FILE   = 1;
  PNL_RECCNT = 2;

  // data file syntax version
  SAVVSN_STR = '4.50';
  SAVVSN_INT = 450;

  VSN_STD = True;
  VSN_HSO = False;

  S_DFTSUPERPASTE = '$USER{tab}$PASS{enter}';

  DFT_BCKCLO = $00CBF1FE;

  // DblClkAction
  DCA_EDIT = 1;
  DCA_COPYPASS = 2;
  DCA_COPYUSER = 3;
  DCA_LNCHURL = 4;

resourcestring
  rsFldCat='Category';
  rsFldSys='System';
  rsFldUsr='User';
  rsFldPsw='Password';
  rsFldCmm='URL/Comments';
  rsFldCst='Custom';
  rsFldInf='More info';
  rsFldSta='Start date';
  rsFldExp='Expires';
  rsChgPswOfFile='Change password|Change password of [%s] data file';
  rsOpenFilter='Password files (*.pins, *.pin, *.txt, *.csv, *.sdf)|*.pins;*.txt;*.csv;*.sdf|PINs files (*.pins, *.pin)|*.pins;*.pin|Text files (*.txt, *.csv, *.sdf)|*.txt;*.csv;*.sdf|All files (*.*)|*.*';
  rsSaveFilter='PINs files (*.pins)|*.pins|Text files (*.txt)|*.txt';
  rsDataNotSave='Data not saved. Save?';
  rsInvPassword='Invalid password!';
  rsInvPINSFile='It''s not a valid PINS data file!';
  rsFileXNotFound='File*|*[%s] not found!';
  rsDeleteRec='Delete current record?';
  rsEnterPass4NewFile='Enter an access password for this newly created data file:';
  rsRetypeNewPass='Retype your new password:';
  rsPassDontMatch='Both versions of the password do not match!';
  rsTypeOldPass='Type your old password:';
  rsTypeNewPass='Type your new password:';
  rsTextXNotFound='Text [%s] was not found';
  rsRecCount='Records: %d';
  rsLaunchURLs='Launch URLs';
  rsLaunch='Launch';
  rsNoname='Noname';
  rsRenCategory='Rename category';
  rsEnterNewCategory='Enter a new name for the category';
  rsNoCategSelected='No category selected!';
  rsDelWholeCateg='Remove the whole category and its items?';
  rsEnterFilePassword='Enter access password for file %s:';
  rsDataNotSaveMinim='Data not saved. Save before minimizing?';
  rsAreYouSure='Are you sure?';
  rsErrDataSave='Error saving the data file*|*[%s]*|**|*System report: %s';
  rsErrSaveToArchive='Error making an archival copy*|*[%s]';
  rsDataLockedPassword='Database is locked. Enter the password to unlock it.';
  rsBadPasswordTryAgain='Invalid password, try again...';
  rsBadPasswordClose='3 bad attempts, terminating the application...';
  rsManyInstances='Multiple instances of this version of PINs are currently running. Please be aware that the last instance to save any settings will overwrite any previously saved settings from other instances.';
  rsDontShowMessage='Don''t show this message again';
  rsNever='Never';
  rsBtnAccept='&Accept';
  rsBtnClose='&Close';
  rsBtnYes='&Yes';
  rsBtnNo='&No';
  rsBtnOk='&OK';
  rsBtnCancel='&Cancel';
  rsBtnAbort='&Abort';
  rsBtnRetry='&Retry';
  rsBtnIgnore='&Ignore';
  rsBtnAll='&All';
  rsBtnNoToAll='N&o to All';
  rsBtnYesToAll='Yes to &All';
  rsBtnHelp='Help';
  rsMsgWarning='Warning';
  rsMsgError='Error';
  rsMsgInformation='Information';
  rsMsgConfirm='Confirm';
  rsReopen='&Reopen';
  rsNotSecureEnterPass='Insecure operation. Please enter the password to proceed';
  rsIfReviewExpPass='Some passwords have expired. Do you want to review them?';
//ilMiscStr(

const
  S_EXTENSION = 'pins';

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    optExit: TMenuItem;
    mnuHelp: TMenuItem;
    optHelpAbout: TMenuItem;
    optFileOpen: TMenuItem;
    optFileSave: TMenuItem;
    tlbMain: TToolBar;
    tbtnSave: TToolButton;
    tbtnLoad: TToolButton;
    ToolButton1: TToolButton;
    tbtnEdit: TToolButton;
    mnuRecord: TMenuItem;
    optRecordEdit: TMenuItem;
    optRecordAdd: TMenuItem;
    optRecordDelete: TMenuItem;
    dlgOpen: TOpenDialog;
    mnuOptions: TMenuItem;
    optChangePsw: TMenuItem;
    tbtnNew: TToolButton;
    ToolButton2: TToolButton;
    tbtnAdd: TToolButton;
    tbtnDel: TToolButton;
    optFileNew: TMenuItem;
    sepFile2: TMenuItem;
    dlgSave: TSaveDialog;
    optFileSaveAs: TMenuItem;
    optToolsOptions: TMenuItem;
    sepTools1: TMenuItem;
    tbtnCopy: TToolButton;
    optRecordCopy: TMenuItem;
    N1: TMenuItem;
    MRUFileList: TMRUFileList;
    tbtnSettings: TToolButton;
    tbtnClipCopyPsw: TToolButton;
    ToolButton4: TToolButton;
    optClipCopyPsw: TMenuItem;
    tbtnSaveAs: TToolButton;
    FindDialog: TFindDialog;
    mnuSearch: TMenuItem;
    optFind: TMenuItem;
    optFindNext: TMenuItem;
    ToolButton3: TToolButton;
    optExportToText: TMenuItem;
    grdTree: TElTree;
    pnlBottom: TPanel;
    lblVsn: TVersionView;
    tbtnFind: TToolButton;
    tbtnFindNext: TToolButton;
    mnuView: TMenuItem;
    optExpandAll: TMenuItem;
    optCollapseAll: TMenuItem;
    tbtnExpandAll: TToolButton;
    tbtnCollapseAll: TToolButton;
    ToolButton8: TToolButton;
    optImportFromText: TMenuItem;
    outtbtnChangePsw: TToolButton;
    tbtnGenPassword: TToolButton;
    optGenPassword: TMenuItem;
    N2: TMenuItem;
    optFileAppend: TMenuItem;
    popupGrid: TPopupMenu;
    N3: TMenuItem;
    optCatRemove: TMenuItem;
    optCatRename: TMenuItem;
    outpopoptCatRename: TMenuItem;
    outpopoptCatRemove: TMenuItem;
    outpopoptRecordEdit: TMenuItem;
    outpopoptRecordAdd: TMenuItem;
    outpopoptRecordCopy: TMenuItem;
    outpopoptRecordDelete: TMenuItem;
    N4: TMenuItem;
    tbtnHyperlink: TToolButton;
    ToolButton6: TToolButton;
    optHyperlink: TMenuItem;
    popHyperlinks: TPopupMenu;
    popoptCancel: TMenuItem;
    lisTmp: TListBox;
    popTray: TPopupMenu;
    popoptRestore: TMenuItem;
    N5: TMenuItem;
    popoptExit: TMenuItem;
    Splitter1: TSplitter;
    sttBar: TStatusBar;
    fldInfo: TmwRichEdit;
    optViewTlbMain: TMenuItem;
    N6: TMenuItem;
    optViewSttBar: TMenuItem;
    optViewAll: TMenuItem;
    optViewNone: TMenuItem;
    N7: TMenuItem;
    optLanguage: TMenuItem;
    optClipCopyUsr: TMenuItem;
    N8: TMenuItem;
    tbtnClipCopyUsr: TToolButton;
    optClipEmpty: TMenuItem;
    btnClipEmpty: TToolButton;
    optCharMap: TMenuItem;
    optFileWiping: TMenuItem;
    N9: TMenuItem;
    tbtnFileWiping: TToolButton;
    ToolButton7: TToolButton;
    imgList: TImageList;
    outpopoptClipCopyUsr: TMenuItem;
    outpopoptClipCopyPsw: TMenuItem;
    N10: TMenuItem;
    btnExit: TToolButton;
    dlgMessage: TMessageDlg;
    optVisitWebPage: TMenuItem;
    N11: TMenuItem;
    optHelp: TMenuItem;
    N12: TMenuItem;
    optCheckNewVersion: TMenuItem;
    N13: TMenuItem;
    optStayOnTop: TMenuItem;
    optDonate: TMenuItem;
    tmrInactivity: TTimer;
    optHotkeys: TMenuItem;
    tmrPaste: TTimer;
    N14: TMenuItem;
    optExpiredPasswords: TMenuItem;
    outoptLockDatabase: TMenuItem;
    popupInfo: TPopupMenu;
    popoptCopySelection: TMenuItem;
    N15: TMenuItem;
    popoptActiveHyperlinks: TMenuItem;
    outTrayIcon: TCoolTrayIcon;
    outbtnLockDatabase: TToolButton;
    actList: TActionList;
    acLockDatabase: TAction;
    procedure optFileSaveClick(Sender: TObject);
    procedure optFileOpenClick(Sender: TObject);
    procedure optExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure optRecordEditClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure optChangePswClick(Sender: TObject);
    procedure tbtnNewClick(Sender: TObject);
    procedure optHelpAboutClick(Sender: TObject);
    procedure optFileSaveAsClick(Sender: TObject);
    procedure optToolsOptionsClick(Sender: TObject);
    procedure MRUFileListMRUItemClick(Sender: TObject; AFilename: String);
    procedure optClipCopyPswClick(Sender: TObject);
    procedure optFindClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure optFindNextClick(Sender: TObject);
    procedure optExportToTextClick(Sender: TObject);
    procedure pnlBottomResize(Sender: TObject);
    procedure grdTreeItemFocused(Sender: TObject);
    procedure grdTreeDblClick(Sender: TObject);
    procedure optExpandAllClick(Sender: TObject);
    procedure optCollapseAllClick(Sender: TObject);
    procedure optImportFromTextClick(Sender: TObject);
    procedure optGenPasswordClick(Sender: TObject);
    procedure optFileAppendClick(Sender: TObject);
    procedure optCatRenameClick(Sender: TObject);
    procedure optCatRemoveClick(Sender: TObject);
    procedure optRecordAddClick(Sender: TObject);
    procedure optRecordCopyClick(Sender: TObject);
    procedure optRecordDeleteClick(Sender: TObject);
    procedure optHyperlinkClick(Sender: TObject);
    procedure popoptRestoreClick(Sender: TObject);
    procedure fldInfoHyperlinkClicked(sURL: String);
    procedure FormResize(Sender: TObject);
    procedure optViewTlbMainClick(Sender: TObject);
    procedure optViewSttBarClick(Sender: TObject);
    procedure optViewAllClick(Sender: TObject);
    procedure optViewNoneClick(Sender: TObject);
    procedure optLanguageClick(Sender: TObject);
    procedure optClipCopyUsrClick(Sender: TObject);
    procedure optClipEmptyClick(Sender: TObject);
    procedure optCharMapClick(Sender: TObject);
    procedure optFileWipingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure grdTreeCompareItems(Sender: TObject; Item1, Item2: TElTreeItem; var res: Integer);
    procedure optVisitWebPageClick(Sender: TObject);
    procedure optHelpClick(Sender: TObject);
    procedure optCheckNewVersionClick(Sender: TObject);
    procedure optStayOnTopClick(Sender: TObject);
    procedure optDonateClick(Sender: TObject);
    procedure grdTreeHeaderColumnClick(Sender: TObject; SectionIndex: Integer);
    procedure tmrInactivityTimer(Sender: TObject);
    procedure optHotkeysClick(Sender: TObject);
    procedure tmrPasteTimer(Sender: TObject);
    procedure optExpiredPasswordsClick(Sender: TObject);
    procedure popoptCopySelectionClick(Sender: TObject);
    procedure popoptActiveHyperlinksClick(Sender: TObject);
    procedure popupInfoPopup(Sender: TObject);
    procedure outTrayIconClick(Sender: TObject);
    procedure outTrayIconMinimizeToTray(Sender: TObject);
    procedure acLockDatabaseExecute(Sender: TObject);

  private { Private declarations }
    m_IsBeta: Boolean;          // is it a Beta version?
    m_regFile: TRegIniFile;     // registry handling
    m_iniFile: TMemIniFile;     // INI file handling
    m_SoundOn: Boolean;         // sound signals?
    m_SaveSettingsToRegistry: Boolean;
    m_sPgmDir: String;          // our home directory
    m_fFirstRun: Boolean;       // first run of this application?
    m_sFullName: String;        // executable file specification
    m_sParameters: String;      // command line parameters
    m_sDataFile: String;        // opened file name
    m_sPassword: String;        // opened file password
    m_DataDirty: Boolean;       // are data modified?
    m_WarnKeys: Boolean;        // warn at start about conflicting keys
    m_CheckExpDate: Boolean;    // check expiration dates after loading
    m_ExpDateDays: Integer;     // expiration dates - days ahead
    m_ShowPswCol: Boolean;      // show passwords in the table?
    m_ShowPswFld: Boolean;      // show passwords in the editing dialog?
    m_ReloadLast: Boolean;      // reload last file at startup?
    m_AddSamples: Boolean;      // add sample records to new docs?
    m_LastSearch: String;       // the last searched text
    m_GridLinesHoriz: Boolean;  // show horizontal grid lines?
    m_GridLinesVert: Boolean;   // show vertical grid lines?
    m_fMakeBAK: Boolean;        // make .BAK copies?
    m_ClipClrExit: Boolean;     // clear the clipboard on exit
    m_ClipClrMini: Boolean;     // clear the clipboard on minimize
    m_AutoCopyPsw: Boolean;     // copy password to Clipboard on URL lanunch
    m_SaveOnMinim: Boolean;     // prompt to save any unsaved changes on minimize
    m_AutoSave: Boolean;        // auto save on exit
    m_ExportSep: String;        // separator used in exporting
    m_ExpQuoteTexts: Boolean;   // Quote all texts in exported data?
    m_sLanguage: String;        // language file
    m_sCrrItemPsw: String;      // the password found in the currently active item
    m_SaveArchCopy: Boolean;    // make additional archival copies?
    m_SaveArchCopyTo: String;   // archival copies folder
    m_LockOnMinimize: Boolean;  // lock database on minimize
    m_DatabaseLocked: Boolean;  // is database locked?
    m_InetUseProxy: Boolean;
    m_InetProxyAddress: String;
    m_InetProxyPort: Integer;
    m_UserColNames: Boolean; // use user names for columns
    m_DblClkAction: Integer; // action performed after double-clicking an item
    m_iNumMRUFil: Integer;   // count of recent files
    m_MaxInactivity: Integer;// inactivity time (seconds) to lock the database
    m_Inactivity: Integer;   // inactivity time (seconds)
    m_StartExpanded: Boolean; // expand all categories after opening data files
    m_PasteInLocked: Boolean; // is Super paste active in locked mode?

    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    procedure OwnAppMessageHandler(var Msg: TMsg; var Handled: Boolean);
    procedure UpdateFormTitle;
    procedure SetDataDirty(fDirty: Boolean);
    procedure SetDataFile(sName: String);
    procedure SetShowPswCol(fShow: Boolean);
    procedure DoSavePINFile(const sName: String);
    function  DoOpenFile(filNam: String; fAppend: Boolean): Boolean;
    function  DoOpenPINFile(filNam: String; fAppend: Boolean): Boolean;
    function  DoOpenTextFile(filNam: String; fAppend: Boolean): Boolean;
    procedure UpdateGridItem(TSI: TElTreeItem; item: TOneItem);
    procedure SetGridLinesHoriz(fShow: Boolean);
    procedure SetGridLinesVert(fShow: Boolean);
    function  GridDataAsStrings(var strLis: TStringList): Boolean;
    function  SelRowAsItem(var itm: TOneItem): Boolean;
    procedure popOneHyperlinkClick(Sender: TObject);
    function  GetUseTray: Boolean;
    procedure SetUseTray(fUse: Boolean);
    function  GetActiveHyperlinks: Boolean;
    procedure SetActiveHyperlinks(fAct: Boolean);
    procedure SetDatabaseLocked(fLocked: Boolean);
    procedure SetLanguage(sLanguage: String);
    procedure SetNumMRUFil(iNum: Integer);
    procedure SetFormOnTop(onTop: Boolean);
    function  GetAutoExpand: Boolean;
    procedure SetAutoExpand(fAuto: Boolean);
    procedure DoCheckExpDate;

  published { Published declarations }
    property IsBeta        : Boolean read m_IsBeta;    // Is it a Beta-test version?
    property PgmDir        : String  read m_sPgmDir;   // our home directory
    property SoundOn       : Boolean read m_SoundOn        write m_SoundOn; // sound signals?
    property DataFile      : String  read m_sDataFile      write SetDataFile;
    property Password      : String  read m_sPassword      write m_sPassword;
    property DataDirty     : Boolean read m_DataDirty      write SetDataDirty;
    property WarnKeys      : Boolean read m_WarnKeys       write m_WarnKeys;    // warn at start about conflicting keys
    property CheckExpDate  : Boolean read m_CheckExpDate   write m_CheckExpDate;// check expiration dates after loading
    property ExpDateDays   : Integer read m_ExpDateDays    write m_ExpDateDays; // expiration dates - days ahead
    property ShowPswCol    : Boolean read m_ShowPswCol     write SetShowPswCol;
    property ShowPswFld    : Boolean read m_ShowPswFld     write m_ShowPswFld;
    property ReloadLast    : Boolean read m_ReloadLast     write m_ReloadLast;
    property AddSamples    : Boolean read m_AddSamples     write m_AddSamples;  // add sample records to new docs?
    property LastSearch    : String  read m_LastSearch     write m_LastSearch;
    property GridLinesHoriz: Boolean read m_GridLinesHoriz write SetGridLinesHoriz; // show horizontal grid lines?
    property GridLinesVert : Boolean read m_GridLinesVert  write SetGridLinesVert; // show vertical grid lines?
    property ActiveHyperlinks: Boolean read GetActiveHyperlinks write SetActiveHyperlinks; // active hyperlinks in memo box?
    property ClipClrExit   : Boolean read m_ClipClrExit    write m_ClipClrExit; // clear the clipboard on exit
    property ClipClrMini   : Boolean read m_ClipClrMini    write m_ClipClrMini; // clear the clipboard on minimize
    property AutoCopyPsw   : Boolean read m_AutoCopyPsw    write m_AutoCopyPsw; // copy password to Clipboard on URL lanunch
    property SaveOnMinim   : Boolean read m_SaveOnMinim    write m_SaveOnMinim; // prompt to save any unsaved changes on minimize
    property AutoSave      : Boolean read m_AutoSave       write m_AutoSave;    // auto save on exit
    property ExportSep     : String  read m_ExportSep      write m_ExportSep;   // separator used in exporting
    property ExpQuoteTexts : Boolean read m_ExpQuoteTexts  write m_ExpQuoteTexts; // Quote all texts in exported data?
    property UseTray       : Boolean read GetUseTray       write SetUseTray;
    property Language      : String  read m_sLanguage      write SetLanguage; // language file
    property CrrItemPsw    : String  read m_sCrrItemPsw    write m_sCrrItemPsw;
    property MakeBAK       : Boolean read m_fMakeBAK       write m_fMakeBAK; // make .BAK copies?
    property SaveArchCopy  : Boolean read m_SaveArchCopy   write m_SaveArchCopy; // make additional archival copies?
    property SaveArchCopyTo: String  read m_SaveArchCopyTo write m_SaveArchCopyTo; // archival copies folder
    property LockOnMinimize: Boolean read m_LockOnMinimize write m_LockOnMinimize; // lock database on minimize
    property DatabaseLocked: Boolean read m_DatabaseLocked write SetDatabaseLocked; // is database locked?
    property UserColNames  : Boolean read m_UserColNames   write m_UserColNames; // use user names for columns
    property DblClkAction  : Integer read m_DblClkAction   write m_DblClkAction; // action performed after double-clicking an item
    property MaxInactivity : Integer read m_MaxInactivity  write m_MaxInactivity; // inactivity time (seconds) to lock the database
    property StartExpanded : Boolean read m_StartExpanded  write m_StartExpanded; // expand all categories after opening data files
    property PasteInLocked : Boolean read m_PasteInLocked  write m_PasteInLocked; // is Super paste active in locked mode?

    property NumMRUFiles   : Integer read m_iNumMRUFil     write SetNumMRUFil;
    property AutoExpand    : Boolean read GetAutoExpand    write SetAutoExpand; // auto-expand focused items

    property InetUseProxy       : Boolean read m_InetUseProxy       write m_InetUseProxy;
    property InetProxyAddress   : String  read m_InetProxyAddress   write m_InetProxyAddress;
    property InetProxyPort      : Integer read m_InetProxyPort      write m_InetProxyPort;

  public { Public declarations }
    function  ReadBool(sSec, sKey: String; fDefault: Boolean): Boolean;
    function  ReadInteger(sSec, sKey: String; nDefault: Integer): Integer;
    function  ReadString(sSec, sKey: String; sDefault: String): String;
    procedure WriteBool(sSec, sKey: String; fVal: Boolean);
    procedure WriteInteger(sSec, sKey: String; nVal: Integer);
    procedure WriteString(sSec, sKey: String; sVal: String);
    procedure TranslateUI;
    procedure LocalizeMessageDlg(md: TMessageDlg);
    function  CstMessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word;
    procedure CstShowMessage(const Msg: string);
    procedure UpdateUI;   // update all user interface elements
    procedure AddGridItem(item: TOneItem; fSetFocus, fSpeedOpt: Boolean); overload;
    procedure AddGridItem(sBff: String; fSetFocus, fSpeedOpt: Boolean); overload;
    function  ItemsCount: Integer;
    function  GridRowAsItem(idx: Integer; var itm: TOneItem): Boolean;
    function  GetNewPassword: String;
    function  ConfirmPassword(sPassword: String; sFstMessage: String): Integer;
    function  CheckFileType(const sPath: String): Integer;
    procedure ShowHelpPage(sTopic: String; hwndCaller: HWND);
    procedure SaveWindowPosition(frm: TForm; fSaveSize: Boolean);
    procedure ReadWindowPosition(frm: TForm; fRestoreSize: Boolean);
    function  SepToStr(sep: String): String;
    procedure ActivityDetected;
    procedure DoPutPasswordOnClipboard(sPass: String);
  end;

var
  frmMain: TfrmMain;
  fInitDone: Boolean;
  sNoname: String;
  CF_CLIPBOARD_VIEWER_IGNORE: Word;  // our private clipboard format
//  ErrLog: TMWErrorLog;

implementation

uses
  u_Password, u_About, u_Options, Clipbrd, FileCtrl, HtmlHlp,
  uMWTools, uMWStrings, uMWForms, u_Blowfish, u_GenPassword, u_Sndkey32,
  u_Import, u_Language, IniLang, u_CharMap, u_FileWipe, u_Calendar,
  u_Sounds, u_Columns, u_HotKeys, u_ProxySettings,
  u_ExpiredDates, u_Export, u_SoftwareCheck, uMWInternet;

{$R *.DFM}
{$R WindowsXP.res}
{$R PINsRes.res}

//------------------------------------------------------------------------------
procedure TfrmMain.FormCreate(Sender: TObject);
var
  i, iTmp: Integer;
  fnt: TFont;
  iSnd: TSounds;
begin
  m_IsBeta  := True; // Beta?
  fInitDone := False;
  m_sFullName := ParamStr(0); // executable file specification
  m_sPgmDir := ExtractFilePath(ParamStr(0)); // our home directory, with '\' at the end
  m_sParameters := Trim(ParamStr(1));

//  ErrLog     := TMWErrorLog.Create;
//  ErrLog.Init(MakePath(PgmDir, 'PINs.log'), True);
//  ErrLog.Activate(True);
//  ErrLog.TimeStamp := False;

  DatabaseLocked := False;
  Application.HelpFile := MakePath(m_sPgmDir, 'PINs.chm');
  InitializeSounds; // initialize the system of sounds
  CF_CLIPBOARD_VIEWER_IGNORE := RegisterClipboardFormat('Clipboard Viewer Ignore'); // spelling counts - must be exact!

  // let Windows know that we accept dropped files
  DragAcceptFiles(frmMain.Handle, True);

  // columns
  for i := COL_CAT to COL_IDXLAST do
    GlbColumns[i] := TOneColumn.Create;
  GlbColumns[COL_CAT].Idf := 'Cat';
  GlbColumns[COL_SYS].Idf := 'Sys';
  GlbColumns[COL_USR].Idf := 'Usr';
  GlbColumns[COL_PSW].Idf := 'Psw';
  GlbColumns[COL_CMM].Idf := 'Cmm';
  GlbColumns[COL_CST].Idf := 'Cst';
  GlbColumns[COL_STA].Idf := 'Sta';
  GlbColumns[COL_EXP].Idf := 'Exp';
  GlbColumns[COL_INF].Idf := 'Inf';
  GlbColumns[COL_SPS].Idf := 'Sps';

  m_iniFile := TMemIniFile.Create(MakePath(m_sPgmDir, 'PINs.ini'));

  // this setting must be always read from an INI file
  // starting with v.4.60 storing settings in an ini file is recommended
  m_SaveSettingsToRegistry := m_iniFile.ReadBool('General', 'SaveSettingsToRegistry', False);

  if m_SaveSettingsToRegistry then
  begin
    m_regFile := TRegIniFile.Create('SOFTWARE\MirWoj\PINs2');
    MRUFileList.AutoSaveName := '\Software\MirWoj\PINs2';
  end;

  outTrayIcon.Hint := 'PINs';

  Randomize;
  grdTree.Align := alClient;
  grdTree.VertScrollBarStyles.ShowTrackHint := False;

  if not m_SaveSettingsToRegistry then
    MRUFileList.AutoSave := False;

  // settings
  m_fFirstRun      := ReadBool('Options', 'FirstRun', True);// first run of this application
  ReloadLast       := ReadBool('Options', 'ReloadLast', True);
  AddSamples       := ReadBool('Options', 'AddSamples', True); // add sample records to new docs
  MakeBAK          := ReadBool('Options', 'MakeBAK', True); // make .BAK copies
  UseTray          := ReadBool('Options', 'UseTray', True);  // minimize to tray
  ActiveHyperlinks := ReadBool('Options', 'ActiveHyperlinks', True);  // active hyperlinks
  Language         := ReadString('Options', 'Language', '???');
  AutoCopyPsw      := ReadBool('Options', 'AutoCopyPsw', False); // copy password to Clipboard on URL lanunch
  SaveOnMinim      := ReadBool('Options', 'SaveOnMinim', True); // prompt to save any unsaved changes on minimize
  AutoSave         := ReadBool('Options', 'AutoSave', False); // auto save on exit
  SaveArchCopy     := ReadBool('Options', 'SaveArchCopy', False); // make additional archival copies?
  SaveArchCopyTo   := ReadString('Options', 'SaveArchCopyTo', 'a:\'); // archival copies folder
  DblClkAction     := ReadInteger('Options', 'DblClkAction', DCA_EDIT); // action performed after double-clicking an item
  NumMRUFiles      := ReadInteger('Options', 'NumRecentFiles', 10); // number of recent files
  SetFormOnTop(ReadBool('Options', 'StayOnTop', False));  // stay on top
  AutoExpand       := ReadBool('Options', 'AutoExpand', False); // auto-expand focused items
  WarnKeys         := ReadBool('Options', 'WarnKeys', True); // warn at start about conflicting keys
  CheckExpDate     := ReadBool('Options', 'CheckExpDate', True);  // check expiration dates after loading
  ExpDateDays      := ReadInteger('Options', 'ExpDateDays', 7); // expiration dates - days ahead
  StartExpanded    := ReadBool('Options', 'StartExpanded', False); // expand all categories?

  // security
  ShowPswCol     := ReadBool('Security', 'ShowPswCol', False);
  ShowPswFld     := ReadBool('Security', 'ShowPswFld', True);
  ClipClrExit    := ReadBool('Security', 'ClipClrExit', True); // clear the clipboard on exit
  ClipClrMini    := ReadBool('Security', 'ClipClrMini', True); // clear the clipboard on minimize
  LockOnMinimize := ReadBool('Security', 'LockOnMinimize', True); // lock database on minimize
  MaxInactivity  := ReadInteger('Security', 'MaxInactivity', 300); // inactivity time (seconds) to lock the database
  PasteInLocked  := ReadBool('Security', 'PasteInLocked', True);  // is Super paste active in locked mode?

  // UI
  sttBar.Visible  := ReadBool('UI', 'ViewSttBar'   , True);
  tlbMain.Visible := ReadBool('UI', 'ViewToolbMain', True);

  sNoname := ilMiscStr(rsNoname, 'rsNoname') + '.' + S_EXTENSION;

  // defaults
  DataFile   := ReadString('Defaults', 'LastFile', sNoname);
  LastSearch := ReadString('Defaults', 'LastSearch', '');

  // form size & placement
  frmMain.Width   := ReadInteger('Layout', 'FormWidth',   640);
  frmMain.Height  := ReadInteger('Layout', 'FormHeight',  480);
  frmMain.Top     := ReadInteger('Layout', 'FormTop',     0);
  frmMain.Left    := ReadInteger('Layout', 'FormLeft',    0);
  iTmp            := ReadInteger('Layout', 'WindowState', 1);
  case iTmp of
    1: WindowState := wsNormal;
    2: WindowState := wsMaximized;
  else
    WindowState := wsNormal;
  end;
  pnlBottom.Height := ReadInteger('Layout', 'PnlBottomHeight', 86);

  GridLinesHoriz    := ReadBool('Layout', 'GridLinesHoriz', True);
  GridLinesVert     := ReadBool('Layout', 'GridLinesVert', False);
  grdTree.BkColor   := ReadInteger('Layout', 'GridBackColor', DFT_BCKCLO); // nice background color
  grdTree.TextColor := ReadInteger('Layout', 'GridTextColor', clWindowText);
  fldInfo.Color     := ReadInteger('Layout', 'InfoBackColor', DFT_BCKCLO); // nice background color
  fldInfo.HyperlinkColor := ReadInteger('Layout', 'HyperlinkColor', clBlue); // URL color

  fnt := TFont.Create;
  StringToFont(ReadString('Layout', 'GridFont', FontToString(grdTree.Font)), fnt);
  grdTree.Font.Assign(fnt);
  StringToFont(ReadString('Layout', 'InfoFont', FontToString(fldInfo.Font)), fnt);
  fldInfo.Font.Assign(fnt);

  // Import/export
  for i := COL_IDXFIRST to COL_IDXLAST do
  begin
    GlbColumns[i].ToExport := ReadBool('Export', 'Exp' + GlbColumns[i].Idf, True);
  end;
  ExportSep     := ReadString('Export', 'Separator', ';');
  ExpQuoteTexts := ReadBool('Export', 'QuoteTexts', False); // don't quote texts

  // Columns
  grdTree.HeaderSections.SectionsOrder := ReadString('Columns', 'SectionsOrder', '');
  grdTree.HeaderSections[COL_CAT].Visible := True;  // must be visible
  grdTree.HeaderSections[COL_INF].Visible := False; // can't be visible
  grdTree.HeaderSections[COL_SPS].Visible := False; // can't be visible

  for i := COL_IDXFIRST to COL_IDXLAST do
  begin
    GlbColumns[i].UserName := ReadString('Columns', 'UserName_' + GlbColumns[i].Idf, '');
  end;
  UserColNames := ReadBool('Columns', 'UserColNames', True); // use user names for columns


  // sounds
  SoundOn := ReadBool('Sounds', 'SoundOn', True); // sound signals?
  for iSnd := FirstSound to LastSound do
    SoundsAry[iSnd].SetFromString(ReadString('Sounds', SoundsAry[iSnd].Sound, SoundsAry[iSnd].GetAsString));


  // Hotkeys
  HKSPaste.SetFromString(ReadString('Hotkeys', 'SPaste', DFTKEY_SPASTE));
  HKUPaste.SetFromString(ReadString('Hotkeys', 'UPaste', DFTKEY_UPASTE));
  HKPPaste.SetFromString(ReadString('Hotkeys', 'PPaste', DFTKEY_PPASTE));
  HKCPaste.SetFromString(ReadString('Hotkeys', 'CPaste', DFTKEY_CPASTE));

  // .pins extension
  if m_SaveSettingsToRegistry then
  begin
    // update .pins extension registration
    RegFileType(S_EXTENSION, 'PINs.Document', Application.ExeName, 0);
  end;


  RegisterHotkeys(WarnKeys); // register system-wide hotkeys

  Application.OnMessage := OwnAppMessageHandler;
end;
//------------------------------------------------------------------------------
// Open the data file on the first activation
procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if Language = '???' then // first run - ask for the language
    if DirectoryExists(MakePath(PgmDir, S_LANGDIR)) then
      optLanguageClick(nil);

  UpdateUI; // update all user interface elements
  if not fInitDone then
  begin
    fInitDone := TRUE;

    if Length(m_sParameters) > 0 then // data file passed as a parameter
    begin
      DoOpenFile(m_sParameters, False);
    end
    else
    begin // no parameters, open last edited
      if m_fFirstRun then
      begin
        tbtnNewClick(nil); // start a new data file
      end
      else
      begin
        if ReloadLast and (UpperCase(DataFile) <> UpperCase(sNoname)) then
        begin
          if Length(ExtractFilePath(DataFile)) > 0 then // has path
          begin
            if not DoOpenPINFile(DataFile, False) then
              DataFile := sNoname;
          end
          else // no path, try from home folder
          begin
            try
              if not DoOpenPINFile(MakePath(m_sPgmDir, DataFile), False) then
                DataFile := sNoname;
            except
            end;
          end;
        end;
      end;
    end;
  end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Close
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;
//------------------------------------------------------------------------------
// The application is about to terminate
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i, asw: Integer;
  iSnd: TSounds;
  hFile: Integer;
  canSave: Boolean;
begin
  if ClipClrExit then // clear the clipboard on exit?
    Clipboard.Clear;

  // perform auto saving
  if AutoSave and DataDirty then
    optFileSaveClick(Nil);

  // allow user to save not saved data manually
  if DataDirty then
  begin
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
    case asw of
      mrCancel: CanClose := False;//Action := caNone;
      mrNo    : CanClose := True;//Action := caFree;
      mrYes   : optFileSaveClick(Nil);
    end;
  end;

  // settings
  WriteBool('Options', 'FirstRun', False); // first run of this application
  WriteBool('Options', 'ReloadLast', ReloadLast);
  WriteBool('Options', 'AddSamples', AddSamples);
  WriteBool('Options', 'MakeBAK', MakeBAK); // make .BAK copies
  WriteBool('Options', 'UseTray', UseTray);  // minimize to tray
  WriteBool('Options', 'ActiveHyperlinks', ActiveHyperlinks);  // active hyperlinks
  WriteString('Options', 'Language', Language);
  WriteBool('Options', 'AutoCopyPsw', AutoCopyPsw); // copy password to Clipboard on URL lanunch
  WriteBool('Options', 'SaveOnMinim', SaveOnMinim); // prompt to save any unsaved changes on minimize
  WriteBool('Options', 'AutoSave', AutoSave); // auto save on exit
  WriteBool('Options', 'SaveArchCopy', SaveArchCopy); // make additional archival copies?
  WriteString('Options', 'SaveArchCopyTo', SaveArchCopyTo); // archival copies folder
  WriteInteger('Options', 'DblClkAction', DblClkAction); // action performed after double-clicking an item
  WriteInteger('Options', 'NumRecentFiles', NumMRUFiles);  // number of recent files
  WriteBool('Options', 'StayOnTop', (FormStyle = fsStayOnTop));  // stay on top
  WriteBool('Options', 'AutoExpand', AutoExpand); // auto-expand focused items
  WriteBool('Options', 'WarnKeys', WarnKeys); // warn at start about conflicting keys
  WriteBool('Options', 'CheckExpDate', CheckExpDate); // check expiration dates after loading
  WriteInteger('Options', 'ExpDateDays', ExpDateDays); // expiration dates - days ahead
  WriteBool('Options', 'StartExpanded', StartExpanded); // expand all categories?

  // security
  WriteBool('Security', 'ShowPswCol', ShowPswCol);
  WriteBool('Security', 'ShowPswFld', ShowPswFld);
  WriteBool('Security', 'ClipClrExit', ClipClrExit); // clear the clipboard on exit
  WriteBool('Security', 'ClipClrMini', ClipClrMini); // clear the clipboard on minimize
  WriteBool('Security', 'LockOnMinimize', LockOnMinimize); // lock database on minimize
  WriteInteger('Security', 'MaxInactivity', MaxInactivity); // inactivity time (seconds) to lock the database
  WriteBool('Security', 'PasteInLocked', PasteInLocked);  // is Super paste active in locked mode?

  // UI
  WriteBool('UI', 'ViewSttBar'   , sttBar.Visible);
  WriteBool('UI', 'ViewToolbMain', tlbMain.Visible);

  // defaults
  if (UpperCase(DataFile) <> UpperCase(sNoname)) then
    WriteString('Defaults', 'LastFile', DataFile);
  WriteString('Defaults', 'LastSearch', LastSearch);

  // layout

  // form size & placement
  case frmMain.WindowState of
    wsNormal:
        begin
          WriteInteger('Layout', 'WindowState', 1);
          WriteInteger('Layout', 'FormWidth', frmMain.Width);
          WriteInteger('Layout', 'FormHeight', frmMain.Height);
          WriteInteger('Layout', 'FormTop', frmMain.Top);
          WriteInteger('Layout', 'FormLeft', frmMain.Left);
        end;
    wsMaximized:  // don't save the window size in maximized mode!
        begin
          WriteInteger('Layout', 'WindowState', 2);
        end;
  end;
  WriteInteger('Layout', 'PnlBottomHeight', pnlBottom.Height);

  WriteBool('Layout', 'GridLinesHoriz', GridLinesHoriz);
  WriteBool('Layout', 'GridLinesVert', GridLinesVert);
  WriteInteger('Layout', 'GridBackColor', grdTree.BkColor);
  WriteInteger('Layout', 'GridTextColor', grdTree.TextColor);
  WriteString('Layout', 'GridFont', FontToString(grdTree.Font));
  WriteInteger('Layout', 'InfoBackColor', fldInfo.Color);
  WriteInteger('Layout', 'HyperlinkColor', fldInfo.HyperlinkColor); // URL color
  WriteString('Layout', 'InfoFont', FontToString(fldInfo.Font));

  // Import/export
  for i := COL_IDXFIRST to COL_IDXLAST do
  begin
    WriteBool('Export', 'Exp' + GlbColumns[i].Idf, GlbColumns[i].ToExport);
  end;
  WriteString('Export', 'Separator', ExportSep);
  WriteBool('Export', 'QuoteTexts', ExpQuoteTexts); // quote texts?

  // Columns
  for i := COL_IDXFIRST to COL_IDXLAST do
  begin
    WriteString('Columns', 'UserName_' + GlbColumns[i].Idf, GlbColumns[i].UserName);
  end;
  WriteBool('Columns', 'UserColNames', UserColNames); // use user names for columns

  WriteString('Columns', 'SectionsOrder', grdTree.HeaderSections.SectionsOrder);

  // sounds
  WriteBool('Sounds', 'SoundOn', SoundOn); // sound signals?
  for iSnd := FirstSound to LastSound do
    WriteString('Sounds', SoundsAry[iSnd].Sound, SoundsAry[iSnd].GetAsString);


  // Hotkeys
  WriteString('Hotkeys', 'SPaste', HKSPaste.GetAsString);
  WriteString('Hotkeys', 'UPaste', HKUPaste.GetAsString);
  WriteString('Hotkeys', 'PPaste', HKPPaste.GetAsString);
  WriteString('Hotkeys', 'CPaste', HKCPaste.GetAsString);


  if not m_SaveSettingsToRegistry then // save to an ini file
  begin
    hFile := FileOpen(m_iniFile.FileName, fmOpenReadWrite);
    canSave := (hFile > 0);
    FileClose(hFile);
    try
      if canSave then
        m_iniFile.UpdateFile; // flush the INI file
    except
      ;
    end;
  end;

  UnRegisterHotkeys;
end;
//------------------------------------------------------------------------------
// Translate the user interface
//------------------------------------------------------------------------------
// Translate the user interface
procedure TfrmMain.TranslateUI;
var
  i: Integer;
begin
  if Language = '???' then // first run - ask for the language
    if DirectoryExists(MakePath(PgmDir, S_LANGDIR)) then
      optLanguageClick(nil);

  CL := loadIni(S_LANGDIR + '\' + Language);
  if CL <> nil then
    fillProps(
               [frmMain, frmItem, frmPassword, frmAboutBox, frmOptions,
                frmCalendar, frmGenPassword, frmImport, frmLanguage,
                frmCharMap, frmFileWipe, frmProxySettings, frmSoftwareCheck,
                frmHotkeys, frmExpiredDates, frmExport],
               CL);

  GlbColumns[COL_CAT].DftName := ilMiscStr(rsFldCat, 'rsFldCat');
  GlbColumns[COL_SYS].DftName := ilMiscStr(rsFldSys, 'rsFldSys');
  GlbColumns[COL_USR].DftName := ilMiscStr(rsFldUsr, 'rsFldUsr');
  GlbColumns[COL_PSW].DftName := ilMiscStr(rsFldPsw, 'rsFldPsw');
  GlbColumns[COL_CMM].DftName := ilMiscStr(rsFldCmm, 'rsFldCmm');
  GlbColumns[COL_CST].DftName := ilMiscStr(rsFldCst, 'rsFldCst');
  GlbColumns[COL_STA].DftName := ilMiscStr(rsFldSta, 'rsFldSta');
  GlbColumns[COL_EXP].DftName := ilMiscStr(rsFldExp, 'rsFldExp');
  GlbColumns[COL_INF].DftName := ilMiscStr(rsFldInf, 'rsFldInf');
  GlbColumns[COL_SPS].DftName := 'SuperPaste';

  for i := COL_IDXFIRST to COL_IDXLAST do
  begin
    grdTree.HeaderSections[i].Text := GlbColumns[i].Caption;
  end;

  outpopoptClipCopyUsr.Caption  := optClipCopyUsr.Caption;
  outpopoptClipCopyPsw.Caption  := optClipCopyPsw.Caption;
  outpopoptRecordEdit.Caption   := optRecordEdit.Caption;
  outpopoptRecordAdd.Caption    := optRecordAdd.Caption;
  outpopoptRecordCopy.Caption   := optRecordCopy.Caption;
  outpopoptRecordDelete.Caption := optRecordDelete.Caption;
  outpopoptCatRename.Caption    := optCatRename.Caption;
  outpopoptCatRemove.Caption    := optCatRemove.Caption;

  MRUFileList.SubmenuName := ilMiscStr(rsReopen, 'rsReopen');

  // localized message boxes
  LocalizeMessageDlg(dlgMessage);

  UpdateUI; // Update user-interface elements
end;
//------------------------------------------------------------------------------
procedure TfrmMain.LocalizeMessageDlg(md: TMessageDlg);
var
  tmpLis: TStringList;
begin
  tmpLis := TStringList.Create;
  with tmpLis do
  begin
    Add(ilMiscStr(rsBtnYes,      'rsBtnYes'));
    Add(ilMiscStr(rsBtnNo,       'rsBtnNo'));
    Add(ilMiscStr(rsBtnOk,       'rsBtnOk'));
    Add(ilMiscStr(rsBtnCancel,   'rsBtnCancel'));
    Add(ilMiscStr(rsBtnAbort,    'rsBtnAbort'));
    Add(ilMiscStr(rsBtnRetry,    'rsBtnRetry'));
    Add(ilMiscStr(rsBtnIgnore,   'rsBtnIgnore'));
    Add(ilMiscStr(rsBtnAll,      'rsBtnAll'));
    Add(ilMiscStr(rsBtnNoToAll,  'rsBtnNoToAll'));
    Add(ilMiscStr(rsBtnYesToAll, 'rsBtnYesToAll'));
    Add(ilMiscStr(rsBtnHelp,     'rsBtnHelp'));
  end;
  md.CustomButtonCaptions := tmpLis;

  tmpLis.Clear;
  with tmpLis do
  begin
    Add(ilMiscStr(rsMsgWarning,     'rsMsgWarning'));
    Add(ilMiscStr(rsMsgError,       'rsMsgError'));
    Add(ilMiscStr(rsMsgInformation, 'rsMsgInformation'));
    Add(ilMiscStr(rsMsgConfirm,     'rsMsgConfirm'));
    Add('');
  end;
  md.CustomDialogCaptions := tmpLis;
  md.CheckBoxCaption := ilMiscStr(rsDontShowMessage, 'rsDontShowMessage');
  dlgOpen.Filter := ilMiscStr(rsOpenFilter, 'rsOpenFilter');
  dlgSave.Filter := ilMiscStr(rsSaveFilter, 'rsSaveFilter');
end;
//------------------------------------------------------------------------------
// Localized version of the MessageDlg
function TfrmMain.CstMessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word;
begin
  Result := dlgMessage.Execute(Msg, DlgType, Buttons, HelpCtx);
end;
//------------------------------------------------------------------------------
// Localized version of the ShowMessage
procedure TfrmMain.CstShowMessage(const Msg: string);
begin
  dlgMessage.Execute(Msg, mtInformation, [mbOK], 0);
end;
//------------------------------------------------------------------------------
// Add one item to the grid
var
  LastAddRoot: TElTreeItem;
procedure TfrmMain.AddGridItem(item: TOneItem; fSetFocus, fSpeedOpt: Boolean);
var
  i: Integer;
  itm: TElTreeItem;
begin
  grdTree.IsUpdating := True; // prevent redrawing

  if (not fSpeedOpt) or ((LastAddRoot <> nil) and (item.Category <> LastAddRoot.Text)) then
  begin // no speed optimization or another category - look for the correct place
    LastAddRoot := nil;
    // check if the category exists
    for i := 0 to grdTree.Items.Count - 1 do
    begin
      if UpperCase(grdTree.Items[i].Text) = UpperCase(item.Category) then
      begin
        LastAddRoot := grdTree.Items[i];
        break;
      end;
    end;
  end;

  if LastAddRoot = nil then // new category
  begin
    LastAddRoot := grdTree.Items.AddItem(nil);
    LastAddRoot.ParentStyle := False;
    LastAddRoot.Bold := True;
    LastAddRoot.Text := item.Category;
    LastAddRoot.ColumnText.Add(item.Category); // invisible column
    LastAddRoot.ForceButtons := True;
  end;

  // append under the proper category
  itm := grdTree.Items.AddItem(LastAddRoot);
  if (itm <> nil) then
  begin
    itm.ParentStyle := False;
    itm.Bold := True;
    itm.Text := ''; //item.Category;
    itm.ColumnText.Add(item.Category); // invisible column
    itm.ColumnText.Add(item.System);
    itm.ColumnText.Add(item.User);
    itm.ColumnText.Add(item.Password);
    itm.ColumnText.Add(item.Comments);
    itm.ColumnText.Add(item.Custom);
    itm.ColumnText.Add(item.StartDate);
    itm.ColumnText.Add(item.Expires);
    itm.ColumnText.Add(item.Info);
    itm.ColumnText.Add(item.SuperPaste);
    if (fSetFocus) then
      grdTree.ItemFocused := itm;
  end;

  grdTree.IsUpdating := False; // redraw the tree now
  DataDirty := TRUE; // data modified
end;
//------------------------------------------------------------------------------
// Add a new item represented in a string
procedure TfrmMain.AddGridItem(sBff: String; fSetFocus, fSpeedOpt: Boolean);
var
  itm: TOneItem;
begin
  itm := TOneItem.Create;
  itm.FromString(sBff);
  AddGridItem(itm, fSetFocus, fSpeedOpt);
end;
//------------------------------------------------------------------------------
// Update data of the specified record
procedure TfrmMain.UpdateGridItem(TSI: TElTreeItem; item: TOneItem);
begin
  grdTree.IsUpdating := True; // prevent redrawing
  if (TSI.Parent <> nil) then
  begin
    if (TSI.Parent.Text = item.Category) then // category remained the same
    begin
      // remove all existing columns
      while TSI.ColumnText.Count > 0 do
        TSI.ColumnText.Delete(0);

      // add modified data
      //TSI.Text := item.Category;
      TSI.ColumnText.Add(item.Category); // invisible column
      TSI.ColumnText.Add(item.System);
      TSI.ColumnText.Add(item.User);
      TSI.ColumnText.Add(item.Password);
      TSI.ColumnText.Add(item.Comments);
      TSI.ColumnText.Add(item.Custom);
      TSI.ColumnText.Add(item.StartDate);
      TSI.ColumnText.Add(item.Expires);
      TSI.ColumnText.Add(item.Info);
      TSI.ColumnText.Add(item.SuperPaste);
    end
    else // new category
    begin
      TSI.Delete; // remove the original record
      AddGridItem(item, True, False); // append a new one
    end;
  end;
  grdTree.IsUpdating := False; // redraw the tree now

  DataDirty := TRUE; // data modified
end;
//------------------------------------------------------------------------------
// Retrieve data of the specified record, return them in
// a TOneItem structure
function TfrmMain.GridRowAsItem(idx: Integer; var itm: TOneItem): Boolean;
var
  grdItm: TElTreeItem;
begin
  GridRowAsItem := False;
  grdItm := grdTree.Items[idx];
  if (grdItm <> nil) then
    if (grdItm.Parent <> nil) then // not a category header
    begin
      GridRowAsItem  := True;
      itm.Category   := grdItm.ColumnText[COL_CAT-1];
      itm.System     := grdItm.ColumnText[COL_SYS-1];
      itm.User       := grdItm.ColumnText[COL_USR-1];
      itm.Password   := grdItm.ColumnText[COL_PSW-1];
      itm.Comments   := grdItm.ColumnText[COL_CMM-1];
      itm.Custom     := grdItm.ColumnText[COL_CST-1];
      itm.StartDate  := grdItm.ColumnText[COL_STA-1];
      itm.Expires    := grdItm.ColumnText[COL_EXP-1];
      itm.Info       := grdItm.ColumnText[COL_INF-1];
      itm.SuperPaste := grdItm.ColumnText[COL_SPS-1];
    end;
end;
//------------------------------------------------------------------------------
// Retrieve data of the current record, return them in
// a TOneItem structure
function TfrmMain.SelRowAsItem(var itm: TOneItem): Boolean;
var
  itmSel: TElTreeItem;
begin
  SelRowAsItem := False;
  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
    SelRowAsItem := GridRowAsItem(itmSel.AbsoluteIndex, itm);
end;
//------------------------------------------------------------------------------
// Update the 'Dirty' flag
procedure TfrmMain.SetDataDirty(fDirty: Boolean);
begin
  m_DataDirty := fDirty; // data modified
  UpdateFormTitle;
end;
//------------------------------------------------------------------------------
// property DataFile
procedure TfrmMain.SetDataFile(sName: String);
var
  sNam: String;
begin
  sNam := Trim(sName);
  if UpperCase(ExtractFileExt(sNam)) <> UpperCase('.' + S_EXTENSION) then
  begin
    sNam := SwapFileExt(sNam, S_EXTENSION);
    m_DataDirty := True; // data modified
  end;

  m_sDataFile := sNam;
  sttBar.Panels[PNL_FILE].Text := ExtractFileName(sNam) +
                           Format(' (%s)', [ExtractFilePath(m_sDataFile)]);
  outTrayIcon.Hint := 'PINs - ' + ExtractFileName(sNam);

  outtbtnChangePsw.Hint := Format(ilMiscStr(rsChgPswOfFile, 'rsChgPswOfFile'), [ExtractFileName(sNam)]);
end;
//------------------------------------------------------------------------------
// 'show passwords in the table' property
procedure TfrmMain.SetShowPswCol(fShow: Boolean);
begin
  m_ShowPswCol := fShow;
  grdTree.HeaderSections[COL_PSW].Password := not fShow;
end;
//------------------------------------------------------------------------------
// 'show horizontal grid lines' property
procedure TfrmMain.SetGridLinesHoriz(fShow: Boolean);
begin
  m_GridLinesHoriz := fShow;
  grdTree.HorizontalLines := fShow;
end;
//------------------------------------------------------------------------------
// 'show vertical grid lines' property
procedure TfrmMain.SetGridLinesVert(fShow: Boolean);
begin
  m_GridLinesVert := fShow;
  grdTree.VerticalLines := fShow;
end;
//------------------------------------------------------------------------------
function TfrmMain.GetUseTray: Boolean;
begin
  Result := outTrayIcon.MinimizeToTray;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.SetUseTray(fUse: Boolean);
begin
  outTrayIcon.MinimizeToTray := fUse;
end;
//------------------------------------------------------------------------------
function TfrmMain.GetActiveHyperlinks: Boolean;
begin
  Result := fldInfo.HyperlinksActive;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.SetActiveHyperlinks(fAct: Boolean);
begin
  if fldInfo.HyperlinksActive <> fAct then
    fldInfo.Text := ''; // clear the box before changing the flag
  fldInfo.HyperlinksActive := fAct;
  grdTreeItemFocused(nil); // repaint item
end;
//------------------------------------------------------------------------------
// Database locked property
procedure TfrmMain.SetDatabaseLocked(fLocked: Boolean);
begin
  m_DatabaseLocked := fLocked;
  if grdTree.Items.Count = 0 then
    m_DatabaseLocked := False; // no locking for an empty database

  if m_DatabaseLocked then
    acLockDatabase.ImageIndex := 29
  else
    acLockDatabase.ImageIndex := 30;
end;
//------------------------------------------------------------------------------
// Activate the given language
procedure TfrmMain.SetLanguage(sLanguage: String);
begin
  m_sLanguage := ExtractFileName(sLanguage);
  if fInitDone then
    TranslateUI;
end;
//------------------------------------------------------------------------------
// Set the NumMRUFil property
procedure TfrmMain.SetNumMRUFil(iNum: Integer);
begin
  if (iNum >= 0) and (iNum <= 30) then
    m_iNumMRUFil := iNum
  else
    m_iNumMRUFil := 10;

  MRUFileList.Maximum := m_iNumMRUFil;

  // if over 10 items, use a submenu
  MRUFileList.UseSubmenu := (m_iNumMRUFil > 10);
end;
//------------------------------------------------------------------------------
// Return all data as a list of strings
function TfrmMain.GridDataAsStrings(var strLis: TStringList): Boolean;
var
  i: Integer;
  itm: TOneItem;
begin
  GridDataAsStrings := False;
  itm := TOneItem.Create;
  strLis.Clear;
  for i := 0 to grdTree.Items.Count-1 do
  begin
    if (grdTree.Items[i].Parent <> nil) then // skip categories headers
    begin
      itm.FromTreeItem(grdTree.Items[i]);
      strLis.Add('#ITEM ' + Chr(Random(25) + 32) + IntToStr(i) + ' ' + itm.AsString);
      GridDataAsStrings := True;
    end;
  end;
end;
//------------------------------------------------------------------------------
// Save data to a .pins file
procedure TfrmMain.DoSavePINFile(const sName: String);
var
  vFile: System.TextFile;
  strList: TStringList;
  sBff, sArchName: String;
  i: Integer;
  savCursor: TCursor;
begin
  savCursor := Screen.Cursor;
  Screen.Cursor := crHourglass; // show hourglass cursor
  strList := TStringList.Create;
  try
    if MakeBAK then // make .BAK copies
    begin
      DeleteFile(sName + '.BAK'); // remove last .BAK
      RenameFile(sName, sName + '.BAK'); // create new
    end;

    GridDataAsStrings(strList);
    System.AssignFile(vFile, sName);
    Rewrite(vFile);
    Writeln(vFile, 'PINS ' + SAVVSN_STR);
    sBff := '#TEST VERIFY';
    for i := 1 to Random(50) do
      sBff := sBff + Chr(Random(25) + 32);

    Writeln(vFile, CvtStr2HexStr(EncodeBlowfish(sBff, Password, SAVVSN_INT)));
    for i := 0 to strList.Count - 1 do
    begin
      sBff := CvtStr2HexStr(EncodeBlowfish(strList[i], Password, SAVVSN_INT));
      Writeln(vFile, sBff);
    end;
    CloseFile(vFile);
    DataFile := sName; // store the new name
    DataDirty := FALSE; // data not modified anymore

    if SaveArchCopy then // an additional archival copy must be created
    begin
      sArchName := MakePath(SaveArchCopyTo, ExtractFileName(sName));
      if not CopyFile(PChar(sName), PChar(sArchName), False) then
        CstShowMessage(Format(ilMiscStr(rsErrSaveToArchive, 'rsErrSaveToArchive'), [sArchName]));
    end;
    PlayOneSound(sndSAVE);
  except
    on E: Exception do
    begin
      PlayOneSound(sndERROR);
      sBff := Format(ilMiscStr(rsErrDataSave, 'rsErrDataSave'), [sName, E.Message]);
      CstShowMessage(sBff);
    end;
  end;
  MRUFileList.AddItem(DataFile);
  Screen.Cursor := savCursor;  // restore default cursor
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Convert the separator name to the separator itself
function TfrmMain.SepToStr(sep: String): String;
begin
  if UpperCase(sep) = 'TAB' then
    Result := S_TAB
  else
    Result := sep;
end;
//------------------------------------------------------------------------------
// Ask up to 3 times for a given password.
// Return value: one of mrYes, mrNo, mrCancel
function TfrmMain.ConfirmPassword(sPassword: String; sFstMessage: String): Integer;
var
  i: Integer;
  iRet: Integer;
begin
  iRet := mrCancel;
  for i := 1 to 3 do // we have 3 chances to enter the correct password
  begin
    if i = 1 then
      frmPassword.sPrompt := sFstMessage
    else
      frmPassword.sPrompt := ilMiscStr(rsBadPasswordTryAgain, 'rsBadPasswordTryAgain');

    frmPassword.sttBar.SimpleText := DataFile;
    frmPassword.fGenPassword := False;
    frmPassword.fBtnMinimizeVisible := False;
    frmPassword.ShowModal;
    case frmPassword.pwaRet of
      PWA_CANCEL:
            begin
              iRet := mrCancel;
              Break; // for
            end;
      PWA_MINIMIZE:
            begin
              iRet := mrCancel;
              Application.Minimize;
              Break; // for
            end;
      PWA_ACCEPT:
            begin
              if sPassword = frmPassword.sEnteredPassword then
              begin
                iRet := mrYes;
                Break; // for
              end
              else
                iRet := mrNo;
            end;
    end;
  end;
  Result := iRet;
end;
//------------------------------------------------------------------------------
// Save button clicked
procedure TfrmMain.optFileSaveClick(Sender: TObject);
var
  sNam: String;
begin
  sNam := m_sDataFile;
  if DataFile = sNoname then // no name given yet, ask the user
    optFileSaveAsClick(Nil)
  else
    DoSavePINFile(sNam); // save under current name
end;
//------------------------------------------------------------------------------
// Save as...
procedure TfrmMain.optFileSaveAsClick(Sender: TObject);
begin
  dlgSave.FilterIndex := 1;
  // set logical initial folder
  if Length(ExtractFilePath(DataFile)) > 0 then // has path
    dlgSave.InitialDir := ExtractFilePath(DataFile)
  else
    dlgSave.InitialDir := m_sPgmDir;

  dlgSave.FileName := ChangeFileExt(ExtractFileName(DataFile), '');
  if dlgSave.Execute then
  begin
    case dlgSave.FilterIndex of
      2:
         begin
            frmExport.FileName := dlgSave.FileName;
            frmExport.ShowModal;
         end;
      else
        DoSavePINFile(dlgSave.FileName);
    end;
  end;
end;
//------------------------------------------------------------------------------
// Export to the text file
procedure TfrmMain.optExportToTextClick(Sender: TObject);
begin
  dlgSave.FileName := SwapFileExt(DataFile, 'txt');

  dlgSave.FilterIndex := 2;
  if dlgSave.Execute then
  case dlgSave.FilterIndex of
    2:
       begin
          frmExport.FileName := dlgSave.FileName;
          frmExport.ShowModal;
       end;
    else
      DoSavePINFile(dlgSave.FileName);
  end;
end;
//------------------------------------------------------------------------------
// Create a new, empty data file
procedure TfrmMain.tbtnNewClick(Sender: TObject);
var
  asw: Integer;
  bff: String;
begin
  asw := mrNo;
  if DataDirty then
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
  case asw of
    mrCancel: ;
    mrNo    : ;
    mrYes   : optFileSaveClick(Nil);
  end;

  if asw <> mrCancel then
  begin
    grdTree.Items.Clear;
    DataDirty := False; // data not modified
    if AddSamples then
    begin
      grdTree.IsUpdating := True; // prevent redrawing
      AddGridItem(TOneItem.Create('e-mail', 'Melissa', 'XNet', 'YYan62',
                                  'My main e-mail', 'cst', '2001.04.23', ilMiscStr(rsNever, 'rsNever'), 'Melissa@XNet.com' + S_CRLF + 'POP,SMTP: mail.XNet.com' + S_CRLF + 'http://www.xnet.com', ''), False, False);
      AddGridItem(TOneItem.Create('e-mail', 'Melissa', 'Yahoo', 'sdgs_g#e',
                                  'For Usenet', 'cst', '2001.04.23', ilMiscStr(rsNever, 'rsNever'), 'www.yahoo.com' + S_CRLF + 'POP,SMTP: mail.yahoo.com', ''), False, False);
      AddGridItem(TOneItem.Create('e-mail', 'Mirek', 'Freemail', 'gr3a-6q',
                                  'www.freemail.com', 'cst', '2001.04.13', ilMiscStr(rsNever, 'rsNever'), 'mirek@freemail.com' + S_CRLF + 'POP, SMTP: mail.freemail.com' + S_CRLF + 'MBox size: 25MB, max mail: 4MB', ''), False, False);
      AddGridItem(TOneItem.Create('PIN', 'John', 'VISA Classic', '1234',
                                  'MidBank', 'cst', '1999.08.13', '01/04', 'No. 4251 6740 0221 9074', ''), False, False);
      AddGridItem(TOneItem.Create('WWW service', 'Fred', 'ZDNet', 'Kr45#$q2@',
                                  'www.zdnet.com', 'cst', '1996.11.03', ilMiscStr(rsNever, 'rsNever'), '', ''), False, False);
      AddGridItem(TOneItem.Create('Password', 'Mirek', 'ZIPped files', 'JasonX81',
                                  'www.yahoo.com', 'cst', '2000.12.01', ilMiscStr(rsNever, 'rsNever'), '', ''), False, False);
      AddGridItem(TOneItem.Create('Password', 'Jennifer', 'Mobile', 'Jenni3',
                                  '', 'cst', '1998.11.21', ilMiscStr(rsNever, 'rsNever'), '', ''), False, False);
      grdTree.IsUpdating := False; // redraw the tree now
    end;

    DataFile := sNoname;
    bff := GetNewPassword();
    Password := bff;
    DataDirty := False; // data not modified
  end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Open the passwords file
function TfrmMain.DoOpenPINFile(filNam: String; fAppend: Boolean): Boolean;
var
  strListFile, strListDec: TStringList;
  i, iTry: Integer;
  isOk, isCancelled: Boolean;
  sPsw, sBff: String;
  iVersion: Integer;  // 2.50 is stored as 250
  savCursor: TCursor;

  // determine the save version
  function ExtractVersion(const sFirstLine: String): Integer;
  var
    sVsn: String;
    iMaj, iMin, iPos: Integer;
  begin
    // example: PINS 2.50
    iMin := 0;
    iMaj := 0;
    sVsn := Trim(Copy(sFirstLine, 5, Length(sFirstLine)));
    iPos := Pos('.', sVsn);
    if (iPos >= 0) then
    begin
      iMaj := CvtStr2Int(Copy(sVsn, 1, iPos - 1));
      iMin := CvtStr2Int(Copy(sVsn, iPos + 1, 3));
    end;
    Result := iMaj * 100 + iMin;
  end;
begin
  savCursor := Screen.Cursor;
  isOk := False;
  isCancelled := False;

  if FileExists(filNam) then
  begin
    strListFile := TStringList.Create; // source lines
    strListFile.LoadFromFile(filNam);
    strListFile.Add('');
    strListFile.Add('');

    strListDec  := TStringList.Create; // decoded lines
    for i := 0 to strListFile.Count - 1 do
      strListDec.Add('');

    if StrStartsWith(strListFile.Strings[0], 'PINS') then // valid file?
    begin
      iVersion := ExtractVersion(strListFile.Strings[0]); // saved with this version

      // we have 3 chances to enter the password
      for iTry := 1 to 3 do
      begin
        if iTry = 1 then
          frmPassword.sPrompt := Format(ilMiscStr(rsEnterFilePassword, 'rsEnterFilePassword'),
                                        [ExtractFileName(filNam)]) // Enter the password for file %s:
        else
          frmPassword.sPrompt := ilMiscStr(rsBadPasswordTryAgain, 'rsBadPasswordTryAgain');
        frmPassword.sttBar.SimpleText := filNam;
        frmPassword.fGenPassword := False;
        Screen.Cursor := savCursor;  // restore default cursor
        frmPassword.fBtnMinimizeVisible := False;
        frmPassword.ShowModal;
        case frmPassword.pwaRet of
          PWA_CANCEL:
                begin
                  isCancelled := True;
                  Break; // for iTry
                end;
          PWA_MINIMIZE:
                begin
                  isCancelled := True;
                  Application.Minimize;
                  Break; // for iTry
                end;
          PWA_ACCEPT:
                begin
                  Screen.Cursor := crHourglass; // show hourglass cursor
                  sPsw := frmPassword.sEnteredPassword; // get the password

                  for i := 1 to strListFile.Count - 1 do // decode everything
                    strListDec.Strings[i] := DecodeBlowfish(CvtHexStr2Str(strListFile.Strings[i]), sPsw, iVersion);

                  if StrStartsWith(strListDec.Strings[1], '#TEST VERIFY') then // valid password?
                  begin
                    Password := sPsw;
                    isOk := True;
                    if not fAppend then
                    begin
                      grdTree.Items.Clear;
                      DataFile := filNam; // register the file name
                    end;
                    MRUFileList.AddItem(filNam);

                    grdTree.IsUpdating := True; // prevent redrawing
                    for i := 2 to strListDec.Count - 1 do
                    begin
                      sBff := strListDec.Strings[i];
                      if StrStartsWith(sBff, '#ITEM') then
                        AddGridItem(sBff, False, i > 2); // speed optiomization for all subsequent items
                    end;
                    grdTree.IsUpdating := False; // redraw the tree now
                    if not fAppend then
                      DataDirty := FALSE; // data not modified
                    PlayOneSound(sndOPEN);

                    if StartExpanded then // expand all items?
                      optExpandAllClick(nil);

                    if CheckExpDate then
                      DoCheckExpDate;

                    Break; // for iTry
                  end;
                end;
        end;
      end; // for iTry

      if (not isOk) and (not isCancelled) then
      begin
        PlayOneSound(sndERROR);
        CstMessageDlg(ilMiscStr(rsInvPassword, 'rsInvPassword'), mtError, [mbOk], 0);
      end;
    end
    else // valid file?
    begin
      PlayOneSound(sndERROR);
      CstMessageDlg(ilMiscStr(rsInvPINSFile, 'rsInvPINSFile'), mtError, [mbOk], 0);
    end;
  end
  else
  begin
    PlayOneSound(sndERROR);
    CstMessageDlg(Format(ilMiscStr(rsFileXNotFound, 'rsFileXNotFound'), [filNam]), mtError, [mbOk], 0);
  end;

  Screen.Cursor := savCursor;  // restore default cursor
  UpdateUI; // update all user interface elements
  Result := isOk;
end;
//------------------------------------------------------------------------------
// Check the type of the specified file.
// -1 - no file
//  0 - PINs file
//  1 - other file
function TfrmMain.CheckFileType(const sPath: String): Integer;
var
  strList: TStringList;
  iTyp: Integer;
begin
  iTyp := -1;

  if FileExists(sPath) then
  begin
    strList := TStringList.Create;
    strList.LoadFromFile(sPath);
    strList.Add('');
    if StrStartsWith(strList.Strings[0], 'PINS') then // PINs file?
      iTyp := 0
    else
      iTyp := 1;
  end;
  Result := iTyp;
end;
//------------------------------------------------------------------------------
// Open the data file. Check if it is password-protected.
procedure TfrmMain.optFileOpenClick(Sender: TObject);
var
  asw: Integer;
begin
  asw := mrNo;
  if DataDirty then
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
  case asw of
    mrCancel: ;
    mrNo    : ;
    mrYes   : optFileSaveClick(Nil);
  end;

  if asw <> mrCancel then
  begin
    // set logical initial folder
    if Length(ExtractFilePath(DataFile)) > 0 then // has path
      dlgOpen.InitialDir := ExtractFilePath(DataFile)
    else
      dlgOpen.InitialDir := m_sPgmDir;
    dlgOpen.FilterIndex := 1;

    if dlgOpen.Execute then
      DoOpenFile(dlgOpen.FileName, False);
  end;
end;
//------------------------------------------------------------------------------
// Try to open the specified file
function TfrmMain.DoOpenFile(filNam: String; fAppend: Boolean): Boolean;
begin
  Result := False;
  case CheckFileType(filNam) of
    0: Result := DoOpenPINFile(filNam, fAppend);
    1: Result := DoOpenTextFile(filNam, fAppend);
  end;
end;
//------------------------------------------------------------------------------
function TfrmMain.DoOpenTextFile(filNam: String; fAppend: Boolean): Boolean;
begin
  frmImport.Init(filNam, fAppend);
  frmImport.ShowModal;
  Result := frmImport.fAccepted;

  if Result and StartExpanded then // expand all items?
    optExpandAllClick(nil);

  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optImportFromTextClick(Sender: TObject);
var
  asw: Integer;
begin
  asw := mrNo;
  if DataDirty then
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
  case asw of
    mrCancel: ;
    mrNo    : ;
    mrYes   : optFileSaveClick(Nil);
  end;

  if asw <> mrCancel then
  begin
    dlgOpen.FilterIndex := 3;
    if dlgOpen.Execute then
      DoOpenFile(dlgOpen.FileName, False);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optFileAppendClick(Sender: TObject);
begin
  dlgOpen.FilterIndex := 1;
  if dlgOpen.Execute then
    DoOpenFile(dlgOpen.FileName, True);
end;
//------------------------------------------------------------------------------
// Check expiration dates
procedure TfrmMain.DoCheckExpDate;
var
  i: Integer;
  itm: TOneItem;
  fExpired: Boolean;
begin
  itm := TOneItem.Create;
  fExpired := False;
  for i := 0 to grdTree.Items.Count-1 do
  begin
    if (grdTree.Items[i].Parent <> nil) then // skip categories headers
    begin
      itm.FromTreeItem(grdTree.Items[i]);
      if (Length(itm.Expires) > 0)
          and (itm.Expires <> ilMiscStr(rsNever, 'rsNever'))
          and (itm.Expires <> 'Never') then // intentionally English (compatibility to v.2.0)
        if IsStrValidDate(itm.Expires) then
          if CvtStr2Date(itm.Expires) <= (Now() + ExpDateDays) then
          begin
            if CstMessageDlg(ilMiscStr(rsIfReviewExpPass, 'rsIfReviewExpPass'), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
              fExpired := True;
            Break; // for
          end;
    end;
  end;

  if fExpired then
  begin
    frmExpiredDates.Show;
    frmExpiredDates.btnRescanClick(Nil);
  end;
end;
//------------------------------------------------------------------------------
// Find expired passwords
procedure TfrmMain.optExpiredPasswordsClick(Sender: TObject);
begin
  frmExpiredDates.Show;
  frmExpiredDates.btnRescanClick(Nil);
end;
//------------------------------------------------------------------------------
// Edit current record
procedure TfrmMain.optRecordEditClick(Sender: TObject);
var
  itm: TOneItem;
begin
  itm := TOneItem.Create;
  if SelRowAsItem(itm) then
  begin
    frmItem.Init(itm, MIM_EDIT);
    frmItem.ShowModal;
    if (frmItem.IsAccepted) then
    begin
      itm := frmItem.GetResult;
      UpdateGridItem(grdTree.ItemFocused, itm);
    end;
  end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Add a new record, allow to edit it
procedure TfrmMain.optRecordAddClick(Sender: TObject);
var
  itmSel: TElTreeItem;
  itm: TOneItem;
begin
  itm := TOneItem.Create;

  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
    itm.Category := itmSel.ColumnText[COL_CAT-1]; //itmSel.Text;

  frmItem.Init(itm, MIM_ADD);
  frmItem.ShowModal;
  if (frmItem.IsAccepted) then
  begin
    itm := frmItem.GetResult;
    AddGridItem(itm, True, False);
    DataDirty := True; // data modified
  end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Add a new record, fill it with a copy of the current record
procedure TfrmMain.optRecordCopyClick(Sender: TObject);
var
  itm: TOneItem;
begin
  itm := TOneItem.Create;
  if SelRowAsItem(itm) then
  begin
    frmItem.Init(itm, MIM_ADD);
    frmItem.ShowModal;
    if (frmItem.IsAccepted) then
    begin
      itm := frmItem.GetResult;
      AddGridItem(itm, True, False);
    end;
    DataDirty := TRUE; // data modified
  end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Delete current row
procedure TfrmMain.optRecordDeleteClick(Sender: TObject);
var
  itmSel: TElTreeItem;
  asw: Integer;
begin
  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
    if (itmSel.Parent <> nil) then
    begin
      asw := CstMessageDlg(ilMiscStr(rsDeleteRec, 'rsDeleteRec'), mtConfirmation, [mbYes, mbNo], 0);
      if asw = mrYes then
      begin
        grdTree.ItemFocused.Delete;
        DataDirty := TRUE; // data modified
      end;
    end;
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Terminate
procedure TfrmMain.optExitClick(Sender: TObject);
//var
//  asw: Integer;
begin
{ done in FormCloseQuery
  asw := mrNo;
  if DataDirty then
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
  case asw of
    mrCancel: ;
    mrNo    : ;
    mrYes   : optFileSaveClick(Nil);
  end;
  if asw <> mrCancel then
}
//    Application.Terminate;
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.pnlBottomResize(Sender: TObject);
begin
  fldInfo.Width := pnlBottom.Width - fldInfo.Left;
  fldInfo.Height := pnlBottom.Height - sttBar.Height - 4;
end;
//------------------------------------------------------------------------------
// Double click - edit item
procedure TfrmMain.grdTreeDblClick(Sender: TObject);
var
  itmSel: TElTreeItem;
begin
  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
  begin
    if (itmSel.Parent <> nil) then // item
    begin
      case DblClkAction of
        DCA_EDIT:     optRecordEditClick(Nil);
        DCA_COPYPASS: optClipCopyPswClick(Nil);
        DCA_COPYUSER: optClipCopyUsrClick(Nil);
        DCA_LNCHURL:  optHyperlinkClick(Nil);
      end;
    end
    else // category - expand/collapse
      if grdTree.ItemFocused.Expanded then
        grdTree.ItemFocused.Collapse(True)
      else
        grdTree.ItemFocused.Expand(True);
  end;
end;
//------------------------------------------------------------------------------
// Update the form title
procedure TfrmMain.UpdateFormTitle;
var
  sTtl: String;
  sName: String;
  sAdd: String; // additional version information
begin
  sTtl := ExtractFileName(DataFile) + ' - ';

  if VSN_STD then
    sName := 'PINs'
  else if VSN_HSO then
    sName := 'HSO PINs'
  else
    sName := 'PINs';

  sAdd := '';
  frmMain.Caption := sTtl + sName + ' v.' + lblVsn.GetLabelText +
                     iifs(IsBeta, ' Beta', '') + sAdd + iifs(DataDirty, ' *', '');
end;
//------------------------------------------------------------------------------
// Function allows to define a new password. The user must
// retype it in order to validate it.
function TfrmMain.GetNewPassword: String;
var
  sNewPsw: String;
begin
  GetNewPassword := '';

  frmPassword.sPrompt := ilMiscStr(rsEnterPass4NewFile, 'rsEnterPass4NewFile'); // Enter an access password for this new data file
  frmPassword.fGenPassword := True;
  frmPassword.fBtnMinimizeVisible := False;
  frmPassword.ShowModal;
  case frmPassword.pwaRet of
    PWA_CANCEL:
          begin
          end;
    PWA_MINIMIZE:
          begin
            Application.Minimize;
          end;
    PWA_ACCEPT:
          begin
            sNewPsw := frmPassword.sEnteredPassword;
            frmPassword.sPrompt := ilMiscStr(rsRetypeNewPass, 'rsRetypeNewPass'); // Retype your new password:
            frmPassword.fGenPassword := False;
            frmPassword.fBtnMinimizeVisible := False;
            frmPassword.ShowModal;
            case frmPassword.pwaRet of
              PWA_CANCEL:
                    begin
                    end;
              PWA_MINIMIZE:
                    begin
                      Application.Minimize;
                    end;
              PWA_ACCEPT:
                    begin
                      if sNewPsw = frmPassword.sEnteredPassword then
                      begin
                        GetNewPassword := sNewPsw;
                      end
                      else
                      begin
                        PlayOneSound(sndERROR);
                        CstMessageDlg(ilMiscStr(rsPassDontMatch, 'rsPassDontMatch'), mtError, [mbOk], 0);
                      end;
                    end;
            end;
          end;
  end;
end;
//------------------------------------------------------------------------------
// Put the user name on the clipboard
procedure TfrmMain.optClipCopyUsrClick(Sender: TObject);
var
  itm: TOneItem;
begin
  itm := TOneItem.Create;
  if SelRowAsItem(itm) then
  begin
    Clipboard.SetTextBuf(PChar(itm.User));
    PlayOneSound(sndCLIPCOPYUSR);
  end;
end;
//------------------------------------------------------------------------------
// Low-level function putting the password on the clipboard
procedure TfrmMain.DoPutPasswordOnClipboard(sPass: String);
begin
  Clipboard.Open;
  Clipboard.Clear;

  // send the "Ignore" flag to clipboard extenders
  Clipboard.SetAsHandle(CF_CLIPBOARD_VIEWER_IGNORE, 0);
  // now put the password on the clipboard
  Clipboard.SetTextBuf(PChar(sPass));
  PlayOneSound(sndCLIPCOPYPSW);

  Clipboard.Close;
end;
//------------------------------------------------------------------------------
// Put the password on the clipboard
procedure TfrmMain.optClipCopyPswClick(Sender: TObject);
var
  itm: TOneItem;
begin
  itm := TOneItem.Create;
  if SelRowAsItem(itm) then
    DoPutPasswordOnClipboard(itm.Password);
end;
//------------------------------------------------------------------------------
// Empty the clipboard
procedure TfrmMain.optClipEmptyClick(Sender: TObject);
begin
  Clipboard.Clear;
end;
//------------------------------------------------------------------------------
// Allow to change the password for the file
procedure TfrmMain.optChangePswClick(Sender: TObject);
var
  sNewPsw: String;
begin
  frmPassword.sPrompt := ilMiscStr(rsTypeOldPass, 'rsTypeOldPass'); // Type your old password
  frmPassword.sttBar.SimpleText := DataFile;
  frmPassword.fGenPassword := False;
  frmPassword.fBtnMinimizeVisible := False;
  frmPassword.ShowModal;
  case frmPassword.pwaRet of
    PWA_CANCEL:
          begin
          end;
    PWA_MINIMIZE:
          begin
            Application.Minimize;
          end;
    PWA_ACCEPT:
          begin
            if (frmPassword.sEnteredPassword = Password) then // old password ok
            begin
              frmPassword.sPrompt := ilMiscStr(rsTypeNewPass, 'rsTypeNewPass'); // Type your new password:
              frmPassword.fGenPassword := True;
              frmPassword.fBtnMinimizeVisible := False;
              frmPassword.ShowModal;
              case frmPassword.pwaRet of
                PWA_CANCEL:
                      begin
                      end;
                PWA_MINIMIZE:
                      begin
                        Application.Minimize;
                      end;
                PWA_ACCEPT:
                      begin
                        // new password entered, verify it
                        sNewPsw := frmPassword.sEnteredPassword;
                        frmPassword.sPrompt := ilMiscStr(rsRetypeNewPass, 'rsRetypeNewPass'); // Retype your new password:
                        frmPassword.fGenPassword := False;
                        frmPassword.fBtnMinimizeVisible := False;
                        frmPassword.ShowModal;
                        case frmPassword.pwaRet of
                          PWA_CANCEL:
                                begin
                                end;
                          PWA_MINIMIZE:
                                begin
                                  Application.Minimize;
                                end;
                          PWA_ACCEPT:
                                begin
                                  if sNewPsw = frmPassword.sEnteredPassword then
                                  begin
                                    Password := sNewPsw;
                                    DataDirty := TRUE; // data modified
                                  end
                                  else
                                  begin
                                    PlayOneSound(sndERROR);
                                    CstMessageDlg(ilMiscStr(rsPassDontMatch, 'rsPassDontMatch'), mtError, [mbOk], 0);
                                  end;
                                end;
                        end;
                      end;
              end;
            end
            else
            begin
              PlayOneSound(sndERROR);
              CstMessageDlg(ilMiscStr(rsInvPassword, 'rsInvPassword'), mtError, [mbOk], 0);
            end;
          end; // PWA_ACCEPT
  end;
end;
//------------------------------------------------------------------------------
// Generate a password
procedure TfrmMain.optGenPasswordClick(Sender: TObject);
begin
  frmGenPassword.Init(False);
  frmGenPassword.ShowModal;
end;
//------------------------------------------------------------------------------
// Invoke Options dialog
procedure TfrmMain.optToolsOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;
//------------------------------------------------------------------------------
// Configure hotkeys
procedure TfrmMain.optHotkeysClick(Sender: TObject);
begin
  frmHotkeys.ShowModal;
end;
//------------------------------------------------------------------------------
// About...
procedure TfrmMain.optHelpAboutClick(Sender: TObject);
begin
  frmAboutBox.ShowModal;
end;
//------------------------------------------------------------------------------
// MRU list - one of files clicked
procedure TfrmMain.MRUFileListMRUItemClick(Sender: TObject; AFilename: String);
var
  asw: Integer;
begin
  asw := mrNo;
  if DataDirty then
    asw := CstMessageDlg(ilMiscStr(rsDataNotSave, 'rsDataNotSave'), mtWarning, mbYesNoCancel, 0);
  case asw of
    mrCancel: ;
    mrNo    : ;
    mrYes   : optFileSaveClick(Nil);
  end;

  if asw <> mrCancel then
    if DoOpenPINFile(AFilename, False) then
      DataFile := AFilename;
end;
//------------------------------------------------------------------------------
// Invoke the "Find" dialog
procedure TfrmMain.optFindClick(Sender: TObject);
begin
  FindDialog.FindText := LastSearch;
  FindDialog.Execute;
end;
//------------------------------------------------------------------------------
// Perform the search
procedure TfrmMain.FindDialogFind(Sender: TObject);
var
  i, startRow: Integer;
  isFound: Boolean;

  function FindInRow(row: Integer): Boolean;
  var
    col: Integer;
    foundInRow: Boolean;
    sTxt: String;
    sSource: String;
  begin
    foundInRow := False;
    sTxt := FindDialog.FindText;
    if not (frMatchCase in FindDialog.Options) then
      sTxt := UpperCase(sTxt);

    for col := 0 to grdTree.Items[row].ColumnText.Count - 1 do
    begin
      sSource := grdTree.Items[row].ColumnText[col];
      if not (frMatchCase in FindDialog.Options) then
        sSource := UpperCase(sSource);
      if Pos(sTxt, sSource) > 0 then
      begin
        foundInRow := True;
        break;
      end;
    end;
    FindInRow := foundInRow;
  end;

begin
  isFound := False;
  LastSearch := FindDialog.FindText;

  if (grdTree.ItemFocused <> nil) then
    startRow := grdTree.ItemFocused.AbsoluteIndex + 1
  else
    startRow := 0;

  for i := startRow to grdTree.Items.Count - 1 do
    if FindInRow(i) then
    begin
      grdTree.ItemFocused := grdTree.Items[i];
      isFound := True;
      break;
    end;

  if not isFound then
    for i := 0 to grdTree.Items.Count - 1 do
      if FindInRow(i) then
      begin
        grdTree.ItemFocused := grdTree.Items[i];
        isFound := True;
        break;
      end;

  if isFound then
    grdTree.EnsureVisible(grdTree.ItemFocused)
  else
    CstShowMessage(Format(ilMiscStr(rsTextXNotFound, 'rsTextXNotFound'), [FindDialog.FindText]));
end;
//------------------------------------------------------------------------------
// Search again for the last text
procedure TfrmMain.optFindNextClick(Sender: TObject);
begin
  FindDialog.FindText := LastSearch;
  FindDialogFind(Nil);
end;
//------------------------------------------------------------------------------
// An item got the focus, show its extra data
procedure TfrmMain.grdTreeItemFocused(Sender: TObject);
begin
  // active item must be expanded...
  if (grdTree.ItemFocused <> nil) then
    if (grdTree.ItemFocused.Parent <> nil) then
      grdTree.ItemFocused.Parent.Expand(True);

  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Launch the main hyperlink
procedure TfrmMain.optHyperlinkClick(Sender: TObject);
begin
  if lisTmp.Items.Count > 0 then
    LaunchHyperlink(Handle, lisTmp.Items.Strings[0]);

  if AutoCopyPsw then // copy password to Clipboard on URL lanunch
    DoPutPasswordOnClipboard(CrrItemPsw);
end;
//------------------------------------------------------------------------------
// One of hyperlinks clicked in the memo field
procedure TfrmMain.fldInfoHyperlinkClicked(sURL: String);
begin
  LaunchHyperlink(Handle, sURL);

  if AutoCopyPsw then // copy password to Clipboard on URL lanunch
    DoPutPasswordOnClipboard(CrrItemPsw);
end;
//------------------------------------------------------------------------------
// Launch one of hyperlinks selected from the popup menu
procedure TfrmMain.popOneHyperlinkClick(Sender: TObject);
begin
  LaunchHyperlink(Handle, TMenuItem(Sender).Caption);

  if AutoCopyPsw then // copy password to Clipboard on URL lanunch
    DoPutPasswordOnClipboard(CrrItemPsw);
end;
//------------------------------------------------------------------------------
// Update all user interface elements
var
  fInUpdate: Boolean = False;
procedure TfrmMain.UpdateUI; // update all user interface elements
var
  itmSel: TElTreeItem;
  isCat, isItm: Boolean;
  i, col: Integer;
  sTxt, sURL: String;
  strLis: TStringList;
  mnu: TMenuItem;
  fIsMinimized: Boolean;
begin
  if fInUpdate then // no recursive calls!
    Exit;

  fInUpdate := True;

  optViewSttBar.Checked  := sttBar.Visible;
  optViewTlbMain.Checked := tlbMain.Visible;

  isCat  := False;
  isItm  := False;
  itmSel := grdTree.ItemFocused;

  fldInfo.Text := '';
  if (itmSel <> nil) then // any item active
  begin
    isCat := itmSel.Parent = nil;
    isItm := not isCat;

    if itmSel.ColumnText.Count >= COL_INF then // redundant in this version check
      fldInfo.Text := itmSel.ColumnText[COL_INF-1] // -1 - ColumnText skips the main column
  end;

  fldInfo.RescanHyperlinks;

  optCatRename.Enabled := isCat;
  optCatRemove.Enabled := isCat;

  optClipCopyPsw.Enabled  := isItm; // not for nodes
  optClipCopyUsr.Enabled  := optClipCopyPsw.Enabled;
  optRecordDelete.Enabled := optClipCopyPsw.Enabled;
  optRecordEdit.Enabled := optClipCopyPsw.Enabled;
  optRecordCopy.Enabled := optClipCopyPsw.Enabled;
  optFileSave.Enabled := DataDirty;

  outpopoptClipCopyPsw.Enabled := optClipCopyPsw.Enabled;
  outpopoptClipCopyUsr.Enabled := optClipCopyUsr.Enabled;
  outpopoptRecordDelete.Enabled := optClipCopyPsw.Enabled;
  outpopoptRecordEdit.Enabled := optClipCopyPsw.Enabled;
  outpopoptRecordCopy.Enabled := optClipCopyPsw.Enabled;
  outpopoptCatRename.Enabled := optCatRename.Enabled;
  outpopoptCatRemove.Enabled := optCatRemove.Enabled;

  tbtnClipCopyUsr.Enabled := optClipCopyUsr.Enabled;
  tbtnClipCopyPsw.Enabled := optClipCopyPsw.Enabled;
  tbtnDel.Enabled  := optRecordDelete.Enabled;
  tbtnEdit.Enabled := optRecordEdit.Enabled;
  tbtnCopy.Enabled := optRecordCopy.Enabled;

  tbtnAdd.Enabled  := optRecordAdd.Enabled;
  tbtnSave.Enabled := optFileSave.Enabled;
  tbtnSaveAs.Enabled := optFileSaveAs.Enabled;

  sttBar.Panels[PNL_RECCNT].Text := Format(ilMiscStr(rsRecCount, 'rsRecCount'), [ItemsCount()]);
  UpdateFormTitle;

  // find all URLs in the record
  optHyperlink.Enabled := False;
  tbtnHyperlink.Hint := ilMiscStr(rsLaunchURLs, 'rsLaunchURLs'); // Launch URLs
  while popHyperlinks.Items.Count > 0 do
    popHyperlinks.Items.Delete(0);
  lisTmp.Clear;

  if isItm then
  begin
    CrrItemPsw := itmSel.ColumnText[COL_PSW - 1]; // the password of the current item

    strLis := TStringList.Create;
    // first the URL column
    sTxt := itmSel.ColumnText[COL_CMM - 1];
    if ExtractURLs(sTxt, strLis) then // URLs detected, fill the Launch menu
    begin
      for i := 0 to strLis.Count - 1 do
      begin
        sURL := strLis.Strings[i];
        if Pos(',', sURL) > 0 then
          sURL := Copy(sURL, Pos(',', sURL) + 1, Length(sURL));
        if lisTmp.Items.IndexOf(sURL) < 0 then  // if does not exist yet
          lisTmp.Items.Add(sURL);
      end;
    end;

    // now the rest of columns
    for col := 0 to itmSel.ColumnText.Count - 1 do
    begin
      sTxt := itmSel.ColumnText[col];
      if ExtractURLs(sTxt, strLis) then // URLs detected, fill the Launch menu
      begin
        for i := 0 to strLis.Count - 1 do
        begin
          sURL := strLis.Strings[i];
          if Pos(',', sURL) > 0 then
            sURL := Copy(sURL, Pos(',', sURL) + 1, Length(sURL));
          if lisTmp.Items.IndexOf(sURL) < 0 then // if does not exist yet
            lisTmp.Items.Add(sURL);
        end;
      end;
    end;

    optHyperlink.Enabled := lisTmp.Items.Count > 0;
    if lisTmp.Items.Count > 0 then
    begin
      tbtnHyperlink.PopupMenu := popHyperlinks;

      for i := 0 to lisTmp.Items.Count - 1 do
      begin
        mnu := TMenuItem.Create(Self);
        mnu.Caption := lisTmp.Items.Strings[i];
        mnu.OnClick := popOneHyperlinkClick;
        popHyperlinks.Items.Add(mnu);
      end;

      mnu := TMenuItem.Create(Self);
      mnu.Caption := '-';
      popHyperlinks.Items.Add(mnu);

      mnu := TMenuItem.Create(Self);
      mnu.Caption := ilMiscStr(rsBtnCancel, 'rsBtnCancel');
      popHyperlinks.Items.Add(mnu);

      tbtnHyperlink.Hint := ilMiscStr(rsLaunch, 'rsLaunch') + ' ' + lisTmp.Items.Strings[0];
    end;
  end;
  tbtnHyperlink.Enabled := optHyperlink.Enabled;

  fIsMinimized := False; // minimized from the password input dialog box?
  if DatabaseLocked then // database is locked, enter the password to unlock it
  begin
    grdTree.Visible := False;
    fldInfo.Visible := False;

    // we have 3 chances
    for i := 1 to 3 do
    begin
      if i = 1 then
        frmPassword.sPrompt := ilMiscStr(rsDataLockedPassword, 'rsDataLockedPassword')
      else
        frmPassword.sPrompt := ilMiscStr(rsBadPasswordTryAgain, 'rsBadPasswordTryAgain');
      frmPassword.sttBar.SimpleText := DataFile;
      frmPassword.fGenPassword := False;
      frmPassword.fBtnMinimizeVisible := True;
      frmPassword.ShowModal;
      case frmPassword.pwaRet of
        PWA_CANCEL:
              begin
                Break; // for
              end;
        PWA_MINIMIZE:
              begin
                Application.Minimize;
                fIsMinimized := True;
                Break; // for
              end;
        PWA_ACCEPT:
              begin
                if (Password = frmPassword.sEnteredPassword) then
                begin
                  grdTree.Visible := True;
                  fldInfo.Visible := True;
                  DatabaseLocked := False;
                  Break; // for
                end;
              end;
      end; // case
    end; // for

    if DatabaseLocked and (not fIsMinimized) then // bad password
    begin
      PlayOneSound(sndERROR);
      if (not frmPassword.fCancel) then
        CstMessageDlg(ilMiscStr(rsBadPasswordClose, 'rsBadPasswordClose'), mtError, [mbOK], 0);
      DataDirty := False;
      Close;
    end;
  end;

  tmrInactivity.Enabled := True;
  fInUpdate := False;
end;
//------------------------------------------------------------------------------
// A file is dropped!
procedure TfrmMain.WMDROPFILES(var Message: TWMDROPFILES);
var
  buffer: array[0..255] of Char;
begin
  DragQueryFile(Message.Drop, 0, @buffer, sizeof(buffer)); // only the first one
  DoOpenFile(buffer, False);
end;
//------------------------------------------------------------------------------
// Detect all keyboard/mouse messages
procedure TfrmMain.OwnAppMessageHandler(var Msg: TMsg; var Handled: Boolean);
begin
  case Msg.Message of
    WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST: ActivityDetected;
  end;
  Handled := False;
end;
//------------------------------------------------------------------------------
// Activity detected, reset the timer
procedure TfrmMain.ActivityDetected;
begin
  m_Inactivity := 0;
end;
//------------------------------------------------------------------------------
// Inactivity timer tick
procedure TfrmMain.tmrInactivityTimer(Sender: TObject);
begin
  if MaxInactivity > 0 then // if not disabled auto-locking
  begin
    if not DatabaseLocked then
    begin
      Inc(m_Inactivity, tmrInactivity.Interval div 1000); // next time intrval passed
      if (m_Inactivity >= MaxInactivity) then // time over?
      begin
        m_Inactivity := 0;
        DatabaseLocked := True; // database is locked
        UpdateUI; // update all user interface elements
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
// Make all items visible
procedure TfrmMain.optExpandAllClick(Sender: TObject);
var
  i: Integer;
begin
  grdTree.IsUpdating := True; // prevent redrawing
  for i := 0 to grdTree.Items.Count-1 do
  begin
    grdTree.Items[i].Expand(True);
  end;
  grdTree.IsUpdating := False; // redraw the tree now
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Hide all items
procedure TfrmMain.optCollapseAllClick(Sender: TObject);
var
  i: Integer;
begin
  grdTree.IsUpdating := True; // prevent redrawing
  for i := 0 to grdTree.Items.Count-1 do
  begin
    grdTree.Items[i].Collapse(True);
  end;
  grdTree.IsUpdating := False; // redraw the tree now
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
// Count all items
var
  TmpCount: Integer;
function TfrmMain.ItemsCount: Integer;
  procedure IntIterate(Item: TElTreeItem; Index: Integer; var ContinueIterate: Boolean;
                       IterateData: Pointer; Tree: TCustomElTree);
  begin
    if Item <> nil then
      if Item.Parent <> nil then // not a category header
        Inc(TmpCount);
  end;
begin
  TmpCount := 0;
//QQtodo: count items
//  grdTree.Items.Iterate(False, @IntIterate, nil);
  ItemsCount := TmpCount;
end;
//------------------------------------------------------------------------------
// Rename the active category
procedure TfrmMain.optCatRenameClick(Sender: TObject);
var
  itmSel: TElTreeItem;
  sOldName, sNewName: String;
  i: Integer;
begin
  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
  begin
    if (itmSel.Parent = nil) then // it's a category
    begin
      sOldName := itmSel.Text;
      sNewName := itmSel.Text;
      // QQtodo: InputQuery's buttons can't be translated
      if InputQuery(ilMiscStr(rsRenCategory, 'rsRenCategory'),
                    ilMiscStr(rsEnterNewCategory, 'rsEnterNewCategory'),
                    sNewName) then
      begin
        if (sNewName <> itmSel.Text) then // name changed
        begin
          // ready, now perform renaming
          for i := 0 to grdTree.Items.Count - 1 do
          begin
            if UpperCase(grdTree.Items[i].Text) = UpperCase(sOldName) then
            begin
              grdTree.Items[i].Text := sNewName;
              DataDirty := True; // data modified
            end;
            if UpperCase(grdTree.Items[i].ColumnText[COL_CAT-1]) = UpperCase(sOldName) then
            begin
              grdTree.Items[i].ColumnText[COL_CAT-1] := sNewName;
              DataDirty := True; // data modified
            end;
          end;
        end;
        UpdateUI; // update all user interface elements
      end;
    end
    else
      CstMessageDlg(ilMiscStr(rsNoCategSelected, 'rsNoCategSelected'), mtError, [mbOk], 0);
  end
  else
    CstMessageDlg(ilMiscStr(rsNoCategSelected, 'rsNoCategSelected'), mtError, [mbOk], 0);
end;
//------------------------------------------------------------------------------
// Remove the whole category
procedure TfrmMain.optCatRemoveClick(Sender: TObject);
var
  itmSel: TElTreeItem;
  asw: Integer;
begin
  itmSel := grdTree.ItemFocused;
  if (itmSel <> nil) then
  begin
    if (itmSel.Parent = nil) then // it's a category
    begin
      asw := CstMessageDlg(ilMiscStr(rsDelWholeCateg, 'rsDelWholeCateg'), // Remove the whole category and its items?
                        mtConfirmation, [mbYes, mbNo], 0);
      if asw = mrYes then
      begin
        itmSel.Delete;
        DataDirty := True; // data modified
        UpdateUI; // update all user interface elements
      end;
    end
    else
      CstMessageDlg(ilMiscStr(rsNoCategSelected, 'rsNoCategSelected'), mtError, [mbOk], 0);
  end
  else
    CstMessageDlg(ilMiscStr(rsNoCategSelected, 'rsNoCategSelected'), mtError, [mbOk], 0);
end;
//------------------------------------------------------------------------------
// Icon in the system tray clicked, restore the application window
procedure TfrmMain.popoptRestoreClick(Sender: TObject);
begin
  outTrayIcon.ShowMainForm;
  UpdateUI; // update menu option checked status
end;
//------------------------------------------------------------------------------
// Tray icon clicked, restore the application window
procedure TfrmMain.outTrayIconClick(Sender: TObject);
begin
  outTrayIcon.ShowMainForm;
  UpdateUI; // update menu option checked status
end;
//------------------------------------------------------------------------------
// Adjust UI to the new size of the main window
procedure TfrmMain.FormResize(Sender: TObject);
begin
  with sttBar do
  begin
    Panels[PNL_HINT].Width := (Width - Panels[PNL_RECCNT].Width) div 2;
    Panels[PNL_FILE].Width := Width - Panels[PNL_HINT].Width - Panels[PNL_RECCNT].Width;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optViewTlbMainClick(Sender: TObject);
begin
  tlbMain.Visible := not tlbMain.Visible;
  UpdateUI; // update menu option checked status
end;
procedure TfrmMain.optViewSttBarClick(Sender: TObject);
begin
  sttBar.Visible := not sttBar.Visible;
  UpdateUI; // update menu option checked status
end;
procedure TfrmMain.optViewAllClick(Sender: TObject);
begin
  tlbMain.Visible := True;
  sttBar.Visible := True;
  UpdateUI; // update menu option checked status
end;
procedure TfrmMain.optViewNoneClick(Sender: TObject);
begin
  tlbMain.Visible := False;
  sttBar.Visible := False;
  UpdateUI; // update menu option checked status
end;
//------------------------------------------------------------------------------
// Select another language
procedure TfrmMain.optLanguageClick(Sender: TObject);
begin
  if frmLanguage <> nil then
  begin
    frmLanguage.RootDir := PgmDir;
    frmLanguage.InitLanguage := Language;
    frmLanguage.ShowModal;
    if Length(frmLanguage.SelectedLanguage) > 0 then
      Language := frmLanguage.SelectedLanguage;
    UpdateUI; // update menu option checked status
  end;
end;
//------------------------------------------------------------------------------
// handle hotkeys
var
  _PasteBff: String;
procedure TfrmMain.WMHotKey(var Msg: TWMHotKey);
var
  itm: TOneItem;
begin
  _PasteBff := '';
  itm := TOneItem.Create;

  if (Msg.HotKey = HKEY_OFS + 4) then
  begin
    if Clipboard.HasFormat(CF_TEXT) then
      _PasteBff := Clipboard.AsText; // HKCPaste
  end
  else // all other paste methods require an active record
  begin
    if SelRowAsItem(itm) then
    begin
      case (Msg.HotKey) of
        HKEY_OFS + 1: // HKSPaste - SuperPaste
                      begin
                        if (Length(Trim(itm.SuperPaste)) > 0) and
                           (AnsiUpperCase(itm.SuperPaste) <> AnsiUpperCase(S_DFTSUPERPASTE)) then
                          _PasteBff := itm.SuperPaste
                        else
                          _PasteBff := S_DFTSUPERPASTE; // '$USER{tab}$PASS{enter}'

                        _PasteBff := ReplStr('$USER'    , itm.User    , _PasteBff);
                        _PasteBff := ReplStr('$PASS'    , itm.Password, _PasteBff);
                        _PasteBff := ReplStr('$CATEGORY', itm.Category, _PasteBff);
                        _PasteBff := ReplStr('$SYSTEM'  , itm.System  , _PasteBff);
                        _PasteBff := ReplStr('$COMMENTS', itm.Comments, _PasteBff);
                        _PasteBff := ReplStr('$CUSTOM'  , itm.Custom  , _PasteBff);
                      end;
        HKEY_OFS + 2: _PasteBff := itm.User;     // HKUPaste
        HKEY_OFS + 3: _PasteBff := itm.Password; // HKPPaste
      end;
    end;
  end;

  if Length(_PasteBff) > 0 then
    tmrPaste.Enabled := True
  else
    PlayOneSound(sndERROR);
end;
//------------------------------------------------------------------------------
procedure TfrmMain.tmrPasteTimer(Sender: TObject);
begin
  tmrPaste.Enabled := False;

  // wait for releasing keys
  while (ShiftDown() or CtrlDown() or AltDown()) do
    Application.ProcessMessages;
  while (ShiftDown() or CtrlDown() or AltDown()) do
    Application.ProcessMessages;

  if (not DatabaseLocked) or PasteInLocked then
  begin
    SendKeys(PChar(_PasteBff), True);
    PlayOneSound(sndPASTE);
  end;
end;
//------------------------------------------------------------------------------
// Handle system messages to trap minimizing
procedure TfrmMain.WMSysCommand;
var
  asw: Integer;
begin
  case Msg.CmdType of
    SC_MINIMIZE:
                begin
                  if SaveOnMinim and DataDirty then // prompt to save any unsaved changes on minimize
                  begin
                    asw := CstMessageDlg(ilMiscStr(rsDataNotSaveMinim, 'rsDataNotSaveMinim'), mtConfirmation, mbYesNoCancel, 0);
                    if asw = mrYes then
                      optFileSaveClick(Nil);
                  end;
                  tmrInactivity.Enabled := False;
                end;
    SC_RESTORE, SC_MAXIMIZE:
                tmrInactivity.Enabled := True;
  end;

  inherited;
end;
//------------------------------------------------------------------------------
// We are about to minimize to the task bar/system tray
procedure TfrmMain.outTrayIconMinimizeToTray(Sender: TObject);
begin
  if ClipClrMini then // clear the clipboard on minimize?
    Clipboard.Clear;

  if LockOnMinimize then
    DatabaseLocked := True; // database is locked

  tmrInactivity.Enabled := False;
end;
//------------------------------------------------------------------------------
// We have been restored from the task bar/system tray
// QQtodo: implement in CoolTrayIcon
// procedure TfrmMain.outSysTrayAfterRestore(Sender: TObject);
// begin
//   tmrInactivity.Enabled := True;
//   UpdateUI; // update all user interface elements
//   // this will enforce password input in case database is locked
// end;
//------------------------------------------------------------------------------
// Handle application messages to trap restoring from the taskbar
(*
procedure TfrmMain.AppMessage(var msgApp: TMsg; var fHandled: Boolean);
begin
  if msgApp.message = WM_SYSCOMMAND then
  begin
    if msgApp.wParam = SC_MINIMIZE then
    begin
                  if LockOnMinimize then
                    m_DatabaseLocked := True; // database is locked
//      ShowWindow(Handle, SW_MINIMIZE);
      fHandled := False;
    end
    else if msgApp.wParam = SC_RESTORE then
    begin
      fHandled := True;
      ShowWindow(Handle, SW_RESTORE);
      UpdateUI; // update all user interface elements
    end;
  end;

  inherited;
end;
*)
//------------------------------------------------------------------------------
// Character map tool
procedure TfrmMain.optCharMapClick(Sender: TObject);
begin
  frmCharMap.ShowInsert := False;
  frmCharMap.ShowModal;
end;
//------------------------------------------------------------------------------
procedure TfrmMain.ShowHelpPage(sTopic: String; hwndCaller: HWND);
begin
  if (hwndCaller <> 0) then
    HtmlHelp(hwndCaller, Pchar(Application.HelpFile), 0, Longint(PChar(sTopic)))
  else
    HtmlHelp(Handle, Pchar(Application.HelpFile), 0, Longint(PChar(sTopic)));
end;
//------------------------------------------------------------------------------
// Restore the window placement
procedure TfrmMain.ReadWindowPosition(frm: TForm; fRestoreSize: Boolean);
begin
  if m_SaveSettingsToRegistry then
    ReadWindowPos(m_regFile, frm, frm.Name, fRestoreSize)
  else
    ReadWindowPos(m_iniFile, frm, frm.Name, fRestoreSize);
end;
//------------------------------------------------------------------------------
// Store the window placement
procedure TfrmMain.SaveWindowPosition(frm: TForm; fSaveSize: Boolean);
begin
  if m_SaveSettingsToRegistry then
    SaveWindowPos(m_regFile, frm, frm.Name, fSaveSize)
  else
    SaveWindowPos(m_iniFile, frm, frm.Name, fSaveSize);
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optFileWipingClick(Sender: TObject);
begin
  frmFileWipe.ShowModal;
end;
//------------------------------------------------------------------------------
var
  SortOnCol: Integer; // the column used for sorting
procedure TfrmMain.grdTreeHeaderColumnClick(Sender: TObject; SectionIndex: Integer);
var
  i: Integer;
begin
//ErrLog.WriteString('>>> HeaderColumnClick(' + IntToStr(SectionIndex) + ')');

  if (SortOnCol = SectionIndex) then // the same column, swap the order
  begin
    if grdTree.SortDir = sdAscend then
    begin
      grdTree.HeaderSections[SectionIndex].SortMode := hsmDescend;
      grdTree.SortDir := sdDescend;
    end
    else
    begin
      grdTree.HeaderSections[SectionIndex].SortMode := hsmAscend;
      grdTree.SortDir := sdAscend;
    end;
  end
  else // another column, preserve the order
  begin
    if grdTree.SortDir = sdAscend then
      grdTree.HeaderSections[SectionIndex].SortMode := hsmAscend
    else
      grdTree.HeaderSections[SectionIndex].SortMode := hsmDescend;
  end;

  SortOnCol := SectionIndex;

  if SortOnCol = COL_CAT then // sort the whole tree
  begin
    grdTree.Sort(False);
  end
  else // sort on specified column
  begin
    for i := 0 to grdTree.Items.Count - 1 do
      if grdTree.Items[i].Parent = Nil then // a category
        grdTree.Items[i].Sort(False);
  end;
end;
//------------------------------------------------------------------------------
// Grid items comparing function, used for sorting
procedure TfrmMain.grdTreeCompareItems(Sender: TObject; Item1, Item2: TElTreeItem; var res: Integer);
var
  dte1, dte2: TDateTime;
  txt1, txt2: String;
begin
  if SortOnCol < 1 then SortOnCol := 1;
  if (Item1.Parent <> nil) and (Item2.Parent <> nil) then // both are items
  begin
    txt1 := Item1.ColumnText[SortOnCol - 1];
    txt2 := Item2.ColumnText[SortOnCol - 1];

//ErrLog.WriteString('   Compare [' + txt1 + '] and [' +  txt2 + '] from col:' + IntToStr(SortOnCol), False);
    if SortOnCol in [COL_STA, COL_EXP] then // date columns
    begin
      dte1 := CvtStr2Date(txt1);
      dte2 := CvtStr2Date(txt2);
      if dte1 > dte2 then
        res := 1
      else if dte1 < dte2 then
        res := -1
      else
        res := 0;
    end
    else // text columns
    begin
      res := CompareText(txt1, txt2);
    end;
  end
  else if (Item1.Parent = nil) and (Item2.Parent = nil) then // both are categories
  begin
    res := CompareText(Item1.ColumnText[COL_CAT-1], Item2.ColumnText[COL_CAT-1]);
  end
  else // one item, one category
  begin
    res := 0;
  end;

//ErrLog.WriteInteger(' result: ', res);
end;
//------------------------------------------------------------------------------
function TfrmMain.ReadBool(sSec, sKey: String; fDefault: Boolean): Boolean;
begin
  if m_SaveSettingsToRegistry then
    Result := m_regFile.ReadBool(sSec, sKey, fDefault)
  else
    Result := m_iniFile.ReadBool(sSec, sKey, fDefault)
end;
function  TfrmMain.ReadInteger(sSec, sKey: String; nDefault: Integer): Integer;
begin
  if m_SaveSettingsToRegistry then
    Result := m_regFile.ReadInteger(sSec, sKey, nDefault)
  else
    Result := m_iniFile.ReadInteger(sSec, sKey, nDefault)
end;
function  TfrmMain.ReadString(sSec, sKey: String; sDefault: String): String;
begin
  if m_SaveSettingsToRegistry then
    Result := m_regFile.ReadString(sSec, sKey, sDefault)
  else
    Result := m_iniFile.ReadString(sSec, sKey, sDefault);
end;
procedure TfrmMain.WriteBool(sSec, sKey: String; fVal: Boolean);
begin
  if m_SaveSettingsToRegistry then
    m_regFile.WriteBool(sSec, sKey, fVal)
  else
    m_iniFile.WriteBool(sSec, sKey, fVal);
end;
procedure TfrmMain.WriteInteger(sSec, sKey: String; nVal: Integer);
begin
  if m_SaveSettingsToRegistry then
    m_regFile.WriteInteger(sSec, sKey, nVal)
  else
    m_iniFile.WriteInteger(sSec, sKey, nVal);
end;
procedure TfrmMain.WriteString(sSec, sKey: String; sVal: String);
begin
  if m_SaveSettingsToRegistry then
    m_regFile.WriteString(sSec, sKey, sVal)
  else
    m_iniFile.WriteString(sSec, sKey, sVal);
end;
//------------------------------------------------------------------------------
// Visit my home page
procedure TfrmMain.optVisitWebPageClick(Sender: TObject);
begin
  if VSN_STD then
    ShellExecute(Handle,'open','http://www.mirekw.com/index.html','','',sw_Normal)
  else if VSN_HSO then
    ShellExecute(Handle,'open','http://hso-usa.com','','',sw_Normal)
  else
    ShellExecute(Handle,'open','http://www.mirekw.com/index.html','','',sw_Normal);
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optDonateClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.mirekw.com/donate/donate.html ','','',sw_Normal);
end;
//------------------------------------------------------------------------------
procedure TfrmMain.optHelpClick(Sender: TObject);
begin
  ShowHelpPage('main.htm', Handle);
end;
//------------------------------------------------------------------------------
// Check for a new version
procedure TfrmMain.optCheckNewVersionClick(Sender: TObject);
begin
  frmSoftwareCheck.ThisProgram := 'PINs';
  frmSoftwareCheck.ThisVersion := lblVsn.GetLabelText + iifs(IsBeta, ' Beta', '');
  frmSoftwareCheck.ThisID      := '#PINS';
  frmSoftwareCheck.VersionFile := 'http://www.mirekw.com/versions.txt';

  frmSoftwareCheck.InetProxyAddress := InetProxyAddress;
  frmSoftwareCheck.InetProxyPort    := InetProxyPort;
  frmSoftwareCheck.InetUseProxy     := InetUseProxy;
  frmSoftwareCheck.ShowModal;
  InetProxyAddress := frmSoftwareCheck.InetProxyAddress;
  InetProxyPort    := frmSoftwareCheck.InetProxyPort;
  InetUseProxy     := frmSoftwareCheck.InetUseProxy;
end;
//------------------------------------------------------------------------------
// "Stay on top" handling
procedure TfrmMain.SetFormOnTop(onTop: Boolean);
begin
  if onTop then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  optStayOnTop.Checked := FormStyle = fsStayOnTop;
end;
procedure TfrmMain.optStayOnTopClick(Sender: TObject);
begin
  SetFormOnTop(FormStyle = fsNormal);
end;
//------------------------------------------------------------------------------
// "AutoExpand" property handling
function  TfrmMain.GetAutoExpand: Boolean;
begin
  Result := grdTree.AutoExpand;
end;
procedure TfrmMain.SetAutoExpand(fAuto: Boolean);
begin
  grdTree.AutoExpand := fAuto;
end;
//------------------------------------------------------------------------------
// Prepare items on the Info box popup
procedure TfrmMain.popupInfoPopup(Sender: TObject);
begin
  popoptCopySelection.Enabled := (Length(fldInfo.SelText) > 0);
  popoptActiveHyperlinks.Checked := ActiveHyperlinks;
end;
//------------------------------------------------------------------------------
// Put selected text in the Info box on the clipboard
procedure TfrmMain.popoptCopySelectionClick(Sender: TObject);
begin
  if Length(fldInfo.SelText) > 0 then
    Clipboard.SetTextBuf(PChar(fldInfo.SelText));
end;
//------------------------------------------------------------------------------
// Toggle activity of hyperlinks in the Info box
procedure TfrmMain.popoptActiveHyperlinksClick(Sender: TObject);
begin
  ActiveHyperlinks := not ActiveHyperlinks;
end;
//------------------------------------------------------------------------------
// Lock the database manually
procedure TfrmMain.acLockDatabaseExecute(Sender: TObject);
begin
  DatabaseLocked := True; // database is locked
  UpdateUI; // update all user interface elements
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

