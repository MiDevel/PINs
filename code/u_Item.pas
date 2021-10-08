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

unit u_Item;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ElTree, Menus;

resourcestring
  rsRecAdd='Add a new record';
  rsRecEdit='Edit the record';

type
  TModItemMode = (MIM_EDIT, MIM_ADD);
  TOneItem = class
  private { Private declarations }
    m_Category: String;
    m_User: String;
    m_System: String;
    m_Password: String;
    m_Comments: String;
    m_Custom: String;
    m_StartDate: String;
    m_Expires: String;
    m_Info: String;
    m_SuperPaste: String;

  published
    property Category  : String read m_Category   write m_Category;
    property User      : String read m_User       write m_User;
    property System    : String read m_System     write m_System;
    property Password  : String read m_Password   write m_Password;
    property Comments  : String read m_Comments   write m_Comments;
    property Custom    : String read m_Custom     write m_Custom;
    property StartDate : String read m_StartDate  write m_StartDate;
    property Expires   : String read m_Expires    write m_Expires;
    property Info      : String read m_Info       write m_Info;
    property SuperPaste: String read m_SuperPaste write m_SuperPaste;

  public { Public declarations }
    constructor Create; overload;
    constructor Create(sCat, sUsr, sSys, sPsw, sCmm, sCst, sStD, sExp, sInf, sPaste: String); overload;
    constructor Clear;
    procedure FromString(sTxt: String);
    procedure FromTreeItem(itm: TElTreeItem);
    function  AsString: String;
  end;

  TfrmItem = class(TForm)
    outlblCategory: TLabel;
    outlblUserName: TLabel;
    fldUserName: TEdit;
    outlblSystem: TLabel;
    fldSystem: TEdit;
    outlblPassword: TLabel;
    outlblStartDate: TLabel;
    fldPassword: TEdit;
    outlblComments: TLabel;
    fldRetype: TEdit;
    lblRetype: TLabel;
    outlblMoreInfo: TLabel;
    fldComments: TEdit;
    fldStartDate: TEdit;
    outlblExpires: TLabel;
    fldExpires: TEdit;
    cmbCategory: TComboBox;
    btnGenPassword: TSpeedButton;
    btnPickDate2: TSpeedButton;
    btnPickDate: TSpeedButton;
    Image1: TImage;
    fldInfo: TMemo;
    lblHintCategory: TLabel;
    lblHintMoreInfo: TLabel;
    outlblCustom: TLabel;
    fldCustom: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    chkPswVisible: TCheckBox;
    btnCharMap: TSpeedButton;
    btnHelp: TButton;
    lblSuperPaste: TLabel;
    fldSuperPaste: TEdit;
    popupSuperPaste: TPopupMenu;
    btnSuperPaste: TButton;
    outpopoptUser: TMenuItem;
    outpopoptPassword: TMenuItem;
    outpopoptCategory: TMenuItem;
    outpopoptSystem: TMenuItem;
    outpopoptComments: TMenuItem;
    outpopoptCustom: TMenuItem;
    N1: TMenuItem;
    outpopoptKeyTab: TMenuItem;
    outpopoptKeyEnter: TMenuItem;
    btnSPasteDefault: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnPickDateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGenPasswordClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnPickDate2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure chkPswVisibleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure fldPasswordEnter(Sender: TObject);
    procedure fldRetypeEnter(Sender: TObject);
    procedure btnCharMapClick(Sender: TObject);
    procedure CtlNonPswEnter(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure popoptSuperPasteClick(Sender: TObject);
    procedure btnSPasteDefaultClick(Sender: TObject);
    procedure btnSuperPasteClick(Sender: TObject);

  private { Private declarations }
    m_item: TOneItem;
    m_edtFld: TEdit; //

  public  { Public declarations }
    IsAccepted: Boolean;
    procedure Init(itm: TOneItem; mde: TModItemMode);
    function GetResult: TOneItem;
  end;

var
  frmItem: TfrmItem;

implementation

uses
  u_Main, uMWStrings, u_Calendar, u_GenPassword, IniLang, u_CharMap,
  u_Columns;

{$R *.DFM}

//------------------------------------------------------------------------------
constructor TOneItem.Clear;
begin
  Category   := '';
  User       := '';
  System     := '';
  Password   := '';
  Comments   := '';
  Custom     := '';
  StartDate  := '';
  Expires    := ilMiscStr(rsNever, 'rsNever');
  Info       := '';
  SuperPaste := '';
end;
//------------------------------------------------------------------------------
constructor TOneItem.Create;
begin
  Clear;
end;
//------------------------------------------------------------------------------
constructor TOneItem.Create(sCat, sUsr, sSys, sPsw, sCmm, sCst, sStD, sExp, sInf, sPaste: String);
begin
  Category  := sCat;
  User      := sUsr;
  System    := sSys;
  Password  := sPsw;
  Comments  := sCmm;
  Custom    := sCmm;
  StartDate := sStD;
  Expires   := sExp;
  Info      := sInf;
end;
//------------------------------------------------------------------------------
procedure TOneItem.FromString(sTxt: String);
begin
  Category   := GetValueStartingWith(sTxt, '$CAT=', Chr(127));
  User       := GetValueStartingWith(sTxt, '$USR=', Chr(127));
  System     := GetValueStartingWith(sTxt, '$SYS=', Chr(127));
  Password   := GetValueStartingWith(sTxt, '$PSW=', Chr(127));
  Comments   := GetValueStartingWith(sTxt, '$CMM=', Chr(127));
  Custom     := GetValueStartingWith(sTxt, '$CST=', Chr(127));
  StartDate  := GetValueStartingWith(sTxt, '$STD=', Chr(127));
  Expires    := GetValueStartingWith(sTxt, '$EXP=', Chr(127));
  Info       := GetValueStartingWith(sTxt, '$INF=', Chr(127));
  SuperPaste := GetValueStartingWith(sTxt, '$SPS=', Chr(127));
end;
//------------------------------------------------------------------------------
procedure TOneItem.FromTreeItem(itm: TElTreeItem);
begin
  Clear;
  if (itm <> nil) then
  begin
    Category   := itm.ColumnText[COL_CAT-1];
    System     := itm.ColumnText[COL_SYS-1];
    User       := itm.ColumnText[COL_USR-1];
    Password   := itm.ColumnText[COL_PSW-1];
    Comments   := itm.ColumnText[COL_CMM-1];
    Custom     := itm.ColumnText[COL_CST-1];
    StartDate  := itm.ColumnText[COL_STA-1];
    Expires    := itm.ColumnText[COL_EXP-1];
    Info       := itm.ColumnText[COL_INF-1];
    SuperPaste := itm.ColumnText[COL_SPS-1];
  end;
end;
//------------------------------------------------------------------------------
function TOneItem.AsString: String;
var
  sRet: String;
begin
  sRet :=
    '$CAT=' + Category  + Chr(127) +
    '$USR=' + User      + Chr(127) +
    '$SYS=' + System    + Chr(127) +
    '$PSW=' + Password  + Chr(127) +
    '$CMM=' + Comments  + Chr(127) +
    '$CST=' + Custom    + Chr(127) +
    '$STD=' + StartDate + Chr(127) +
    '$EXP=' + Expires   + Chr(127) +
    '$INF=' + Info;

  if (Length(Trim(SuperPaste)) > 0) and
     (AnsiUpperCase(SuperPaste) <> AnsiUpperCase(S_DFTSUPERPASTE)) then
    sRet := sRet + Chr(127) + '$SPS=' + SuperPaste;

  Result := sRet;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmItem.FormCreate(Sender: TObject);
begin
  frmMain.ReadWindowPosition(self, False);
  m_item := TOneItem.Create;
end;
procedure TfrmItem.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
procedure TfrmItem.Init(itm: TOneItem; mde: TModItemMode);
var
  i: Integer;
  lisCat: TStringList;
begin
  case mde of
    MIM_EDIT:
      Caption := ilMiscStr(rsRecEdit, 'rsRecEdit'); // Edit record
    MIM_ADD:
      Caption := ilMiscStr(rsRecAdd, 'rsRecAdd'); // Add new record
  end;

  // fill the combo with existing categories
  lisCat := TStringList.Create;
  lisCat.Sorted := True;
  lisCat.Duplicates := dupIgnore;
  for i := 0 to frmMain.grdTree.Items.Count - 1 do
    if Length(frmMain.grdTree.Items[i].Text) > 0 then
      lisCat.Add(frmMain.grdTree.Items[i].Text);
  cmbCategory.Clear;
  cmbCategory.Items.AddStrings(lisCat);
  lisCat.Free;

  m_item := itm;
  IsAccepted := False;

  cmbCategory.Text  := itm.Category;
  fldUserName.Text  := itm.User;
  fldSystem.Text    := itm.System;
  fldPassword.Text  := itm.Password;
  fldRetype.Text    := itm.Password;
  fldComments.Text  := itm.Comments;
  fldCustom.Text    := itm.Custom;
  fldStartDate.Text := itm.StartDate;
  fldExpires.Text   := itm.Expires;
  fldInfo.Text      := itm.Info;

  if (Length(Trim(itm.SuperPaste)) = 0) then
    fldSuperPaste.Text := S_DFTSUPERPASTE
  else
    fldSuperPaste.Text := itm.SuperPaste;

  frmItem.ActiveControl := cmbCategory;
end;
//------------------------------------------------------------------------------
function TfrmItem.GetResult: TOneItem;
begin
  GetResult := m_item;
end;
//------------------------------------------------------------------------------
// OK button clicked
procedure TfrmItem.btnOkClick(Sender: TObject);
begin
  if fldPassword.Text = fldRetype.Text then
  begin
    cmbCategory.Text := Trim(cmbCategory.Text);
    if Length(cmbCategory.Text) = 0 then
      cmbCategory.Text := '???';
    m_item.Category   := cmbCategory.Text;
    m_item.User       := fldUserName.Text;
    m_item.System     := fldSystem.Text;
    m_item.Password   := fldPassword.Text;
    m_item.Password   := fldRetype.Text;
    m_item.Comments   := fldComments.Text;
    m_item.Custom     := fldCustom.Text;
    m_item.StartDate  := fldStartDate.Text;
    m_item.Expires    := fldExpires.Text;
    m_item.Info       := fldInfo.Text;

    if Length(Trim(fldSuperPaste.Text)) > 0 then
      m_item.SuperPaste := Trim(fldSuperPaste.Text);

    IsAccepted := True;
    Close;
  end
  else
    frmMain.CstMessageDlg(ilMiscStr(rsPassDontMatch, 'rsPassDontMatch'), mtError, [mbOk], 0);
end;
//------------------------------------------------------------------------------
// Cancel button clicked
procedure TfrmItem.btnCancelClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
// Date selection
procedure TfrmItem.btnPickDateClick(Sender: TObject);
begin
  if IsStrValidDate(fldStartDate.Text) then
    frmCalendar.dtSelectedDate := CvtStr2Date(fldStartDate.Text)
  else
    frmCalendar.dtSelectedDate := now;
  frmCalendar.ShowModal;
  if frmCalendar.fSelected then
    fldStartDate.Text := DateToStr(frmCalendar.dtSelectedDate);
end;
procedure TfrmItem.btnPickDate2Click(Sender: TObject);
begin
  if IsStrValidDate(fldExpires.Text) then
    frmCalendar.dtSelectedDate := CvtStr2Date(fldExpires.Text)
  else
    frmCalendar.dtSelectedDate := now;
  frmCalendar.ShowModal;
  if frmCalendar.fSelected then
    fldExpires.Text := DateToStr(frmCalendar.dtSelectedDate);
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmItem.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
// Generate the password
procedure TfrmItem.btnGenPasswordClick(Sender: TObject);
begin
  frmGenPassword.Init(True);
  frmGenPassword.ShowModal;
  if (frmGenPassword.Accepted) then
  begin
    fldPassword.Text := frmGenPassword.Password;
    fldRetype.Text := fldPassword.Text;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItem.FormActivate(Sender: TObject);
begin
  if frmMain.ShowPswFld then
    fldPassword.PasswordChar := #0
  else
    fldPassword.PasswordChar := '*';
  fldRetype.PasswordChar := fldPassword.PasswordChar;
  chkPswVisible.Checked  := False;
  chkPswVisible.Visible  := not frmMain.ShowPswFld;

  outlblCategory.Caption  := GlbColumns[COL_CAT].Caption + ':';
  outlblSystem.Caption    := GlbColumns[COL_SYS].Caption + ':';
  outlblUserName.Caption  := GlbColumns[COL_USR].Caption + ':';
  outlblPassword.Caption  := GlbColumns[COL_PSW].Caption + ':';
  outlblComments.Caption  := GlbColumns[COL_CMM].Caption + ':';
  outlblCustom.Caption    := GlbColumns[COL_CST].Caption + ':';
  outlblStartDate.Caption := GlbColumns[COL_STA].Caption + ':';
  outlblExpires.Caption   := GlbColumns[COL_EXP].Caption + ':';
  outlblMoreInfo.Caption  := GlbColumns[COL_INF].Caption + ':';

  // Translate SuperPaste tokens popup menu
  outpopoptUser.Caption     := GlbColumns[COL_USR].Caption + Chr(9) + outpopoptUser.Hint;
  outpopoptPassword.Caption := GlbColumns[COL_PSW].Caption + Chr(9) + outpopoptPassword.Hint;;
  outpopoptCategory.Caption := GlbColumns[COL_CAT].Caption + Chr(9) + outpopoptCategory.Hint;;
  outpopoptSystem.Caption   := GlbColumns[COL_SYS].Caption + Chr(9) + outpopoptSystem.Hint;;
  outpopoptComments.Caption := GlbColumns[COL_CMM].Caption + Chr(9) + outpopoptComments.Hint;;
  outpopoptCustom.Caption   := GlbColumns[COL_CST].Caption + Chr(9) + outpopoptCustom.Hint;
  outpopoptKeyTab.Caption   := '<Tab>'                     + Chr(9) + outpopoptKeyTab.Hint;
  outpopoptKeyEnter.Caption := '<Enter>'                   + Chr(9) + outpopoptKeyEnter.Hint;
end;
//------------------------------------------------------------------------------
procedure TfrmItem.chkPswVisibleClick(Sender: TObject);
begin
  if chkPswVisible.Checked then
    fldPassword.PasswordChar := #0
  else
    fldPassword.PasswordChar := '*';
  fldRetype.PasswordChar := fldPassword.PasswordChar;
end;
//------------------------------------------------------------------------------
procedure TfrmItem.fldPasswordEnter(Sender: TObject);
begin
  m_edtFld := fldPassword;
end;
procedure TfrmItem.fldRetypeEnter(Sender: TObject);
begin
  m_edtFld := fldRetype;
end;
procedure TfrmItem.CtlNonPswEnter(Sender: TObject);
begin
  m_edtFld := Nil;
end;
//------------------------------------------------------------------------------
procedure TfrmItem.btnCharMapClick(Sender: TObject);
begin
  frmCharMap.ShowInsert := (m_edtFld <> Nil);
  frmCharMap.ShowModal;
  if (m_edtFld <> Nil) then
    if Length(frmCharMap.SelectedChars) > 0 then
      m_edtFld.Text := m_edtFld.Text + frmCharMap.SelectedChars;
end;
//------------------------------------------------------------------------------
procedure TfrmItem.btnHelpClick(Sender: TObject);
begin
  frmMain.ShowHelpPage('dlg_record.htm', Handle);
end;
//------------------------------------------------------------------------------
// Add a SuperPaste token
procedure TfrmItem.popoptSuperPasteClick(Sender: TObject);
var
  sText: String;
begin
  sText := Trim(TMenuItem(Sender).Hint);
  if Length(sText) > 0 then
    fldSuperPaste.SelText := sText;
end;
//------------------------------------------------------------------------------
// Reset the SuperPaste string
procedure TfrmItem.btnSPasteDefaultClick(Sender: TObject);
begin
  fldSuperPaste.Text := S_DFTSUPERPASTE;
end;
//------------------------------------------------------------------------------
// Pop out the SuperTaste menu
procedure TfrmItem.btnSuperPasteClick(Sender: TObject);
begin
  popupSuperPaste.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------



end.
