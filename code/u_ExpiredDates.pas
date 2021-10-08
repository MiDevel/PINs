{*****************************************************************************
 *  PINs
 *  u_frmExpiredDates.pas - expired passwords
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

unit u_ExpiredDates;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls;

resourcestring
  rsCntPassToVerify='Count of passwords to verify:';

type
  TfrmExpiredDates = class(TForm)
    pnlButtons: TPanel;
    pnlLeft: TPanel;
    pnlTop: TPanel;
    pnlRight: TPanel;
    sttBar: TStatusBar;
    lvExpItems: TListView;
    btnRescan: TButton;
    btnClose: TButton;
    btnEdit: TButton;
    procedure pnlButtonsResize(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRescanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lvExpItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure lvExpItemsDblClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);

  private { Private declarations }
    procedure LocateSelected(Item: TListItem);
    procedure UpdateUI;

  public { Public declarations }
  end;

var
  frmExpiredDates: TfrmExpiredDates;

implementation

uses
  u_Main, u_Item, IniLang, uMWStrings, u_Columns;

{$R *.DFM}

//------------------------------------------------------------------------------
// Save / restore the dialog position and size
procedure TfrmExpiredDates.FormCreate(Sender: TObject);
begin
  frmMain.ReadWindowPosition(self, True);
end;
//------------------------------------------------------------------------------
procedure TfrmExpiredDates.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, False);
end;
//------------------------------------------------------------------------------
// Prepare the user interface
procedure TfrmExpiredDates.FormActivate(Sender: TObject);
begin
  lvExpItems.Columns.Items[2].Caption := GlbColumns[COL_CAT].Caption;
  lvExpItems.Columns.Items[3].Caption := GlbColumns[COL_SYS].Caption;
  lvExpItems.Columns.Items[4].Caption := GlbColumns[COL_USR].Caption;
  lvExpItems.Columns.Items[5].Caption := GlbColumns[COL_EXP].Caption;
end;
//------------------------------------------------------------------------------
procedure TfrmExpiredDates.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmExpiredDates.btnRescanClick(Sender: TObject);
var
  i: Integer;
  itm: TOneItem;
  iExpiredCnt: Integer;
  lvItem: TListItem;
begin
  itm := TOneItem.Create;
  iExpiredCnt := 0;
  lvExpItems.Items.Clear;
  for i := 0 to frmMain.grdTree.Items.Count-1 do
  begin
    if (frmMain.grdTree.Items[i].Parent <> nil) then // skip categories headers
    begin
      itm.FromTreeItem(frmMain.grdTree.Items[i]);
      if (Length(itm.Expires) > 0) and (itm.Expires <> ilMiscStr(rsNever, 'rsNever')) and (itm.Expires <> 'Never') then
        if IsStrValidDate(itm.Expires) then
          if CvtStr2Date(itm.Expires) <= (Now() + frmMain.ExpDateDays) then
          begin
            lvItem := lvExpItems.Items.Add;
            lvItem.Caption := IntToStr(i);
            lvItem.SubItems.Add(IntToStr(i));
            lvItem.SubItems.Add(itm.Category);
            lvItem.SubItems.Add(itm.System);
            lvItem.SubItems.Add(itm.User);
            lvItem.SubItems.Add(itm.Expires);
            Inc(iExpiredCnt);
          end;
    end;
  end;
  sttBar.SimpleText := ilMiscStr(rsCntPassToVerify, 'rsCntPassToVerify') + ' ' + IntToStr(iExpiredCnt);
  UpdateUI;
end;
//------------------------------------------------------------------------------
// Highlight in the main grid the selected item
procedure TfrmExpiredDates.LocateSelected(Item: TListItem);
var
  idx: Integer;
begin
  if (Item <> Nil) then
  begin
    idx := CvtStr2Int(Item.Subitems[0]);
    if (idx < frmMain.grdTree.Items.Count) then
    begin
      frmMain.grdTree.ItemFocused := frmMain.grdTree.Items[idx];
      frmMain.grdTree.EnsureVisible(frmMain.grdTree.ItemFocused);
    end;
  end;
end;
//------------------------------------------------------------------------------
// Any item selected, locate it in the main list
procedure TfrmExpiredDates.lvExpItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  LocateSelected(lvExpItems.Selected);
  UpdateUI;
end;
//------------------------------------------------------------------------------
// Edit the active item
procedure TfrmExpiredDates.btnEditClick(Sender: TObject);
begin
  if (lvExpItems.Selected <> Nil) then
  begin
    LocateSelected(lvExpItems.Selected);
    frmMain.optRecordEditClick(Nil);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmExpiredDates.lvExpItemsDblClick(Sender: TObject);
begin
  btnEditClick(Nil);
  UpdateUI;
end;
//------------------------------------------------------------------------------
// resize UI elements
procedure TfrmExpiredDates.pnlButtonsResize(Sender: TObject);
begin
  btnClose.Left := pnlButtons.Width - btnClose.Width - 4;
end;
//------------------------------------------------------------------------------
procedure TfrmExpiredDates.UpdateUI;
begin
  btnEdit.Enabled := (lvExpItems.Selected <> Nil);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------




end.
