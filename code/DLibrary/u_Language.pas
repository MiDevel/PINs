{*****************************************************************************
 *
 *  u_Language.pas - Language selection form
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

unit u_Language;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, IniFiles;

resourcestring
  rsNoXltInfo='No translation info';

const
  S_LANGDIR = 'Lang'; // subfolder used for languages

type
  TfrmLanguage = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    sttBar: TStatusBar;
    Panel2: TPanel;
    outlblInfo1: TLabel;
    outlblInfo2: TLabel;
    outlblInfo3: TLabel;
    lisLanguages: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lisLanguagesClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lisLanguagesMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure lisLanguagesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lisLanguagesDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);

  private { Private declarations }
    procedure FillList;

  public { Public declarations }
    RootDir: String;          // set before calling the form
    InitLanguage: String;     // set before calling the form
    SelectedLanguage: String; // return value
  end;

var
  frmLanguage: TfrmLanguage;

implementation

uses
  uMWStrings, IniLang, u_Main;

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TfrmLanguage.FillList;
var
  sr: TSearchRec;
  sPath: String;
  i: Integer;

  procedure AddListItem(const sName: String);
  var
    bmp: TBitmap;
    bmpPath: String;
  begin
    bmpPath := MakePath(RootDir, S_LANGDIR + '\' + sr.Name);
    bmpPath := SwapFileExt(bmpPath, 'bmp');
    if not FileExists(bmpPath) then // try to cut off the language subversion digit
    begin
      bmpPath := LeftStr(bmpPath, Length(bmpPath) - 5) + RightStr(bmpPath, 4);
    end;
    if FileExists(bmpPath) then
    begin
      try
        bmp := TBitmap.Create;
        bmp.LoadFromFile(bmpPath);
        lisLanguages.Items.AddObject(sr.Name, TObject(bmp));
      except
        lisLanguages.Items.Add(sr.Name);
      end;
    end
    else
      lisLanguages.Items.Add(sr.Name);
  end;
begin
  sPath := MakePath(RootDir, S_LANGDIR + '\*.ini');
  lisLanguages.Clear;
  lisLanguages.Sorted := False;
  if FindFirst(sPath, faAnyFile, sr) = 0 then
  begin
    AddListItem(sr.Name);
    while FindNext(sr) = 0 do
      AddListItem(sr.Name);
    FindClose(sr);

    lisLanguages.Sorted := True;
    // locate the active language
    for i := 0 to lisLanguages.Items.Count - 1 do
      if lisLanguages.Items[i] = InitLanguage then
      begin
        lisLanguages.ItemIndex := i;
        lisLanguagesClick(nil);
        break;
      end;
  end;
end;
//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmLanguage.FormCreate(Sender: TObject);
begin
  lisLanguages.Align := alClient;
  frmMain.ReadWindowPosition(self, True);
end;
procedure TfrmLanguage.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, True);
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.FormActivate(Sender: TObject);
begin
  outlblInfo1.Caption := '';
  outlblInfo2.Caption := '';
  outlblInfo3.Caption := '';
  FillList;
  SelectedLanguage := '';
end;
//------------------------------------------------------------------------------
// One of languages has been clicked
procedure TfrmLanguage.lisLanguagesClick(Sender: TObject);
var
  idx: Integer;
  iniFile: TIniFile; // INI file
begin
  idx := lisLanguages.ItemIndex;
  if idx >= 0 then
  begin
    iniFile := TIniFile.Create(MakePath(RootDir, S_LANGDIR + '\' + lisLanguages.Items[idx]));
    outlblInfo1.Caption := iniFile.ReadString('General', 'Info1', ilMiscStr(rsNoXltInfo, 'rsNoXltInfo'));
    outlblInfo2.Caption := iniFile.ReadString('General', 'Info2', '');
    outlblInfo3.Caption := iniFile.ReadString('General', 'Info3', '');
    iniFile.Free;
  end
  else
  begin
    outlblInfo1.Caption := '';
    outlblInfo2.Caption := '';
    outlblInfo3.Caption := '';
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.btnOkClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lisLanguages.ItemIndex;
  if idx >= 0 then
    SelectedLanguage := lisLanguages.Items[idx]
  else
    SelectedLanguage := '';
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.btnCancelClick(Sender: TObject);
begin
  SelectedLanguage := '';
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.lisLanguagesDblClick(Sender: TObject);
begin
  btnOkClick(nil);
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.lisLanguagesMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
//var
//  Bitmap: TBitmap;
begin
{
  with Control as TListBox do
  begin
    Bitmap := TBitmap(Items.Objects[Index]);
    if Bitmap <> nil then
      if Bitmap.Height > Height then
        Height := Bitmap.Height;
  end;
}
  Height := 20 + 2; // bmps have fixed 20x36 pixels size
end;
//------------------------------------------------------------------------------
procedure TfrmLanguage.lisLanguagesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Bitmap: TBitmap; // temporary variable for the item’s bitmap
  //Offset: Integer; // text offset width
begin
  with (Control as TListBox).Canvas do  // draw on control canvas
  begin
    FillRect(Rect); // clear the rectangle
    //Offset := 2; // default offset
    Bitmap := TBitmap((Control as TListBox).Items.Objects[Index]); // get the bitmap
    if Bitmap <> nil then // a bitmap has been assigned
    begin
      BrushCopy(Bounds(Rect.Left + 2, Rect.Top + 1, Bitmap.Width, Bitmap.Height),
      		Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clWhite); // render bitmap
      //Offset := Bitmap.Width + 6; // add four pixels between bitmap and text
    end;
    //TextOut(Rect.Left + Offset, Rect.Top + 2, (Control as TListBox).Items[Index]); // the text
    TextOut(Rect.Left + 36, Rect.Top + 2, (Control as TListBox).Items[Index]); // the text
  end;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmLanguage.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
