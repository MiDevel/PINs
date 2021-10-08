{*****************************************************************************
 *
 *  u_CharMap.pas - Pop-up character map
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

unit u_CharMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, ComCtrls, Buttons, StdCtrls, Clipbrd;

type
  TfrmCharMap = class(TForm)
    sttBar: TStatusBar;
    pnlBottom: TPanel;
    btnSelect: TButton;
    fldText: TEdit;
    btnClipCopy: TBitBtn;
    btnClose: TButton;
    pnlGrid: TPanel;
    grdChars: TStringGrid;
    pnlOneChar: TPanel;
    btnInsert: TButton;
    procedure pnlGridResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grdCharsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnClipCopyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure grdCharsDblClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure pnlBottomResize(Sender: TObject);
    procedure fldTextChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);

  private { Private declarations }
    procedure ShowChar(col, row: Integer);

  public { Public declarations }
    ShowInsert: Boolean;
    SelectedChars: String;
  end;

var
  frmCharMap: TfrmCharMap;

implementation

uses u_Main;

{$R *.DFM}


//------------------------------------------------------------------------------
function CalcAscii(col, row: Integer): Integer;
begin
  CalcAscii := row*16 + col + 32;
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.ShowChar(col, row: Integer);
begin
  pnlOneChar.Caption := Chr(CalcAscii(col, row));
  sttBar.SimpleText := ' "' + Chr(CalcAscii(col, row)) + '"' +
                      ', ASCII: ' + IntToStr(CalcAscii(col, row)) +
                      ', HEX: '   + Format('%x', [CalcAscii(col, row)]);
end;
//------------------------------------------------------------------------------
// Save / restore the dialog position
procedure TfrmCharMap.FormCreate(Sender: TObject);
var
  col, row: Integer;
begin
  frmMain.ReadWindowPosition(self, True);
  ShowInsert := False;
  pnlGrid.Align := alClient;
  grdChars.Align := alClient;

  for col := 0 to 15 do
  begin
    for row := 0 to 13 do
    begin
      grdChars.Cells[col, row] := Chr(CalcAscii(col, row));
    end;
  end;

  grdChars.Col := 0;
  grdChars.Row := 0;
end;
procedure TfrmCharMap.FormDestroy(Sender: TObject);
begin
  frmMain.SaveWindowPosition(self, True);
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.FormActivate(Sender: TObject);
begin
  btnInsert.Visible := ShowInsert;
  ShowChar(grdChars.Col, grdChars.Row);
  fldText.Text := '';
  SelectedChars := '';
  fldTextChange(Nil);
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.pnlGridResize(Sender: TObject);
begin
  grdChars.DefaultColWidth := pnlGrid.Width div (grdChars.ColCount + 1);
  grdChars.DefaultRowHeight := pnlGrid.Height div (grdChars.RowCount + 1);
  grdChars.Font.Height := grdChars.DefaultRowHeight - 1;
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.pnlBottomResize(Sender: TObject);
begin
  btnInsert.Left := pnlBottom.Width - btnInsert.Width - 4;
  btnClose.Left := btnInsert.Left;
end;
//------------------------------------------------------------------------------
// Another character in a grid selected
procedure TfrmCharMap.grdCharsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  ShowChar(ACol, ARow);
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.btnSelectClick(Sender: TObject);
begin
  fldText.Text := fldText.Text + Chr(CalcAscii(grdChars.Col, grdChars.Row));
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.grdCharsDblClick(Sender: TObject);
begin
  btnSelectClick(Nil);
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.btnClipCopyClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(fldText.Text));
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.btnInsertClick(Sender: TObject);
begin
  SelectedChars := fldText.Text;
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.btnCloseClick(Sender: TObject);
begin
  SelectedChars := '';
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmCharMap.fldTextChange(Sender: TObject);
begin
  btnInsert.Enabled := Length(fldText.Text) > 0;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmCharMap.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
