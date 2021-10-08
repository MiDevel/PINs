{*****************************************************************************
 *  PINs
 *  u_Calendar.pas - A pop-up calendar
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

unit u_Calendar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons;

type
  TfrmCalendar = class(TForm)
    ctlCalendar: TMonthCalendar;
    Panel1: TPanel;
    btnSelect: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }
    dtSelectedDate: TDateTime;
    fSelected: Boolean;
  end;

var
  frmCalendar: TfrmCalendar;

implementation

{$R *.DFM}

//------------------------------------------------------------------------------
// Initialize form
procedure TfrmCalendar.FormCreate(Sender: TObject);
begin
  fSelected := False;
  dtSelectedDate := Now;
end;
//------------------------------------------------------------------------------
// Initial date
procedure TfrmCalendar.FormActivate(Sender: TObject);
begin
  ctlCalendar.Date := dtSelectedDate;
end;
//------------------------------------------------------------------------------
// Selected
procedure TfrmCalendar.btnSelectClick(Sender: TObject);
begin
  fSelected := True;
  dtSelectedDate := ctlCalendar.Date;
  frmCalendar.Close;
end;
//------------------------------------------------------------------------------
// Cancelled
procedure TfrmCalendar.btnCancelClick(Sender: TObject);
begin
  fSelected := False;
  frmCalendar.Close;
end;
//------------------------------------------------------------------------------
// Allow <Esc> to close the form
procedure TfrmCalendar.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then Close; // Escape
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
