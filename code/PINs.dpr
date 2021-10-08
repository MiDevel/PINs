program PINs;

uses
  Forms,
  u_Main in 'u_Main.pas' {frmMain},
  u_Item in 'u_Item.pas' {frmItem},
  u_Password in 'u_Password.pas' {frmPassword},
  u_About in 'u_About.pas' {frmAboutBox},
  u_Options in 'u_Options.pas' {frmOptions},
  u_Calendar in '..\DLibrary\u_Calendar.pas' {frmCalendar},
  uMWErrorLog in '..\DLibrary\uMWErrorLog.pas',
  uMWTools in '..\DLibrary\uMWTools.pas',
  uMWStrings in '..\DLibrary\uMWStrings.pas',
  u_Language in '..\DLibrary\u_Language.pas' {frmLanguage},
  u_CharMap in '..\DLibrary\u_CharMap.pas' {frmCharMap},
  uMWForms in '..\DLibrary\uMWForms.pas',
  u_GenPassword in 'u_GenPassword.pas' {frmGenPassword},
  u_Blowfish in '..\DLibrary\u_Blowfish.pas',
  u_Import in 'u_Import.pas' {frmImport},
  u_FileWipe in 'u_FileWipe.pas' {frmFileWipe},
  u_Sounds in 'u_Sounds.pas',
  u_Columns in 'u_Columns.pas',
  u_ProxySettings in '..\DLibrary\u_ProxySettings.pas' {frmProxySettings},
  u_HotKeys in 'u_HotKeys.pas' {frmHotkeys},
  u_ExpiredDates in 'u_ExpiredDates.pas' {frmExpiredDates},
  u_Export in 'u_Export.pas' {frmExport},
  u_SoftwareCheck in '..\DLibrary\u_SoftwareCheck.pas' {frmSoftwareCheck},
  uMWInternet in '..\DLibrary\uMWInternet.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'PINs';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmLanguage, frmLanguage);
  Application.CreateForm(TfrmItem, frmItem);
  Application.CreateForm(TfrmPassword, frmPassword);
  Application.CreateForm(TfrmAboutBox, frmAboutBox);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmCalendar, frmCalendar);
  Application.CreateForm(TfrmGenPassword, frmGenPassword);
  Application.CreateForm(TfrmImport, frmImport);
  Application.CreateForm(TfrmCharMap, frmCharMap);
  Application.CreateForm(TfrmFileWipe, frmFileWipe);
  Application.CreateForm(TfrmProxySettings, frmProxySettings);
  Application.CreateForm(TfrmHotkeys, frmHotkeys);
  Application.CreateForm(TfrmExpiredDates, frmExpiredDates);
  Application.CreateForm(TfrmExport, frmExport);
  Application.CreateForm(TfrmSoftwareCheck, frmSoftwareCheck);
  frmMain.TranslateUI;
  Application.Run;
end.
