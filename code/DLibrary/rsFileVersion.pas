unit rsFileVersion;
{*******************************************************************************
Unit:        rsFileVersion
Author:      Michael Burton
             Copyright © 1999 Rimrock Software
             All rights reserved.
Date:        February 3, 1999
Use with:    Delphi 2, 3, 4 only
Description: Describes an object called TrsFileVersion that will retrieve
             version and build information from 32-bit executable files (exe,
             dll, ocx, vbx, vxd, drv, pdr, mpd, etc), if that information is
             present in the file.
Maintenance:
********************************************************************************}

interface

uses Windows, Classes, SysUtils;

type
  TrsFileVersion = class(TObject)
  private
    { Private declarations }
    FVersion: string;  {concatenation of major.minor.release}
    FMajor:   Integer; {major version number}
    FMinor:   Integer; {minor version number}
    FRelease: Integer; {release version number}
    FBuild:   Integer; {build number}
    procedure SetDefaultProperties;
    function  ReadVersionInfo(sProgram: string; Major, Minor,
                              Release, Build : pWord) :Boolean;
  public
    { Public declarations }
    property    Version: string  read FVersion;
    property    Major:   Integer read FMajor   default  0;
    property    Minor:   Integer read FMinor   default  0;
    property    Release: Integer read FRelease default  0;
    property    Build:   Integer read FBuild   default  0;
    constructor Create;
    function    GetFileVersion(sFile: string): boolean;
  end;

implementation

{********************************************************************
Function    : Create
Date        : February 1, 1999
Description : Initialize variables
Inputs      : None
Outputs     : None
********************************************************************}
constructor TrsFileVersion.Create;
begin
  inherited Create;
  SetDefaultProperties;
end;

{********************************************************************
Function    : GetFileVersion
Date        : February 3, 1999
Description : Get the version of an executable file
Inputs      : sfile - The path/filename of the file to get a
              version for
Outputs     : True if there was version info, else false.
              If true, the version info will be in the object
              properties.
********************************************************************}
function TrsFileVersion.GetFileVersion(sFile: string): boolean;
var
  Major,Minor, Release, Build : Word;
begin
  {get version information from file sFile}
  Result := ReadVersionInfo(sFile, @Major, @Minor, @Release, @Build);
  if Result then begin
    FMajor   := Integer(Major);   {set major version property}
    FMinor   := Integer(Minor);   {set minor version property}
    FRelease := Integer(Release); {set release version property}
    FBuild   := Integer(Build);   {set build version property}
    FVersion := IntToStr(FMajor) + '.' + IntToStr(FMinor) + '.' +
                IntToStr(FRelease) + '.' + IntToStr(FBuild);
  end else begin
    SetDefaultProperties;
  end;
end;

{********************************************************************
Function    : SetDefaultProperties
Date        : February 1, 1999
Description : set the properties to their default values
Inputs      : None.
Outputs     : None.
********************************************************************}
procedure TrsFileVersion.SetDefaultProperties;
begin
  FVersion := '';
  FMajor :=   0;
  FMinor :=   0;
  FRelease := 0;
  FBuild :=   0;
end;

{********************************************************************
Function    : ReadVersionInfo
Date        : February 1, 1999
Description : Read the version and build info from an executable
Inputs      : sProgram - the name of the file to read
Outputs     : Major - the major version number
              Minor - the minor version number
              Release - the release number
              Build - the build number
********************************************************************}
function TrsFileVersion.ReadVersionInfo(sProgram: string; Major, Minor,
                                        Release, Build : pWord) :Boolean;
var
  Info:       PVSFixedFileInfo;
{$ifdef VER120} {Delphi 4 definition for this differs from D2 & D3}
  InfoSize:   Cardinal;
{$else}
  InfoSize:   UINT;
{$endif}
  nHwnd:      DWORD;
  BufferSize: DWORD;
  Buffer:     Pointer;
begin
  BufferSize := GetFileVersionInfoSize(pchar(sProgram),nHWnd); {Get buffer size}
  Result := True;
  if BufferSize <> 0 then begin {if zero, there is no version info}
    GetMem( Buffer, BufferSize); {allocate buffer memory}
    try
      if GetFileVersionInfo(PChar(sProgram),nHWnd,BufferSize,Buffer) then begin
        {got version info}
        if VerQueryValue(Buffer, '\', Pointer(Info), InfoSize) then begin
          {got root block version information}
          if Assigned(Major) then begin
            Major^ := HiWord(Info^.dwFileVersionMS); {extract major version}
          end;
          if Assigned(Minor) then begin
            Minor^ := LoWord(Info^.dwFileVersionMS); {extract minor version}
          end;
          if Assigned(Release) then begin
            Release^ := HiWord(Info^.dwFileVersionLS); {extract release version}
          end;
          if Assigned(Build) then begin
            Build^ := LoWord(Info^.dwFileVersionLS); {extract build version}
          end;
        end else begin
          Result := False; {no root block version info}
        end;
      end else begin
        Result := False; {couldn't extract version info}
      end;
    finally
      FreeMem(Buffer, BufferSize); {release buffer memory}
    end;
  end else begin
    Result := False; {no version info at all in the file}
  end;
end;

end.
