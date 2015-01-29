{***************************************************************************}
{  TurboBird: FireBird database administration and management tool          }
{  Started by  : Motaz Abdel Azeem http://code.sd/                          }
{  Start development :  5.Dec.2009                                          }
{  Last updated      : 29.Jan.2015                                          }
{  License           : GPL for GUI, LGPL for Units                          }
{***************************************************************************}

program TurboBird;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Controls, memdslaz, main, CreateDb, Reg, QueryWindow, ViewView,
  ViewTrigger, ViewSProc, ViewGen, NewTable, NewGen, EnterPass, About,
  CreateTrigger, EditTable, CallProc, EditDataFullRec, UDFInfo, ViewDomain,
  NewDomain, SysTables, NewConstraint, NewEditField, Calen, Scriptdb,
  UserPermissions, TableManage, BackupRestore, CreateUser, ChangePass,
  PermissionManage, SQLHistory, CopyTable, dynlibs, ibase60dyn, dbInfo,
  sysutils, Comparison, Update, topologicalsort, UnitFirebirdServices, 
  turbocommon, importtable, fileimport, csvdocument, sqldblib;

const
  Major = 1;
  Minor = 2;
  Release = 1;

  VersionDate = '2010 - Jan 2015';
{$IFDEF Unix}
{$DEFINE extdecl:=cdecl}
    fbclib = 'libfbclient.' + sharedsuffix + '.2';
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
   fbclib = 'fbembed.dll'; //allows both embedded and client/server access
   seclib = 'fbclient.dll'; //only client/server access
   thirdlib = 'gds32.dll'; //could be Firebird, could be old Interbase library...
{$ENDIF}

{$R *.res}

var
  SAbout: TfmAbout;
  ErrorMessage: string;
  IBaseLibraryHandle : TLibHandle;
  {$IFDEF UNIX}
  SLib: TSQLDBLibraryLoader;
  {$ENDIF}
begin
  Application.Initialize;

  // Load library using SQLDBLibraryLoader in Linux, OSX,...
  {$IFDEF UNIX}
  SLib:= TSQLDBLibraryLoader.Create(nil);
  SLib.ConnectionType:= 'Firebird';
  SLib.LibraryName:= 'libfbclient.so.2'; //todo: is this correct for OSX?
  SLib.Enabled:= True;
  {$ENDIF}

  {$IFDEF DEBUG}
  // Requires the build mode to set -dDEBUG in Project Options/Other and
  // defining -gh/heaptrace on
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}
  IBaseLibraryHandle:= LoadLibrary(fbclib);

  // search for all compatible FireBird libraries in Windows
  {$IFDEF Windows}
  if IBaseLibraryHandle = NilHandle then
    IBaseLibraryHandle:= LoadLibrary(seclib);
  if IBaseLibraryHandle = NilHandle then
    IBaseLibraryHandle:= LoadLibrary(thirdlib);
  {$ENDIF}

  // Check Firebird library existence
  if (IBaseLibraryHandle = nilhandle) then
  begin
    ErrorMessage:= Format('Unable to load Firebird library: %s.' + LineEnding +
      'Please follow the Firebird documentation to install the Firebird client on your system.',
      [fbclib]);
    {$IFDEF WINDOWS}
    // More libraries and additional hint
    ErrorMessage:= Format('Unable to load Firebird library: %s.' + LineEnding +
      'Please follow the Firebird documentation to install the Firebird client on your system.' + LineEnding +
      'Hint: you could copy the fbclient/fbembed.dll and associated dlls into the TurboBird directory.',
      [fbclib+'/'+seclib+'/'+thirdlib]);
    {$ENDIF}
    Application.MessageBox(PChar(ErrorMessage), 'Warning', 0);
  end;

  SAbout:= TfmAbout.Create(nil);
  SAbout.BorderStyle:= bsNone;
  SAbout.BitBtn1.Visible:= False;
  SAbout.Show;
  Application.ProcessMessages;
  SAbout.Update;
  Application.CreateForm(TfmMain, fmMain);
  fmMain.Version:= Format('%d.%d.%d', [Major, Minor, Release]);
  fmMain.StatusBar1.Panels[1].Text:= 'Version: ' + fmMain.Version;
  fmMain.VersionDate:= VersionDate;
  fmMain.Major:= Major;
  fmMain.Minor:= Minor;
  fmMain.ReleaseVersion:= Release;
  Application.CreateForm(TfmCreateDB, fmCreateDB);
  Application.CreateForm(TfmReg, fmReg);
  Application.CreateForm(TfmNewGen, fmNewGen);
  Application.CreateForm(TfmEnterPass, fmEnterPass);
  Application.CreateForm(TfmCreateTrigger, fmCreateTrigger);
  Application.CreateForm(TfmEditTable, fmEditTable);
  Application.CreateForm(TfmCallProc, fmCallProc);
  Application.CreateForm(TfmEditDataFullRec, fmEditDataFullRec);
  Application.CreateForm(TfmNewDomain, fmNewDomain);
  Application.CreateForm(TdmSysTables, dmSysTables);
  Application.CreateForm(TfmNewConstraint, fmNewConstraint);
  Application.CreateForm(TfmCalen, fmCalen);
  Application.CreateForm(TfmBackupRestore, fmBackupRestore);
  Application.CreateForm(TfmCreateUser, fmCreateUser);
  Application.CreateForm(TfmChangePass, fmChangePass);
  Application.CreateForm(TfmSQLHistory, fmSQLHistory);
  Application.CreateForm(TfmCopyTable, fmCopyTable);
  SAbout.Free;
  InitialiseIBase60;
  Application.Run;
  ReleaseIBase60;
end.
