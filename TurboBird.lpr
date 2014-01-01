{***************************************************************************}
{  TurboBird: FireBird database administration and management tool          }
{  Developed by: Motaz Abdel Azeem http://code.sd/                          }
{  Start development :  5.Dec.2009                                          }
{  Last updated      :  1.Jan.2014                                          }
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
  sysutils, Comparison, Update;

const
  Major = 1;
  Minor = 0;
  Release = 0;

  VersionDate = '2010 - January 2014';
{$IFDEF Unix}
{$DEFINE extdecl:=cdecl}
    fbclib = 'libfbclient.' + sharedsuffix;
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
   fbclib = 'fbclient.dll';
   seclib = 'gds32.dll';
   thirdlib = 'fbembed.dll';
{$ENDIF}

{$R *.res}

var
  SAbout: TfmAbout;
  IBaseLibraryHandle : TLibHandle;
begin
  Application.Initialize;
  IBaseLibraryHandle:= LoadLibrary(fbclib);

  // search for all compatible FireBird libraries in Windows
  {$IFDEF Windows}
  if IBaseLibraryHandle = NilHandle then
    IBaseLibraryHandle:= LoadLibrary(seclib);
  if IBaseLibraryHandle = NilHandle then
    IBaseLibraryHandle:= LoadLibrary(thirdlib);
  {$ENDIF}

  // Check Firebird library existance
  if (IBaseLibraryHandle = nilhandle) then
    Application.MessageBox('Unable to load Firebird library: ' + fbclib, 'Warning', 0);

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
