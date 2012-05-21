{***************************************************************************}
{  TurboBird: FireBird database administration and management tool          }
{  Developed by: Motaz Abdel Azeem http://code.sd/                          }
{  Start development :  5.Dec.2009                                          }
{  Last updated      : 21.May.2012                                          }
{  License           : GPL for GUI, LGPL for Units                          }
{***************************************************************************}

program TurboBird;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Controls, memdslaz, main, createdb, Reg, QueryWindow, ViewView,
  ViewTrigger, ViewSProc, ViewGen, NewTable, NewGen, EnterPass, About,
  CreateTrigger, EditTable, CallProc, EditDataFullRec, UDFInfo, ViewDomain,
  NewDomain, SysTables, NewConstraint, NewEditField, Calen, Scriptdb,
  UserPermissions, TableManage, BackupRestore, CreateUser, ChangePass,
  PermissionManage, SQLHistory, CopyTable, dynlibs, ibase60dyn, dbInfo,
  sysutils, Comparison;

const
  Version = '0.9.3';
  VersionDate = '2010 - May 2012';
{$IFDEF Unix}
{$DEFINE extdecl:=cdecl}
    fbclib = 'libfbclient.' + sharedsuffix;
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
   fbclib = 'fbclient.dll';
   seclib = 'gds32.dll';
{$ENDIF}

{$R *.res}

var
  SAbout: TfmAbout;
  IBaseLibraryHandle : TLibHandle;
begin
  Application.Initialize;
  IBaseLibraryHandle:= LoadLibrary(fbclib);
{$IFDEF Windows}
  if IBaseLibraryHandle = NilHandle then
    IBaseLibraryHandle:= LoadLibrary(seclib);
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
  Application.CreateForm(TfmComparison, fmComparison);
  fmMain.Version:= Version;
  fmMain.VersionDate:= VersionDate;
  SAbout.Free;
  InitialiseIBase60;
  Application.Run;
  ReleaseIBase60;

end.
