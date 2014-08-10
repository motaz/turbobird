{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Abstract SQL scripting engine.
		DB header file with interface section.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ Updated SQLScript.
Modified from FPC  sqldb.pp and sqlscript.pp to solve bugs with
parameters. See 
http://wiki.lazarus.freepascal.org/User_Changes_Trunk#TSQLScript_supports_:.2C_backtick_quotes_and_explicit_COMMIT.2FCOMMIT_RETAIN
}
{ Note regarding SQLScript bug in FPC <= 2.7.1:
If parameters are used in the script e.g. as in the sample EMPLOYEE.FDB
CREATE Procedure DELETE_EMPLOYEE
...
SELECT count(po_number)
FROM sales
WHERE sales_rep = :emp_num
INTO :any_sales;
you may get this error
: PrepareStatement :
-Dynamic SQL Error
-SQL error code = -104
-Token unknown - line 19, column 7
-?
because the TSQLScript tries to process parameters as if they were sqldb
parameters
}

unit modsqlscript;

{$mode objfpc}{$H+}

interface

{$IF FPC_FULLVERSION<20701}
uses
  Classes, SysUtils, db, dbconst, sqldb, trunksqlscript;
{$ELSE}
uses
  Classes, SysUtils, db, sqldb;
{$ENDIF}

{$IF FPC_FULLVERSION<20701}
// Only use this customised version for non-trunk FPC
type
  { TModCustomSQLQuery }
    TModCustomSQLQuery = class(TCustomSQLQuery)
    public
      //redeclaration from protected to public
      property ParamCheck;
      property ParseSQL;
      property SQL;
    end;

  { TModSQLScript }
    TModSQLScript = class (TTrunkCustomSQLScript)
    private
      FOnDirective: TSQLScriptDirectiveEvent;
      FQuery   : TModCustomSQLQuery;
      FDatabase : TDatabase;
      FTransaction : TDBTransaction;
    protected
      procedure ExecuteStatement (SQLStatement: TStrings; var StopExecution: Boolean); override;
      procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); override;
      procedure ExecuteCommit(CommitRetaining: boolean=true); override;
      Procedure SetDatabase (Value : TDatabase); virtual;
      Procedure SetTransaction(Value : TDBTransaction); virtual;
      Procedure CheckDatabase;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Execute; override;
      procedure ExecuteScript;
    published
      Property DataBase : TDatabase Read FDatabase Write SetDatabase;
      Property Transaction : TDBTransaction Read FTransaction Write SetTransaction;
      property OnDirective: TSQLScriptDirectiveEvent read FOnDirective write FOnDirective;
      property Directives;
      property Defines;
      property Script;
      property Terminator;
      property CommentsinSQL;
      property UseSetTerm;
      property UseCommit;
      property UseDefines;
      property OnException;
    end;


implementation


{ TModSQLScript }

procedure TModSQLScript.ExecuteStatement(SQLStatement: TStrings;
  var StopExecution: Boolean);
begin
  fquery.SQL.assign(SQLStatement);
  fquery.ExecSQL;
end;

procedure TModSQLScript.ExecuteDirective(Directive, Argument: String;
  var StopExecution: Boolean);
begin
  if assigned (FOnDirective) then
    FOnDirective (Self, Directive, Argument, StopExecution);
end;

procedure TModSQLScript.ExecuteCommit(CommitRetaining: boolean=true);
begin
  if FTransaction is TSQLTransaction then
    if CommitRetaining then
      TSQLTransaction(FTransaction).CommitRetaining
    else
      begin
      TSQLTransaction(FTransaction).Commit;
      TSQLTransaction(FTransaction).StartTransaction;
      end
  else
    begin
    FTransaction.Active := false;
    FTransaction.Active := true;
    end;
end;

procedure TModSQLScript.SetDatabase(Value: TDatabase);
begin
  FDatabase := Value;
end;

procedure TModSQLScript.SetTransaction(Value: TDBTransaction);
begin
  FTransaction := Value;
end;

procedure TModSQLScript.CheckDatabase;
begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

constructor TModSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery := TModCustomSQLQuery.Create(nil);
  CommentsInSQL:=false; //don't process comments by default
  FQuery.ParamCheck := false; // Do not parse for parameters; breaks use of e.g. select bla into :bla in Firebird procedures
  FQuery.ParseSQL:= false; //added for extra protection against messing with parameters
end;

destructor TModSQLScript.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

procedure TModSQLScript.Execute;
begin
  FQuery.DataBase := FDatabase;
  FQuery.Transaction := FTransaction;
  inherited Execute;
end;

procedure TModSQLScript.ExecuteScript;
begin
  Execute;
end;
{$ELSE}
// In FPC trunk, we can just use existing code
type
  TModSQLScript = TSQLScript;
implementation
{$ENDIF}
end.
