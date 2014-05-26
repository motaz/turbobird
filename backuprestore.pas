unit BackupRestore;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Zipper;

type

  { TfmBackupRestore }

  TfmBackupRestore = class(TForm)
    bbStart: TBitBtn;
    cbOperation: TComboBox;
    edBackup: TEdit;
    edPassword: TEdit;
    edTargetDatabase: TEdit;
    edHost: TEdit;
    edUserName: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    meLog: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    sbBrowseTargetdb: TSpeedButton;
    procedure bbStartClick(Sender: TObject);
    procedure sbBrowseTargetdbClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FDatabase: string; //doesn't really seem to be used anywhere
  public
    procedure Init(Title, Database, User, Password: string);
    { public declarations }
  end; 

var
  fmBackupRestore: TfmBackupRestore;

implementation

{ TfmBackupRestore }

uses UnitFirebirdServices;

procedure TfmBackupRestore.SpeedButton1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.fbk';
  if ((cbOperation.ItemIndex = 0) and (SaveDialog1.Execute)) or
   ((cbOperation.ItemIndex = 1) and (OpenDialog1.Execute)) then
  begin
    if cbOperation.ItemIndex = 0 then //backup
      edBackup.Text:= SaveDialog1.FileName
    else //restore
      edBackup.Text:= OpenDialog1.FileName;
  end;
end;

procedure TfmBackupRestore.Init(Title, Database, User, Password: string);
begin
  FDatabase:= Database;
  edUserName.Text:= User;
  edPassword.Text:= Password;

  // Linux: servername:/path/test.fdb  or /path/test.fdb
  // Windows: servername:c:\path\test.fdb or c:\path\test.fdb
  if Pos(':', Trim(FDatabase)) > 2 then
  begin
    edHost.Text:= Trim(Copy(FDatabase, 1, Pos(':', FDatabase) - 1));
    edTargetDatabase.Text:= Trim(Copy(FDatabase, Pos(':', FDatabase) + 1, Length(FDatabase)));
  end
  else
  begin
    // Assume local host for *nix, embedded for Windows
    {$IFDEF MSWINDOWS}
    edHost.Text := '';
    {$ELSE}
    edHost.Text := 'localhost';
    {$ENDIF}
    edTargetDatabase.Text := FDatabase;
  end;
end;

procedure TfmBackupRestore.bbStartClick(Sender: TObject);
var
  FireBirdServices: TFirebirdServices;
  Res: Ansistring;
  TempDir: string; //directory for temp files (including path delimiter)
  TempFile: string; //if not empty: used for intermediate file when zipping/unzipping
  Unzipper: TUnzipper;
  UserFile: string; //file the user chose: either backup destination or restore source
  FBKZippedFile: string; //name of fbk file when zip compressing
  Zipper: TZipper;
begin
  TempDir:= GetTempDir(false);
  FireBirdServices:= TFirebirdServices.Create;
  try
    FireBirdServices.VerboseOutput:= True;
    meLog.Clear;
    with FireBirdServices do
    begin
      HostName:= edHost.Text;
      DBName:= edTargetDatabase.Text;
      UserName:= edUserName.Text;
      Password:= edPassword.Text;
      UserFile:= trim(edBackup.Text);

      if LowerCase(ExtractFileExt(UserFile))='.zip' then
      begin
        if cbOperation.ItemIndex = 0 then
        begin
          // Backup: set up destination for backup process
          TempFile:= GetTempFilename(TempDir,'B');
        end
        else
        begin
          // Restore: unzip .fbk into temporary file
          TempFile:= sysutils.GetTempFilename;
          Unzipper:= TUnzipper.Create;
          try
            Unzipper.FileName:= UserFile;
            Unzipper.OutputPath:= TempDir;
            Unzipper.Examine;
            if Unzipper.Entries.Count=0 then
            begin
              ShowMessage(Format('%s contains no files. Aborting.',[UserFile]));
              exit;
            end;
            if Unzipper.Entries.Count<>1 then
            begin
              ShowMessage(Format('%s has more than 1 files. Only zip files with one .fbk file are supported. Aborting.',[UserFile]));
              exit;
            end;
            meLog.Lines.Add('Going to unzip file ' + UserFile + ':' + Unzipper.Entries[0].DiskFileName + ' into directory ' + TempDir);
            Unzipper.UnZipAllFiles; //we know we're unzipping just 1 file
            TempFile:= TempDir +
              ExtractFileName(Unzipper.Entries[0].DiskFileName);
          finally
            Unzipper.Free;
          end;
        end;
      end;

      try
        if TempFile='' then
          BackupFile:= UserFile // no zip files involved
        else
          {backup to temp, then zip later or
          restore from temp file}
          BackupFile:= TempFile;

        AttachService;
        if cbOperation.ItemIndex = 0 then
          StartBackup
        else
          StartRestore;

        while ServiceQuery(Res) do
          meLog.Lines.Add(Res);

        if (TempFile<>'' {using zip file}) and
          (cbOperation.ItemIndex <> 0 {restore}) then
        // Delete temp file when restore from zip is done
        begin
          Sleep(40); //give file system chance to update locks etc
          DeleteFile(TempFile);
        end;
      finally
        DetachService;
      end;
      meLog.Lines.Add('');
    end;

    if (tempfile<>'' {user wants zip file}) and
      (cbOperation.ItemIndex = 0 {backup}) then
    begin
      // Zip up the resulting backup
      Zipper:= TZipper.Create;
      try
        Zipper.FileName:= UserFile; //target is the user-selected backup file
        // Figure out the name of the .fbk file to be put in the zip file
        FBKZippedFile:= ExtractFileName(UserFile);
        // ChangeFileExt apparently cannot remove the extension, so do it manually
        //ChangeFileExt(FBKZippedFile,''); //get rid of ending .zip
        if LowerCase(ExtractFileExt(FBKZippedFile))='.zip' then
          Delete(FBKZippedFile,1+length(FBKZippedFile)-length('.zip'),length(FBKZippedFile));
        if LowerCase(ExtractFileExt(FBKZippedFile))<>'.fbk' then
          FBKZippedFile:= FBKZippedFile+'.fbk'; //add extension if not specified
        Zipper.Entries.AddFileEntry(TempFile, FBKZippedFile);
        meLog.Lines.Add('Going to compress file ' + TempFile +
          ' as filename ' + FBKZippedFile +
          ' in zip file ' + UserFile);
        Zipper.ZipAllFiles; //zip up all entries (just 1 in our case)
        // Delete temp file containing fbk
        Sleep(40); //give filesystem chance to update locks etc
        Sysutils.DeleteFile(TempFile);
      finally
        Zipper.Free;
      end;
    end;
  finally
    FireBirdServices.Free;
  end;
end;


procedure TfmBackupRestore.sbBrowseTargetdbClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt:= '.fdb';
  if SaveDialog1.Execute then
    edTargetDatabase.Text:= SaveDialog1.FileName;
end;

initialization
  {$I backuprestore.lrs}

end.

