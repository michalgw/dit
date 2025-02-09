program dimgtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  DIT.Image.Base,
  DIT.Image.Atari8.ATR,
  DIT.FileSystem.Base,
  DIT.FileSystem.Atari8.DOS12;

type
  TDITOperation = (oCreate, oList, oAdd, oExtract, oDelete, oMkDir);

type

  { TDITCli }

  TDITCli = class(TCustomApplication)
  protected
    function Normalize(AName: String): String;
    procedure DoCreateImage;
    procedure DoOpenImage(AWrite: Boolean);
    procedure DoList;
    procedure DoAdd;
    procedure DoExtract;
    procedure DoDelete;
    procedure DoMkDir;
    procedure DoRun; override;
  public
    ImageFileName: String;
    DiskImageFile: TFileStream;
    DiskImage: TDiskImage;
    FileSys: TFileSystem;
    Operation: TDITOperation;
    BaseDir: String;

    CreateImageFormat: String;
    CreateSectorCount, CreateSectorSize: Integer;
    CreateFileSystem: String;

    //ImgFileMask: String;

    LocalFileName: String;
    RemoteFileName: String;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDITCli }

function TDITCli.Normalize(AName: String): String;
var
  L: TStringArray;
  I: Integer;
begin
  L := AName.Split(['/','\']);
  Result := '';
  for I := Low(L) to High(L) do
  begin
    if Result <> '' then
      Result := Result + DirectorySeparator;
    Result := Result + FileSys.NormalizeFileName(L[I]);
  end;
  if (Result <> '') and (not (Result[1] in ['/', '\'])) then
    Result := DirectorySeparator + Result;
end;

procedure TDITCli.DoCreateImage;
var
  DiskImageClass: TDiskImageClass;
  FileSystemClass: TFileSystemClass;
begin
  WriteLn('Create new image file.');
  DiskImageClass := FindDiskImageClassByAlias(CreateImageFormat);
  if DiskImageClass = nil then
    raise Exception.Create('Unknown disk image format: ' + CreateImageFormat);
  DiskImageFile := TFileStream.Create(ImageFileName, fmCreate);
  DiskImage := DiskImageClass.Create(DiskImageFile);
  DiskImage.InitImage(TSectorSize(CreateSectorSize), CreateSectorCount);
  if CreateFileSystem <> '' then
  begin
    FileSystemClass := FindFileSystemByAlias(CreateFileSystem);
    if FileSystemClass <> nil then
    begin
      WriteLn('Formating: ' + FileSystemClass.FileSystemName);
      FileSystemClass.Format(DiskImage);
    end;
  end;
end;

procedure TDITCli.DoOpenImage(AWrite: Boolean);
var
  DiskImageClass: TDiskImageClass;
  FileSysClass: TFileSystemClass;
begin
  DiskImageClass := FindDiskImageClassByExt(ExtractFileExt(ImageFileName));
  if DiskImageClass = nil then
    raise Exception.Create('Unknown image format.');
  DiskImageFile := TFileStream.Create(ImageFileName, specialize IfThen<Word>(AWrite, fmOpenReadWrite, fmOpenRead));
  DiskImage := DiskImageClass.Create(DiskImageFile);
  DiskImage.Load;
  FileSysClass := nil;
  for FileSysClass in KnownFileSystems do
    if FileSysClass.Detect(DiskImage) then
    begin
      FileSys := FileSysClass.Create(DiskImage);
      Break;
    end;
  if FileSys = nil then
    raise Exception.Create('Unknown file system.');
  WriteLn('Detected file system: ' + FileSys.FileSystemName);
end;

procedure TDITCli.DoList;

procedure List(ADir: String);
var
  FList: TFileList;
  FItem: TFileListItem;
begin
  FList := FileSys.List(ADir);
  for FItem in FList do
  begin
    WriteLn(IncludeTrailingPathDelimiter(ADir) + FItem.Name, ', ', FItem.Size,
      specialize IfThen<String>(FItem.IsDir, ', <DIR>', ''));
    if FItem.IsDir then
      List(IncludeTrailingPathDelimiter(ADir) + FItem.Name);
  end;
end;

begin
  DoOpenImage(False);
  List(BaseDir);
  WriteLn;
  WriteLn('Free space: ', FileSys.FreeSpace);
  WriteLn('Capacity: ', FileSys.Capacity);
end;

procedure TDITCli.DoAdd;
var
  InFile: TFileStream = nil;
  OutFile: TFSFileStream = nil;
begin
  DoOpenImage(True);
  WriteLn('Add file: ', LocalFileName);
  try
    InFile := TFileStream.Create(LocalFileName, fmOpenRead);
    if RemoteFileName = '' then
      RemoteFileName := LocalFileName;
    RemoteFileName := Normalize(RemoteFileName);
    if BaseDir <> '' then
      RemoteFileName := IncludeTrailingPathDelimiter(Normalize(BaseDir)) + RemoteFileName;
    OutFile := FileSys.CreateFileStram(RemoteFileName, fmCreate);
    OutFile.CopyFrom(InFile, InFile.Size);
  finally
    if InFile <> nil then
      InFile.Free;
    if OutFile <> nil then
      OutFile.Free;
  end;
  WriteLn('Done.');
end;

procedure TDITCli.DoExtract;
var
  InFile: TFSFileStream = nil;
  OutFile: TFileStream = nil;
begin
  DoOpenImage(False);
  try
    RemoteFileName := Normalize(RemoteFileName);
    if BaseDir <> '' then
      RemoteFileName := IncludeTrailingPathDelimiter(Normalize(BaseDir)) + RemoteFileName;
    WriteLn('Extract file: ', RemoteFileName);
    InFile := FileSys.CreateFileStram(RemoteFileName, fmOpenRead);
    if LocalFileName = '' then
      LocalFileName := ExtractFileName(RemoteFileName);
    OutFile := TFileStream.Create(LocalFileName, fmCreate);
    OutFile.CopyFrom(InFile, InFile.Size);
  finally
    if InFile <> nil then
      InFile.Free;
    if OutFile <> nil then
      OutFile.Free;
  end;
  WriteLn('Done.');
end;

procedure TDITCli.DoDelete;
begin
  DoOpenImage(True);
  RemoteFileName := Normalize(RemoteFileName);
  if BaseDir <> '' then
    RemoteFileName := IncludeTrailingPathDelimiter(Normalize(BaseDir)) + RemoteFileName;
  WriteLn('Delete file/dir: ', RemoteFileName);
  FileSys.Delete(RemoteFileName);
  WriteLn('Done.');
end;

procedure TDITCli.DoMkDir;
begin
  DoOpenImage(True);
  RemoteFileName := Normalize(RemoteFileName);
  if BaseDir <> '' then
    RemoteFileName := IncludeTrailingPathDelimiter(Normalize(BaseDir)) + RemoteFileName;
  WriteLn('Make dir: ', RemoteFileName);
  FileSys.MakeDir(RemoteFileName);
  WriteLn('Done.');
end;

procedure TDITCli.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hi:o:f:r:b:', 'help image: operation: image-format: sectors: sector-size: disk-format: fs: lf: rf: base-dir:');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ImageFileName := GetOptionValue('i', 'image');
  if ImageFileName = '' then
  begin
    ShowException(Exception.Create('Missing image file name.'));
    Terminate;
    Exit;
  end;

  case LowerCase(GetOptionValue('o', 'operation')) of
    'c', 'create': begin
      CreateImageFormat := GetOptionValue('image-format');
      if CreateImageFormat = '' then
        CreateImageFormat := 'ATR';
      CreateSectorCount := StrToIntDef(GetOptionValue('sectors'), 0);
      CreateSectorSize := StrToIntDef(GetOptionValue('sector-size'), 0);
      case LowerCase(GetOptionValue('disk-format')) of
        'sd': begin
          CreateSectorCount := 720;
          CreateSectorSize := 128;
        end;
        'ed': begin
          CreateSectorCount := 1040;
          CreateSectorSize := 128;
        end;
        'dd': begin
          CreateSectorCount := 720;
          CreateSectorSize := 256;
        end;
      end;
      if (CreateSectorSize = 0) then
        CreateSectorSize := 128;
      if CreateSectorCount = 0 then
        CreateSectorCount := 720;
      CreateFileSystem := GetOptionValue('fs');
      Operation := oCreate;
    end;
    'l', 'list': begin
      //ImgFileMask := GetOptionValue('mask');
      //if ImgFileMask = '' then
      //  ImgFileMask := '*.*';
      BaseDir := GetOptionValue('b', 'base-dir');
      Operation := oList;
    end;
    'a', 'add': begin
      LocalFileName := GetOptionValue('f', 'lf');
      if LocalFileName = '' then
      begin
        ShowException(Exception.Create('No file specified.'));
        Terminate;
        Exit;
      end;
      BaseDir := GetOptionValue('b', 'base-dir');
      RemoteFileName := GetOptionValue('r', 'rf');
      Operation := oAdd;
    end;
    'e', 'extract': begin
      RemoteFileName := GetOptionValue('r', 'rf');
      if RemoteFileName = '' then
      begin
        ShowException(Exception.Create('No file specified.'));
        Terminate;
        Exit;
      end;
      LocalFileName := GetOptionValue('f', 'lf');
      BaseDir := GetOptionValue('b', 'base-dir');
      Operation := oExtract;
    end;
    'd', 'delete': begin
      RemoteFileName := GetOptionValue('r', 'rf');
      if RemoteFileName = '' then
      begin
        ShowException(Exception.Create('No file specified.'));
        Terminate;
        Exit;
      end;
      BaseDir := GetOptionValue('b', 'base-dir');
      Operation := oDelete;
    end;
    'm', 'mkdir': begin
      RemoteFileName := GetOptionValue('r', 'rf');
      if RemoteFileName = '' then
      begin
        ShowException(Exception.Create('No directory specified.'));
        Terminate;
        Exit;
      end;
      BaseDir := GetOptionValue('b', 'base-dir');
      Operation := oMkDir;
    end
    else begin
      ShowException(Exception.Create('Invalid operation.'));
      Terminate;
      Exit;
    end;
  end;

  case Operation of
    oCreate: DoCreateImage;
    oList: DoList;
    oAdd: DoAdd;
    oExtract: DoExtract;
    oDelete: DoDelete;
    oMkDir: DoMkDir;
  end;

  // stop program loop
  Terminate;
end;

constructor TDITCli.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TDITCli.Destroy;
begin
  if Assigned(FileSys) then
    FileSys.Free;
  if Assigned(DiskImage) then
    DiskImage.Free;
  if Assigned(DiskImageFile) then
    DiskImageFile.Free;
  inherited Destroy;
end;

procedure TDITCli.WriteHelp;
var
  S: String;
  DiskImageClass: TDiskImageClass;
  FileSysClass: TFileSystemClass;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -o <operation> -i <image file> ...');
  WriteLn('Operations: create,list,add,extract,delete,mkdir');
  WriteLn(' create - create new image file:');
  S := '';
  for DiskImageClass in KnownDiskImageFormats do
    S := S + DiskImageClass.FormatInfo.FormatAlias + '|';
  WriteLn('   --image-format=', S);
  WriteLn('   --sectors=number');
  WriteLn('   --sector-size=128|256|512');
  WriteLn('   --disk-format=SD|ED|DD');
  S := '';
  for FileSysClass in KnownFileSystems do
    S := S + FileSysClass.FileSystemAlias + '|';
  WriteLn('   --fs=', S);
  WriteLn();
  WriteLn(' list - list files:');
  WriteLn('   --mask=<file mask>');
  WriteLn('   -b <dir>|--base-dir=<dir>');
  WriteLn();
  WriteLn(' add - add file to image:');
  WriteLn('   -f <imput file name>|--lf=<input file name>');
  WriteLn('   -r <output file name>|--rf=<output file name>');
  WriteLn('   -b <dir>|--base-dir=<dir>');
  WriteLn();
  WriteLn(' extract - extract file from image:');
  WriteLn('   -r <input file name>|--rf=<input file name>');
  WriteLn('   -f <output file name>|--lf=<output file name>');
  WriteLn('   -b <dir>|--base-dir=<dir>');
  WriteLn();
  WriteLn(' delete - delete file or directory in image:');
  WriteLn('   -r <file or dir name>|--rf=<file or dir name>');
  WriteLn('   -b <dir>|--base-dir=<dir>');
  WriteLn();
  WriteLn(' mkdir - create directory in image:');
  WriteLn('   -r <dir name>|--rf=<dir name>');
  WriteLn('   -b <dir>|--base-dir=<dir>');
end;

var
  Application: TDITCli;
begin
  Application := TDITCli.Create(nil);
  Application.Title := 'Disk Image Tool CLI';
  Application.Run;
  Application.Free;
end.

