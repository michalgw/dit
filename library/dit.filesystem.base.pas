{ DIT.FileSystem.Base

  Copyright (c) 2025 Michał Gawrycki

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit DIT.FileSystem.Base;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, DIT.Image.Base, Generics.Collections;

const
  EA_NAME = 1;
  EA_SIZE = 2;
  EA_TIME = 3;
  EA_IS_DIR = 4;
  EA_READ_ONLY = 5;
  EA_HIDDEN = 6;
  EA_SYS_FILE = 7;
  EA_ARCHIVE = 8;
  EA_SYMLINK = 9;

  EA_USER = $10000;

type
  EFileSystemException = class(Exception);

  TFileAttribute = (faReadOnly, faHidden, faSysFile, faVolumeID,  faDirectory,
    faArchive, faSymLink);
  TFileAttributes = set of TFileAttribute;

  TExtAttributeId = Integer;
  TExtAttributeValue = Variant;
  TExtAttributeType = (eatNotAvaiable, eatBoolean, eatInteger, eatDate, eatTime,
    eatDateTime, eatString);

  TExtAttribute = record
    Attribute: TExtAttributeId;
    Value: TExtAttributeValue;
  end;

  PExtAttributeInfo = ^TExtAttributeInfo;
  TExtAttributeInfo = record
    Attribute: TExtAttributeId;
    Name: String;
    Description: String;
    ValueType: TExtAttributeType;
    ReadOnly: Boolean;
  end;

  TExtAttributes = array of TExtAttribute;

  TExtAttributesInfo = array of TExtAttributeInfo;

  { TExtAttributesHelper }

  TExtAttributesHelper = type helper for TExtAttributes
  private
    function GetAttribute(AttrId: TExtAttributeId): TExtAttributeValue;
  public
    procedure Add(AAttribute: TExtAttribute); overload;
    procedure Add(AAttribute: TExtAttributeId; AValue: Variant); overload;
    property Attribute[AttrId: TExtAttributeId]: TExtAttributeValue read GetAttribute;
  end;

  { TExtAttributesInfoHelper }

  TExtAttributesInfoHelper = type helper for TExtAttributesInfo
  private
    function GetAttributeInfo(AAttrId: TExtAttributeId): PExtAttributeInfo;
    function GetExists(AAttrId: TExtAttributeId): Boolean;
    function GetReadOnly(AAttrId: TExtAttributeId): Boolean;
    function GetValueType(AAttrId: TExtAttributeId): TExtAttributeType;
  public
    property AttributeInfo[AAttrId: TExtAttributeId]: PExtAttributeInfo read GetAttributeInfo;
    property Exists[AAttrId: TExtAttributeId]: Boolean read GetExists;
    property ReadOnly[AAttrId: TExtAttributeId]: Boolean read GetReadOnly;
    property ValueType[AAttrId: TExtAttributeId]: TExtAttributeType read GetValueType;
  end;

  { TFileSystemOptions }

  TFileSystemOptions = class(TPersistent)
  public
    constructor Create; virtual;
  end;

  TFileSystemOptionsClass = class of TFileSystemOptions;

  { TFileListItem }

  TFileListItem = record
    Name: String;
    Size: Int64;
    Attributes: TFileAttributes;
    Time: TDateTime;
    ExtAttributes: TExtAttributes;
    function IsDir: Boolean;
  end;

  TFileList = array of TFileListItem;

  TFileSystem = class;

  { TFSFileStream }

  TFSFileStream = class(TStream)
  private
    FFileSystem: TFileSystem;
    FFileName: String;
    FMode: Word;
  protected
    function GetFileName: String; virtual;
    property FileSystem: TFileSystem read FFileSystem;
    procedure SetSize(NewSize: Longint); override; overload;
  public
    constructor Create(AFileSystem: TFileSystem; AFileName: String; AMode: Word); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override; overload;
    function Write(const Buffer; Count: Longint): Longint; override; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property FileName: String read FFileName;
    property Mode: Word read FMode;
  end;

  TFSFileStreams = specialize TList<TFSFileStream>;

  { TFileSystem }

  TFileSystem = class
  private
    FDisk: TDiskImage;
    FStreams: TFSFileStreams;
  protected
    FOptions: TFileSystemOptions;
    function DoCreateFileStram(AFileName: String; AMode: Word): TFSFileStream; virtual; abstract;
    function StreamRead(AStream: TFSFileStream; var Buffer; Count: Longint): Longint; virtual; abstract;
    function StreamWrite(AStream: TFSFileStream; const Buffer; Count: Longint): Longint; virtual; abstract;
    function StreamSeek(AStream: TFSFileStream; const Offset: Int64; Origin: TSeekOrigin): Int64; virtual; abstract;
    procedure StreamSetSize(AStream: TFSFileStream; NewSize: Longint); virtual; abstract;
    property Disk: TDiskImage read FDisk;
  public
    constructor Create(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); virtual;
    destructor Destroy; override;
    function List(ADir: String = ''; AFillExtAttr: Boolean = False; AShowDeleted: Boolean = False): TFileList; virtual; abstract;
    function FileExists(AFileName: String): Boolean; virtual;
    function DirExists(ADir: String): Boolean; virtual;
    procedure Delete(AFileName: String); virtual; abstract;
    procedure Rename(AOldName, ANewName: String); virtual; abstract;
    procedure Attrib(AFileName: String; AAttibutes: TExtAttributes); virtual;
    procedure MakeDir(ADir: String); virtual;
    function CreateFileStram(AFileName: String; AMode: Word): TFSFileStream; virtual;
    function ValidateFileName(AFileName: String): Boolean; virtual;
    function NormalizeFileName(AFileName: String): String; virtual;
    function FreeSpace: Integer; virtual;
    function Capacity: Integer; virtual;
    function ExtAttribs(AFileName: String): TExtAttributes; virtual;
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil): Boolean; virtual;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); virtual; abstract;
    class function FileSystemName: String; virtual;
    class function FileSystemAlias: String; virtual;
    class function HasDirs: Boolean; virtual;
    class function AvaiableAttributes: TExtAttributesInfo; virtual;
    class function FileSystemOptionsClass: TFileSystemOptionsClass; virtual;
    property Options: TFileSystemOptions read FOptions;
  end;

  TFileSystemClass = class of TFileSystem;

  { TClusterBasedFileSystem }

  TClusterBasedFileSystem = class(TFileSystem)
  private
    FClusterSize: Integer;
    FSectorsPerCluster: Integer;
    FFirstClusterOffset: Integer;
    FClusterCount: Integer;
  protected
    function ReadCluster(AClusterNo: Integer; var Buf): Integer; virtual;
    function WriteCluster(AClusterNo: Integer; const Buf): Integer; virtual;
    property ClusterSize: Integer read FClusterSize write FClusterSize;
    property SectorsPerCluster: Integer read FSectorsPerCluster write FSectorsPerCluster;
    property FirstClusterOffset: Integer read FFirstClusterOffset write FFirstClusterOffset;
    property ClusterCount: Integer read FClusterCount write FClusterCount;
  end;

procedure RegisterFileSystem(AFileSystem: TFileSystemClass); overload;
procedure RegisterFileSystem(AFileSystems: array of TFileSystemClass); overload;
function KnownFileSystems: specialize TList<TFileSystemClass>;
function FindFileSystemByAlias(AAlias: String): TFileSystemClass;

implementation

uses
  Variants;

var
  FKnownFileSystems: specialize TList<TFileSystemClass> = nil;

procedure RegisterFileSystem(AFileSystem: TFileSystemClass);
begin
  if not Assigned(FKnownFileSystems) then
    FKnownFileSystems := specialize TList<TFileSystemClass>.Create;
  FKnownFileSystems.Add(AFileSystem);
end;

procedure RegisterFileSystem(AFileSystems: array of TFileSystemClass);
begin
  if not Assigned(FKnownFileSystems) then
    FKnownFileSystems := specialize TList<TFileSystemClass>.Create;
  FKnownFileSystems.AddRange(AFileSystems);
end;

function KnownFileSystems: specialize TList<TFileSystemClass>;
begin
  if not Assigned(FKnownFileSystems) then
    FKnownFileSystems := specialize TList<TFileSystemClass>.Create;
  Result := FKnownFileSystems;
end;

function FindFileSystemByAlias(AAlias: String): TFileSystemClass;
begin
  if not Assigned(FKnownFileSystems) then
    Exit(nil);
  for Result in FKnownFileSystems do
    if SameText(Result.FileSystemAlias, AAlias) then
      Exit;
  Result := nil;
end;

{ TClusterBasedFileSystem }

function TClusterBasedFileSystem.ReadCluster(AClusterNo: Integer; var Buf): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (AClusterNo < FClusterCount) and (AClusterNo >= 0) then
    for I := 0 to FSectorsPerCluster - 1 do
    begin
      Disk.ReadSector(FFirstClusterOffset + AClusterNo * SectorsPerCluster + I,
        PByte(@Buf)[I * Ord(Disk.SectorSize)]);
      Inc(Result, Ord(Disk.SectorSize));
    end
  else
    raise EFileSystemException.Create('Invalid cluster number: ' + IntToStr(AClusterNo));
end;

function TClusterBasedFileSystem.WriteCluster(AClusterNo: Integer; const Buf): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (AClusterNo < FClusterCount) and (AClusterNo >= 0) then
    for I := 0 to FSectorsPerCluster - 1 do
    begin
      Disk.WriteSector(FFirstClusterOffset + AClusterNo * SectorsPerCluster + I,
        PByte(@Buf)[I * Ord(Disk.SectorSize)]);
      Inc(Result, Ord(Disk.SectorSize));
    end
  else
    raise EFileSystemException.Create('Invalid cluster number: ' + IntToStr(AClusterNo));
end;

{ TFileSystemOptions }

constructor TFileSystemOptions.Create;
begin
  inherited;
end;

{ TFileListItem }

function TFileListItem.IsDir: Boolean;
begin
  Result := faDirectory in Attributes;
end;

{ TExtAttributesInfoHelper }

function TExtAttributesInfoHelper.GetAttributeInfo(AAttrId: TExtAttributeId
  ): PExtAttributeInfo;
var
  A: TExtAttributeInfo;
begin
  for A in Self do
    if AAttrId = A.Attribute then
      Exit(@A);
  Result := nil;
end;

function TExtAttributesInfoHelper.GetExists(AAttrId: TExtAttributeId): Boolean;
begin
  Result := GetAttributeInfo(AAttrId) <> nil;
end;

function TExtAttributesInfoHelper.GetReadOnly(AAttrId: TExtAttributeId
  ): Boolean;
var
  A: PExtAttributeInfo;
begin
  A := GetAttributeInfo(AAttrId);
  if A <> nil then
    Result := A^.ReadOnly
  else
    Result := True;
end;

function TExtAttributesInfoHelper.GetValueType(AAttrId: TExtAttributeId
  ): TExtAttributeType;
var
  A: PExtAttributeInfo;
begin
  A := GetAttributeInfo(AAttrId);
  if A <> nil then
    Result := A^.ValueType
  else
    Result := eatNotAvaiable;
end;

{ TExtAttributesHelper }

function TExtAttributesHelper.GetAttribute(AttrId: TExtAttributeId
  ): TExtAttributeValue;
var
  A: TExtAttribute;
begin
  for A in Self do
    if AttrId = A.Attribute then
      Exit(A.Value);
  Result := Null;
end;

procedure TExtAttributesHelper.Add(AAttribute: TExtAttribute);
begin
  Insert(AAttribute, Self, Length(Self));
end;

procedure TExtAttributesHelper.Add(AAttribute: TExtAttributeId; AValue: Variant
  );
var
  A: TExtAttribute;
begin
  A.Attribute := AAttribute;
  A.Value := AValue;
  Add(A);
end;

{ TFSFileStream }

function TFSFileStream.GetFileName: String;
begin
  Result := '';
end;

procedure TFSFileStream.SetSize(NewSize: Longint);
begin
  if Assigned(FFileSystem) then
    FFileSystem.StreamSetSize(Self, NewSize)
  else
    raise EFileSystemException.Create('File system not present.');
end;

constructor TFSFileStream.Create(AFileSystem: TFileSystem; AFileName: String;
  AMode: Word);
begin
  FFileSystem := AFileSystem;
  FFileName := AFileName;
  FMode := AMode;
end;

destructor TFSFileStream.Destroy;
begin
  FFileSystem.FStreams.Remove(Self);
  inherited Destroy;
end;

function TFSFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FFileSystem) then
    Result := FFileSystem.StreamRead(Self, Buffer, Count)
  else
    raise EFileSystemException.Create('File system not present.');
end;

function TFSFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FFileSystem) then
    Result := FFileSystem.StreamWrite(Self, Buffer, Count)
  else
    raise EFileSystemException.Create('File system not present.');
end;

function TFSFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Assigned(FFileSystem) then
    Result := FFileSystem.StreamSeek(Self, Offset, Origin)
  else
    raise EFileSystemException.Create('File system not present.');
end;

{ TFileSystem }

constructor TFileSystem.Create(ADisk: TDiskImage; AOptions: TFileSystemOptions);
begin
  FDisk := ADisk;
  FStreams := TFSFileStreams.Create;
  if Assigned(AOptions) then
  begin
    FOptions := AOptions.ClassType.Create as TFileSystemOptions;
    FOptions.Assign(AOptions);
  end
  else
    FOptions := nil;
end;

destructor TFileSystem.Destroy;
var
  S: TFSFileStream;
begin
  for S in FStreams do
    S.FFileSystem := nil;
  FStreams.Free;
  if Assigned(FOptions) then
    FreeAndNil(FOptions);
  inherited Destroy;
end;

function TFileSystem.FileExists(AFileName: String): Boolean;
begin
  Result := False;
end;

function TFileSystem.DirExists(ADir: String): Boolean;
begin
  Result := (ADir = '') or (ADir = '/') or (ADir = '\');
end;

procedure TFileSystem.Attrib(AFileName: String; AAttibutes: TExtAttributes);
begin

end;

procedure TFileSystem.MakeDir(ADir: String);
begin

end;

function TFileSystem.CreateFileStram(AFileName: String; AMode: Word
  ): TFSFileStream;
begin
  Result := DoCreateFileStram(AFileName, AMode);
  if Assigned(Result) then
    FStreams.Add(Result);
end;

function TFileSystem.ValidateFileName(AFileName: String): Boolean;
begin
  Result := True;
end;

function TFileSystem.NormalizeFileName(AFileName: String): String;
begin
  Result := AFileName;
end;

function TFileSystem.FreeSpace: Integer;
begin
  Result := 0;
end;

function TFileSystem.Capacity: Integer;
begin
  Result := 0;
end;

function TFileSystem.ExtAttribs(AFileName: String): TExtAttributes;
begin
  Result := [];
end;

class function TFileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
begin
  Result := False;
end;

class function TFileSystem.FileSystemName: String;
begin
  Result := '';
end;

class function TFileSystem.FileSystemAlias: String;
begin
  Result := '';
end;

class function TFileSystem.HasDirs: Boolean;
begin
  Result := False;
end;

class function TFileSystem.AvaiableAttributes: TExtAttributesInfo;
begin
  Result := [];
end;

class function TFileSystem.FileSystemOptionsClass: TFileSystemOptionsClass;
begin
  Result := nil;
end;

finalization
  if Assigned(FKnownFileSystems) then
    FKnownFileSystems.Free;

end.

