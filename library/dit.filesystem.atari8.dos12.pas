{ DIT.FileSystem.Atari8.DOS12

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

unit DIT.FileSystem.Atari8.DOS12;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, DIT.Image.Base, DIT.FileSystem.Base,
  DIT.FileSystem.Atari8.Utils;

const
  SECTOR_NO_VTOC = 360;
  SECTOR_NO_VTOC25 = 1024;
  SECTOR_NO_DIR = 361;
  SECTOR_DIR_LEN = 8;

  EA_OPEN_FOR_WRITE = EA_USER + 1;
  EA_DOS2_FLAG = EA_USER + 2;
  EA_FILE_EXIST = EA_USER + 3;
  EA_DELETED = EA_USER + 4;
  EA_START_SECTOR = EA_USER + 5;
  EA_SECTOR_COUNT = EA_USER + 6;
  EA_RAW_FILENAME = EA_USER + 7;
  EA_RAW_EXT = EA_USER + 8;

type

  { TAtariDOS1Options }

  TAtariDOS1Options = class(TFileSystemOptions)
  private
    FBootSectorCount: Integer;
    procedure SetBootSectorCount(AValue: Integer);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BootSectorCount: Integer read FBootSectorCount write SetBootSectorCount;
  end;

  { TAtariDOS2Options }

  TAtariDOS2Options = class(TAtariDOS1Options)
  public
    constructor Create; override;
  end;

  TDOS8Bits = bitpacked array[0..7] of Boolean;
  TDOSByte = bitpacked record
    case Byte of
      0: (
        Bits: TDOS8Bits;
      );
      2: (
        AByte: Byte;
      );
  end;

  TDOSFileStatusByte = bitpacked record
    case Boolean of
      False: (
        OpenForWrite: Boolean;
        Dos2Flag: Boolean;
        MyDosFlag: Boolean;
        Unused: Boolean;
        IsDir: Boolean;
        ReadOnly: Boolean;
        FileExists: Boolean;
        Deleted: Boolean;
      );
      True: (
        Value: Byte;
      );
  end;

  { TDOS1SectorTag }

  PDOS1SectorTag = ^TDOS1SectorTag;
  TDOS1SectorTag = bitpacked record
  private
    function GetNextSector: Integer;
    procedure SetNextSector(AValue: Integer);
  public
    NextSectorHi: 0..3;
    FileNo: 0..63;
    NextSectorLo: 0..255;
    SeqNoOrSize: 0..127;
    FullFlag: Boolean;
    property NextSector: Integer read GetNextSector write SetNextSector;
  end;

  { TDOS2SectorTag }

  PDOS2SectorTag = ^TDOS2SectorTag;
  TDOS2SectorTag = bitpacked record
  private
    function GetNextSector: Integer;
    procedure SetNextSector(AValue: Integer);
  public
    NextSectorHi: 0..3;
    FileNo: 0..63;
    NextSectorLo: 0..255;
    Size: Byte;
    property NextSector: Integer read GetNextSector write SetNextSector;
  end;

  { TMyDOSSectorTag }

  PMyDOSSectorTag = ^TMyDOSSectorTag;
  TMyDOSSectorTag = bitpacked record
  private
    function GetNextSector: Integer;
    procedure SetNextSector(AValue: Integer);
  public
    NextSectorHi: Byte;
    NextSectorLo: Byte;
    Size: Byte;
    property NextSector: Integer read GetNextSector write SetNextSector;
  end;

  { TDOSVTOC }

  PDOSVTOC = ^TDOSVTOC;
  TDOSVTOC = packed record
  private
    function GetSectorBit(AIndex: Integer): Boolean;
    procedure SetSectorBit(AIndex: Integer; AValue: Boolean);
  public
    Version: Byte;
    MaxSectors: Word;
    FreeSectors: Word;
    Unused: array[0..4] of Byte;
    Bitmap: array[0..245] of TDOSByte;
    property SectorBit[AIndex: Integer]: Boolean read GetSectorBit write SetSectorBit;
  end;

  { TDOS25VTOC }

  TDOS25VTOC = packed record
  private
    function GetSectorBit(AIndex: Integer): Boolean;
    procedure SetSectorBit(AIndex: Integer; AValue: Boolean);
  public
    Bitmap: array[0..$79] of TDOSByte;
    FreeSectors: Word;
    property SectorBit[AIndex: Integer]: Boolean read GetSectorBit write SetSectorBit;
  end;

  { TDOSFileEntry }

  TDOSFileEntry = packed record
  private
    function GetAttributes: TFileAttributes;
    function GetExtAttributes: TExtAttributes;
    function GetFileName: String;
    procedure SetFileName(AValue: String);
  public
    Status: TDOSFileStatusByte;
    SectorLength: Word;
    FirstSector: Word;
    Name: array[0..7] of Char;
    Ext: array[0..2] of Char;
    property FileName: String read GetFileName write SetFileName;
    property Attributes: TFileAttributes read GetAttributes;
    property ExtAttributes: TExtAttributes read GetExtAttributes;
  end;

  TDOSFileEntries = packed array[0..63] of TDOSFileEntry;

  { TDOSDirectory }

  TDOSDirectory = record
  public
    StartSector: Integer;
    Entries: TDOSFileEntries;
  end;

  TBaseDOSFileSystem = class;

  { TFileMap }

  TFileMap = record
    Map: array of Integer;
    RealSize: Integer;
    DirIdx: Integer;
    DirSector: Integer;
    function SectorMapIndex(ASector: Integer): Integer;
  end;

  { TDOSFileStream }

  TDOSFileStream = class(TFSFileStream)
  private
    FMap: TFileMap;
    FPosition: Integer;
  end;

  { TBaseDOSFileSystem }

  TBaseDOSFileSystem = class(TFileSystem)
  private
    FSectorDataSize: Integer;
  protected
  const
    VALID_FNAME_CHARS: TValidChars = ['0'..'9', 'A'..'Z'];
  var
    FVTOC: TDOSVTOC;
    procedure ReadVTOC; virtual;
    function FindDir(APath: String; out ADOSDir: TDOSDirectory): Boolean;
    function ReadDir(ASector: Integer): TDOSDirectory;
    function FileIndex(const ADir: TDOSDirectory; AFileName: String): Integer;
    function DirIsEmpty(const ADir: TDOSDirectory): Boolean;
    function DirFreeEntry(ADosDir: TDOSDirectory): Integer;
    procedure WriteVTOC; virtual;
    procedure WriteDir(ADir: TDOSDirectory; AFileIndex: Integer);

    function FileIsAvaiable(ADosDir: TDOSDirectory; AFileIdx: Integer; AShowDeleted: Boolean): Boolean; virtual;
    function FileMap(ADosDir: TDOSDirectory; AFileIdx: Integer): TFileMap;
    function FindFreeSector: Integer; virtual;
    function AllocSector(ASectorIndex: Integer; var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean; virtual;
    procedure DeallocSector(ASectorIndex: Integer; AStream: TDOSFileStream); virtual;

    procedure DoDelete(const AMap: TFileMap; var ADir: TDOSDirectory; AFileIdx: Integer); virtual;

    procedure DecodeFileLink(const ABuf; var AFileNo, ANextSector,
      ADataSize, ASeqNo: Integer; var ALast: Boolean); virtual; abstract;
    procedure EncodeFileLink(var ABuf; AFileNo, ANextSector,
      ADataSize, ASeqNo: Integer; ALast: Boolean); virtual; abstract;
    procedure InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer); virtual; abstract;

    function DoCreateFileStram(AFileName: String; AMode: Word): TFSFileStream; override;
    function StreamRead(AStream: TFSFileStream; var Buffer; Count: Longint
      ): Longint; override;
    function StreamWrite(AStream: TFSFileStream; const Buffer; Count: Longint
      ): Longint; override;
    function StreamSeek(AStream: TFSFileStream; const Offset: Int64;
      Origin: TSeekOrigin): Int64; override;
    procedure StreamSetSize(AStream: TFSFileStream; NewSize: Longint); override;

    class function GetValidChars: TValidChars; virtual;

    function GetVTOCBitmap(AIndex: Integer): Boolean; virtual;
    procedure SetVTOCBitmap(AIndex: Integer; AValue: Boolean); virtual;
    property VTOCBitmap[AIndex: Integer]: Boolean read GetVTOCBitmap write SetVTOCBitmap;
  public
    constructor Create(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); override;
    function List(ADir: String = ''; AFillExtAttr: Boolean = False; AShowDeleted: Boolean = False): TFileList; override;
    function FileExists(AFileName: String): Boolean; override;
    function DirExists(ADir: String): Boolean; override;
    procedure Rename(AOldName, ANewName: String); override;
    procedure Delete(AFileName: String); override;
    procedure Attrib(AFileName: String; AAttributes: TExtAttributes); override;
    function ValidateFileName(AFileName: String): Boolean; override;
    function NormalizeFileName(AFileName: String): String; override;
    function FreeSpace: Integer; override;
    function Capacity: Integer; override;
    class function AvaiableAttributes: TExtAttributesInfo; override;
    property SectorDataSize: Integer read FSectorDataSize;
  end;

  { TDOS1FileSystem }

  TDOS1FileSystem = class(TBaseDOSFileSystem)
  protected
    function FileIsAvaiable(ADosDir: TDOSDirectory; AFileIdx: Integer; AShowDeleted: Boolean): Boolean; override;
    procedure DecodeFileLink(const ABuf; var AFileNo, ANextSector, ADataSize, ASeqNo: Integer; var ALast: Boolean); override;
    procedure EncodeFileLink(var ABuf; AFileNo, ANextSector, ADataSize, ASeqNo: Integer; ALast: Boolean); override;
    procedure InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer); override;
  public
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions): Boolean; override;
    class function FileSystemName: String; override;
    class function FileSystemAlias: String; override;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions); override;
    class function FileSystemOptionsClass: TFileSystemOptionsClass; override;
  end;

  { TDOS2FileSystem }

  TDOS2FileSystem = class(TBaseDOSFileSystem)
  protected
    function FileIsAvaiable(ADosDir: TDOSDirectory; AFileIdx: Integer; AShowDeleted: Boolean): Boolean; override;
    procedure DecodeFileLink(const ABuf; var AFileNo, ANextSector, ADataSize, ASeqNo: Integer; var ALast: Boolean); override;
    procedure EncodeFileLink(var ABuf; AFileNo, ANextSector, ADataSize, ASeqNo: Integer; ALast: Boolean); override;
    procedure InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer); override;
    class procedure InitVTOC(ADisk: TDiskImage; BootScts: Integer); virtual;
  public
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions): Boolean; override;
    class function FileSystemName: String; override;
    class function FileSystemAlias: String; override;
    class function AvaiableAttributes: TExtAttributesInfo; override;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions); override;
    class function FileSystemOptionsClass: TFileSystemOptionsClass; override;
  end;

  { TDOS25FileSystem }

  TDOS25FileSystem = class(TDOS2FileSystem)
  protected
    FVTOC2: TDOS25VTOC;
    procedure ReadVTOC; override;
    procedure WriteVTOC; override;
    function AllocSector(ASectorIndex: Integer; var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean; override;
    procedure DeallocSector(ASectorIndex: Integer; AStream: TDOSFileStream); override;
    function FindFreeSector: Integer; override;
    function FileIsAvaiable(ADosDir: TDOSDirectory; AFileIdx: Integer; AShowDeleted: Boolean): Boolean; override;
    procedure DoDelete(const AMap: TFileMap; var ADir: TDOSDirectory; AFileIdx: Integer); override;
    function GetVTOCBitmap(AIndex: Integer): Boolean; override;
    procedure SetVTOCBitmap(AIndex: Integer; AValue: Boolean); override;
    class procedure InitVTOC(ADisk: TDiskImage; BootScts: Integer); override;
  public
    function FreeSpace: Integer; override;
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions): Boolean; override;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions); override;
    class function FileSystemName: String; override;
    class function FileSystemAlias: String; override;
  end;

  TMyDOSVTOC = array[0..Pred(33*256)] of TDOSByte;

  { TMyDOSVTOCHelper }

  TMyDOSVTOCHelper = type helper for TMyDOSVTOC
  private
    function GetSectorBit(AIndex: Integer): Boolean;
    procedure SetSectorBit(AIndex: Integer; AValue: Boolean);
  public
    property SectorBit[AIndex: Integer]: Boolean read GetSectorBit write SetSectorBit;
  end;

  { TMyDOSFileSystem }

  TMyDOSFileSystem = class(TDOS2FileSystem)
  private
    FMyVTOC: TMyDOSVTOC;
  protected
    procedure ReadVTOC; override;
    procedure WriteVTOC; override;
    function AllocSector(ASectorIndex: Integer; var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean; override;
    procedure DeallocSector(ASectorIndex: Integer; AStream: TDOSFileStream); override;
    function FindFreeSector: Integer; override;
    procedure DecodeFileLink(const ABuf; var AFileNo, ANextSector, ADataSize, ASeqNo: Integer; var ALast: Boolean); override;
    procedure EncodeFileLink(var ABuf; AFileNo, ANextSector, ADataSize, ASeqNo: Integer; ALast: Boolean); override;
    function FileIsAvaiable(ADosDir: TDOSDirectory; AFileIdx: Integer; AShowDeleted: Boolean): Boolean; override;
    procedure InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer); override;
    class function GetValidChars: TValidChars; override;
    function GetVTOCBitmap(AIndex: Integer): Boolean; override;
    procedure SetVTOCBitmap(AIndex: Integer; AValue: Boolean); override;
  public
    function DirExists(ADir: String): Boolean; override;
    procedure Delete(AFileName: String); override;
    procedure MakeDir(ADir: String); override;
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions): Boolean; override;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions); override;
    class function FileSystemName: String; override;
    class function FileSystemAlias: String; override;
    class function HasDirs: Boolean; override;
  end;

implementation

uses
  StrUtils, LazFileUtils, Regex, Math;

const
  DOS1_BOOTSECTOR : array[0..127] of byte = (
    $00,$01,$00,$07,$75,$07,$4C,$21,$07,$03,$01,$00,$00,$00,$00,
    $00,$00,$7D,$00,$00,$62,$6F,$6F,$74,$20,$65,$72,$72,$6F,$72,
    $9C,$1C,$9C,$A2,$00,$A9,$14,$8D,$44,$03,$A9,$07,$8D,$45,$03,
    $A9,$0A,$8D,$48,$03,$A9,$00,$8D,$49,$03,$A9,$0B,$20,$77,$07,
    $A2,$10,$A9,$7D,$9D,$44,$03,$A9,$07,$9D,$45,$03,$A9,$04,$9D,
    $4A,$03,$A9,$03,$20,$77,$07,$A9,$00,$9D,$48,$03,$9D,$49,$03,
    $A9,$07,$20,$77,$07,$A9,$0C,$20,$77,$07,$A2,$00,$A9,$1E,$9D,
    $44,$03,$A9,$03,$9D,$48,$03,$A9,$0B,$20,$77,$07,$38,$60,$9D,
    $42,$03,$4C,$56,$E4,$4B,$9B,$00);

  DOS2_BOOTSECTOR1 : array[0..127] of byte = (
    $00,$03,$00,$07,$B2,$07,$4C,$5E,$07,$03,$01,$00,$00,$00,$00,
    $00,$00,$7D,$00,$00,$43,$61,$6E,$6E,$6F,$74,$20,$62,$6F,$6F,
    $74,$20,$73,$69,$6E,$63,$65,$20,$74,$68,$65,$72,$65,$20,$69,
    $73,$20,$6E,$6F,$20,$44,$4F,$53,$9B,$70,$72,$65,$73,$65,$6E,
    $74,$20,$6F,$6E,$20,$74,$68,$69,$73,$20,$64,$69,$73,$6B,$2E,
    $20,$2D,$70,$72,$65,$73,$73,$20,$61,$6E,$79,$20,$6B,$65,$79,
    $2D,$9C,$1C,$9C,$A2,$00,$A9,$14,$8D,$44,$03,$A9,$07,$8D,$45,
    $03,$A9,$47,$8D,$48,$03,$A9,$00,$8D,$49,$03,$A9,$0B,$20,$B4,
    $07,$A2,$10,$A9,$BA,$9D,$44,$03);
  DOS2_BOOTSECTOR2 : array[0..59] of byte = (
    $A9,$07,$9D,$45,$03,$A9,$04,$9D,$4A,$03,$A9,$03,$20,$B4,$07,
    $A9,$00,$9D,$48,$03,$9D,$49,$03,$A9,$07,$20,$B4,$07,$A9,$0C,
    $20,$B4,$07,$A2,$00,$A9,$5B,$9D,$44,$03,$A9,$03,$9D,$48,$03,
    $A9,$0B,$20,$B4,$07,$38,$60,$9D,$42,$03,$4C,$56,$E4,$4B,$9B);

{ TMyDOSVTOCHelper }

function TMyDOSVTOCHelper.GetSectorBit(AIndex: Integer): Boolean;
begin
  Result := Self[AIndex div 8].Bits[7 - (AIndex mod 8)];
end;

procedure TMyDOSVTOCHelper.SetSectorBit(AIndex: Integer; AValue: Boolean);
begin
  Self[AIndex div 8].Bits[7 - (AIndex mod 8)] := AValue;
end;

{ TMyDOSSectorTag }

function TMyDOSSectorTag.GetNextSector: Integer;
begin
  Result := (NextSectorHi shl 8) + NextSectorLo;
end;

procedure TMyDOSSectorTag.SetNextSector(AValue: Integer);
begin
  NextSectorLo := AValue and $FF;
  NextSectorHi := (AValue and $FF00) shr 8;
end;

{ TMyDOSFileSystem }

procedure TMyDOSFileSystem.ReadVTOC;
var
  I, NumVTOC: Integer;
begin
  inherited ReadVTOC;
  if FVTOC.Version > 2 then
  begin
    FillByte(FMyVTOC, SizeOf(FMyVTOC), 0);
    NumVTOC := FVTOC.Version - 2;
    if Disk.SectorSize = ss128 then
      NumVTOC := NumVTOC * 2;
    for I := 0 to NumVTOC - 1 do
      Disk.ReadSector(359 - I, FMyVTOC[I * Ord(Disk.SectorSize)]);
  end;
end;

procedure TMyDOSFileSystem.WriteVTOC;
var
  I: Integer;
begin
  inherited WriteVTOC;
  if FVTOC.Version > 2 then
  begin
    for I := 0 to FVTOC.Version - 3 do
      Disk.WriteSector(359 - I, FMyVTOC[I * Ord(Disk.SectorSize)]);
  end;
end;

function TMyDOSFileSystem.AllocSector(ASectorIndex: Integer;
  var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean;
begin
  if FVTOC.Version <= 2 then
    Result := inherited AllocSector(ASectorIndex, ADosDir, AFileIndex)
  else
  begin
    VTOCBitmap[ASectorIndex] := False;
    Inc(ADosDir.Entries[AFileIndex].SectorLength);
    Dec(FVTOC.FreeSectors);
    Result := True;
  end;
end;

procedure TMyDOSFileSystem.DeallocSector(ASectorIndex: Integer;
  AStream: TDOSFileStream);
var
  DosDir: TDOSDirectory;
begin
  if (FVTOC.Version <= 2) and (FVTOC.MaxSectors < 720) then
    inherited DeallocSector(ASectorIndex, AStream)
  else
  begin
    VTOCBitmap[ASectorIndex] := True;
    Inc(FVTOC.FreeSectors);
    DosDir := ReadDir(AStream.FMap.DirSector);
    Dec(DosDir.Entries[TDOSFileStream(AStream).FMap.DirIdx].SectorLength);
    WriteDir(DosDir, AStream.FMap.DirIdx);
  end;
end;

function TMyDOSFileSystem.FindFreeSector: Integer;
var
  I: Integer;
begin
  if FVTOC.Version <= 2 then
    Result := inherited FindFreeSector
  else
  begin
    for I := 1 to Disk.SectorCount do
      if VTOCBitmap[I] then
        Exit(I);
    Result := 0;
  end;
end;

procedure TMyDOSFileSystem.DecodeFileLink(const ABuf; var AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; var ALast: Boolean);
begin
  if FVTOC.Version <= 2 then
    inherited
  else
    with PMyDOSSectorTag(@ABuf + SectorDataSize)^ do
    begin
      AFileNo := -1;
      ANextSector := NextSector;
      ALast := ANextSector = 0;
      ADataSize := Size;
      ASeqNo := -1;
    end;
end;

procedure TMyDOSFileSystem.EncodeFileLink(var ABuf; AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; ALast: Boolean);
begin
  if FVTOC.Version <= 2 then
    inherited EncodeFileLink(ABuf, AFileNo, ANextSector, ADataSize, ASeqNo, ALast)
  else
    with PMyDOSSectorTag(@ABuf + SectorDataSize)^ do
    begin
      NextSector := ANextSector;
      Size := ADataSize;
    end;
end;

function TMyDOSFileSystem.FileIsAvaiable(ADosDir: TDOSDirectory;
  AFileIdx: Integer; AShowDeleted: Boolean): Boolean;
begin
  Result := (AFileIdx >= 0) and (ADosDir.Entries[AFileIdx].Status.FileExists or
    ADosDir.Entries[AFileIdx].Status.IsDir) and (not ADosDir.Entries[AFileIdx].Status.Deleted);
end;

procedure TMyDOSFileSystem.InitAttributes(var ADosDir: TDOSDirectory;
  AIndex: Integer);
begin
  if FVTOC.Version > 2 then
    with ADosDir.Entries[AIndex].Status do
    begin
      Value := 0;
      FileExists := True;
      Dos2Flag := True;
      MyDosFlag := True;
    end
  else
    inherited;
end;

class function TMyDOSFileSystem.GetValidChars: TValidChars;
begin
  Result := inherited GetValidChars + ['@', '_'];
end;

function TMyDOSFileSystem.GetVTOCBitmap(AIndex: Integer): Boolean;
begin
  if (FVTOC.MaxSectors > 720) and
    (((Disk.SectorSize = ss128) and (AIndex >= 118 * 8)) or
    ((Disk.SectorSize = ss256) and (AIndex >= 246 * 8))) then
  begin
    AIndex := AIndex - ((Ord(Disk.SectorSize) - 10) * 8);
    Result := FMyVTOC.SectorBit[AIndex];
  end
  else
    Result := inherited GetVTOCBitmap(AIndex);
end;

procedure TMyDOSFileSystem.SetVTOCBitmap(AIndex: Integer; AValue: Boolean);
begin
  if (FVTOC.MaxSectors > 720) and
    (((Disk.SectorSize = ss128) and (AIndex >= 118 * 8)) or
    ((Disk.SectorSize = ss256) and (AIndex >= 246 * 8))) then
  begin
    AIndex := AIndex - ((Ord(Disk.SectorSize) - 10) * 8);
    FMyVTOC.SectorBit[AIndex] := AValue;
  end
  else
    inherited SetVTOCBitmap(AIndex, AValue);
end;

function TMyDOSFileSystem.DirExists(ADir: String): Boolean;
var
  DosDir: TDOSDirectory;
begin
  Result := FindDir(ADir, DosDir);
end;

procedure TMyDOSFileSystem.Delete(AFileName: String);
var
  DosDir: TDOSDirectory;
  S: String;
  FIdx, I: Integer;
begin
  if DirExists(AFileName) then
  begin
    FindDir(AFileName, DosDir);
    if not DirIsEmpty(DosDir) then
      raise EFileSystemException.Create('Directory not empty');
    S := ExtractFilePath(ExcludeTrailingPathDelimiter(AFileName));
    if not FindDir(S, DosDir) then
      raise EFileSystemException.Create('Parent folder not found');
    S := ExtractFileName(AFileName);
    FIdx := FileIndex(DosDir, S);
    if FIdx < 0 then
      raise EFileSystemException.Create('Dir not found');
    for I := 0 to 7 do
      VTOCBitmap[DosDir.Entries[FIdx].FirstSector + I] := True;
    with DosDir.Entries[FIdx].Status do
    begin
      Value := 0;
      Deleted := True;
    end;
    Inc(FVTOC.FreeSectors, 8);
    WriteVTOC;
    WriteDir(DosDir, FIdx);
  end
  else
    inherited Delete(AFileName);
end;

procedure TMyDOSFileSystem.MakeDir(ADir: String);
var
  DosDir: TDOSDirectory;
  FIdx: Integer;
  I, S, C: Integer;
  Buf: array[0..255] of Byte;
begin
  if not FindDir(ExtractFilePath(ExcludeTrailingPathDelimiter(ADir)), DosDir) then
    raise EFileSystemException.Create('Path not found: ' + ExtractFilePath(ExcludeTrailingPathDelimiter(ADir)));
  if FileIndex(DosDir, ExtractFileName(ExcludeTrailingPathDelimiter(ADir))) >= 0 then
    raise EFileSystemException.Create('File or directory exists: ' + ADir);
  I := 1;
  S := 0;
  C := 0;
  while (I <= FVTOC.MaxSectors) and (C < 8) do
  begin
    if VTOCBitmap[I] then
    begin
      if S = 0 then
        S := I;
      Inc(C);
    end
    else
    begin
      S := 0;
      C := 0;
    end;
    Inc(I);
  end;
  if (C < 8) or (S = 0) then
    raise EFileSystemException.Create('Not room for directory');
  FIdx := DirFreeEntry(DosDir);
  if FIdx < 0 then
    raise EFileSystemException.Create('Directory is full.');
  for I := S to S + 7 do
    VTOCBitmap[I] := False;
  Dec(FVTOC.FreeSectors, 8);
  with DosDir.Entries[FIdx] do
  begin
    FileName := ExtractFileName(ExcludeTrailingPathDelimiter(ADir));
    FirstSector := S;
    SectorLength := 8;
    Status.Value := 0;
    Status.IsDir := True;
    Status.MyDosFlag := FVTOC.Version > 2;
  end;
  WriteVTOC;
  WriteDir(DosDir, FIdx);
  FillByte(Buf, SizeOf(Buf), 0);
  for I := S to S + 7 do
    Disk.WriteSector(I, Buf);
end;

class function TMyDOSFileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
var
  Buf: array[0..255] of Byte;
  VTOC: TDOSVTOC absolute Buf;
  //I: Integer;
begin
  Result := False;
  if (ADisk.SectorSize <> ss128) and (ADisk.SectorSize <> ss256) then
    Exit;
  if ADisk.ReadSector(1, Buf) <> Ord(ADisk.SectorSize) then
    Exit;
  if Buf[0] <> Ord('M') then
    Exit;
  FillByte(Buf, SizeOf(Buf), 0);
  ADisk.ReadSector(SECTOR_NO_VTOC, Buf);
  if ADisk.SectorCount > 720 then
  begin
    if (VTOC.Version < 3) or (VTOC.FreeSectors > VTOC.MaxSectors) then
      Exit;
    {for I := 1 to VTOC.Version - 2 do
      if VTOC.SectorBit[360 - I] then
        Exit;}
    Result := True;
  end
  else
    Result := (VTOC.Version = 2) and (VTOC.MaxSectors >= VTOC.FreeSectors)
      and (VTOC.SectorBit[SECTOR_NO_VTOC] = False) and (VTOC.SectorBit[SECTOR_NO_DIR] = False);
end;

class procedure TMyDOSFileSystem.Format(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
var
  Buf: array[0..255] of Byte;
  VTOC: TDOSVTOC absolute Buf;
  MyVTOC: TMyDOSVTOC;
  I: Integer;
  NumVTOC: Integer;
  BootScts: Integer;
begin
  if ((ADisk.SectorSize <> ss128) and (ADisk.SectorSize <> ss256))
    or (ADisk.SectorCount < 720) then
    raise EFileSystemException.Create('Unsupported disk geometry.');
  if Assigned(AOptions) and (AOptions is TAtariDOS1Options) then
    BootScts := TAtariDOS1Options(AOptions).BootSectorCount
  else
    BootScts := 3;
  if BootScts < 1 then
    BootScts := 1;
  FillByte(Buf, SizeOf(Buf), 0);
  for I := 1 to ADisk.SectorCount do
    if ADisk.WriteSector(I, Buf) <> Ord(ADisk.SectorSize) then
      raise EFileSystemException.Create('Block write error.');
  Move(DOS2_BOOTSECTOR1, Buf, SizeOf(DOS2_BOOTSECTOR1));
  Buf[0] := Ord('M');
  ADisk.WriteSector(1, Buf);
  FillByte(Buf, SizeOf(Buf), 0);
  Move(DOS2_BOOTSECTOR2, Buf, SizeOf(DOS2_BOOTSECTOR2));
  ADisk.WriteSector(2, Buf);
  FillByte(Buf, SizeOf(Buf), 0);
  if (Ord(ADisk.SectorSize) - 10) * 8 < ADisk.SectorCount then
  begin
    NumVTOC := (ADisk.SectorCount + 1 + 80 + 2047) div 2048;
    if ADisk.SectorSize = ss128 then
    begin
      NumVTOC := NumVTOC * 2;
      VTOC.Version := (NumVTOC div 2) + 2;
    end
    else
      VTOC.Version := NumVTOC + 2;
  end
  else
  begin
    NumVTOC := 1;
    VTOC.Version := 2;
  end;
  VTOC.MaxSectors := ADisk.SectorCount - 8 - NumVTOC - BootScts;
  VTOC.FreeSectors := VTOC.MaxSectors;
  for I := 0 to BootScts do
    VTOC.SectorBit[I] := False;
  for I := BootScts + 1 to 359 do
    VTOC.SectorBit[I] := True;
  for I := 360 to 368 do
    VTOC.SectorBit[I] := False;
  for I := 369 to Min(ADisk.SectorCount, (Ord(ADisk.SectorSize) - 10) * 8) do
    VTOC.SectorBit[I] := True;
  if NumVTOC > 1 then
    for I := 1 to NumVTOC - 1 do
      VTOC.SectorBit[360 - I] := False;
  ADisk.WriteSector(SECTOR_NO_VTOC, Buf);
  if NumVTOC > 0 then
  begin
    FillByte(MyVTOC, SizeOf(MyVTOC), 0);
    for I := 0 to ADisk.SectorCount - ((Ord(ADisk.SectorSize) - 10) * 8) do
      MyVTOC.SectorBit[I] := True;
    for I := 0 to NumVTOC - 2 do
      ADisk.WriteSector(359 - I, MyVTOC[I * Ord(ADisk.SectorSize)]);
  end;
end;

class function TMyDOSFileSystem.FileSystemName: String;
begin
  Result := 'MyDOS';
end;

class function TMyDOSFileSystem.FileSystemAlias: String;
begin
  Result := 'MyDOS';
end;

class function TMyDOSFileSystem.HasDirs: Boolean;
begin
  Result := True;
end;

{ TAtariDOS2Options }

constructor TAtariDOS2Options.Create;
begin
  FBootSectorCount := 3;
end;

{ TAtariDOS1Options }

procedure TAtariDOS1Options.SetBootSectorCount(AValue: Integer);
begin
  if FBootSectorCount = AValue then Exit;
  if AValue < 1 then
    FBootSectorCount := 1
  else if AValue > 46 then
    FBootSectorCount := 46
  else
    FBootSectorCount := AValue;
end;

constructor TAtariDOS1Options.Create;
begin
  FBootSectorCount := 1;
end;

procedure TAtariDOS1Options.Assign(Source: TPersistent);
begin
  if Source is TAtariDOS1Options then
    BootSectorCount := TAtariDOS1Options(Source).BootSectorCount;
end;

{ TFileMap }

function TFileMap.SectorMapIndex(ASector: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(Map) - 1 do
    if Map[I] = ASector then
      Exit(I);
  Result := -1;
end;

{ TDOS2SectorTag }

function TDOS2SectorTag.GetNextSector: Integer;
begin
  Result := NextSectorLo + (NextSectorHi shl 8);
end;

procedure TDOS2SectorTag.SetNextSector(AValue: Integer);
begin
  NextSectorHi := (AValue and %1100000000) shr 8;
  NextSectorLo := AValue and 255;
end;

{ TDOS1SectorTag }

function TDOS1SectorTag.GetNextSector: Integer;
begin
  Result := NextSectorLo + (NextSectorHi shl 8);
end;

procedure TDOS1SectorTag.SetNextSector(AValue: Integer);
begin
  NextSectorHi := (AValue and %1100000000) shr 8;
  NextSectorLo := AValue and 255;
end;

{ TBaseDOSFileSystem }

function TBaseDOSFileSystem.GetVTOCBitmap(AIndex: Integer): Boolean;
begin
  Result := FVTOC.SectorBit[AIndex];
end;

procedure TBaseDOSFileSystem.SetVTOCBitmap(AIndex: Integer; AValue: Boolean);
begin
  FVTOC.SectorBit[AIndex] := AValue;
end;

procedure TBaseDOSFileSystem.ReadVTOC;
var
  Buf: array[0..255] of Byte;
begin
  if Disk.SectorSize = ss256 then
    FillChar(Buf, SizeOf(Buf), 0);
  Disk.ReadSector(SECTOR_NO_VTOC, Buf);
  Move(Buf, FVTOC, SizeOf(FVTOC));
end;

function TBaseDOSFileSystem.FindDir(APath: String; out ADOSDir: TDOSDirectory
  ): Boolean;
var
  Path: TStringArray;
  S: String;
  I: Integer;
  FileInd: Integer;
begin
  S := ExtractFilePath(IncludeTrailingPathDelimiter(APath));
  Path := S.Split(['/','\']);
  if (Length(Path) = 0) or (Path[0] <> '') then
    Exit(False);
  ADOSDir := ReadDir(SECTOR_NO_DIR);
  for I := 1 to Length(Path) - 1 do
  begin
    if Path[I] = '' then
      Continue;
    FileInd := FileIndex(ADOSDir, Path[I]);
    if (FileInd < 0) or (not ADOSDir.Entries[FileInd].Status.IsDir) then
      Exit(False);
    ADOSDir := ReadDir(ADOSDir.Entries[FileInd].FirstSector);
  end;
  Result := True;
end;

function TBaseDOSFileSystem.ReadDir(ASector: Integer): TDOSDirectory;
var
  Buf: array[0..255] of Byte;
  I: Integer;
begin
  FillByte(Result.Entries, SizeOf(Result.Entries), 0);
  for I := 0 to 7 do
  begin
    Disk.ReadSector(ASector + I, Buf);
    Move(Buf, Result.Entries[I * 8], 128);
  end;
  Result.StartSector := ASector;
end;

function TBaseDOSFileSystem.FileIndex(const ADir: TDOSDirectory;
  AFileName: String): Integer;
begin
  for Result := 0 to 63 do
    if FileIsAvaiable(ADir, Result, False)
      and (ADir.Entries[Result].FileName = AFileName) then
      Exit;
  Result := -1;
end;

function TBaseDOSFileSystem.DirIsEmpty(const ADir: TDOSDirectory): Boolean;
var
  I: Integer;
begin
  for I := 0 to 63 do
    if FileIsAvaiable(ADir, I, False) then
      Exit(False);
  Result := True;
end;

function TBaseDOSFileSystem.DirFreeEntry(ADosDir: TDOSDirectory): Integer;
var
  I: Integer;
begin
  for I := 0 to 63 do
    if not FileIsAvaiable(ADosDir, I, False) then
      Exit(I);
  Result := -1;
end;

procedure TBaseDOSFileSystem.WriteVTOC;
var
  Buf: array[0..255] of Byte;
begin
  FillByte(Buf, SizeOf(Buf), 0);
  Move(FVTOC, Buf, SizeOf(FVTOC));
  Disk.WriteSector(SECTOR_NO_VTOC, Buf);
end;

procedure TBaseDOSFileSystem.WriteDir(ADir: TDOSDirectory; AFileIndex: Integer);
var
  Buf: array[0..255] of Byte;
  S, E, I: Integer;
begin
  if AFileIndex < 0 then
  begin
    S := 0;
    E := 7;
  end
  else
  begin
    S := AFileIndex div 8;
    E := S;
  end;
  FillByte(Buf, SizeOf(Buf), 0);
  for I := S to E do
  begin
    Move(ADir.Entries[I * 8], Buf, 128);
    Disk.WriteSector(ADir.StartSector + I, Buf);
  end;
end;

function TBaseDOSFileSystem.FileIsAvaiable(ADosDir: TDOSDirectory;
  AFileIdx: Integer; AShowDeleted: Boolean): Boolean;
begin
  Result := (AFileIdx >= 0) and ((not ADosDir.Entries[AFileIdx].Status.Deleted) or AShowDeleted) and
    (ADosDir.Entries[AFileIdx].FirstSector > 0) and (ADosDir.Entries[AFileIdx].SectorLength > 0);
end;

function TBaseDOSFileSystem.FileMap(ADosDir: TDOSDirectory; AFileIdx: Integer
  ): TFileMap;
var
  Buf: array[0..255] of Byte;
  TgFileNo, TgNextSector, TgDataLen, TgSeqNo: Integer;
  TgLast: Boolean;
begin
  Result := Default(TFileMap);
  if ADosDir.Entries[AFileIdx].FirstSector = 0 then
    Exit;
  Disk.ReadSector(ADosDir.Entries[AFileIdx].FirstSector, Buf);
  DecodeFileLink(Buf, TgFileNo, TgNextSector, TgDataLen, TgSeqNo, TgLast);
  if (FVTOC.Version <= 2) and (TgFileNo <> AFileIdx) then
    raise EFileSystemException.CreateFmt('Invalid file number in file link (readed: %d, expected: %d)', [TgFileNo, AFileIdx]);
  Result.Map := Concat(Result.Map, [Integer(ADosDir.Entries[AFileIdx].FirstSector)]);
  Inc(Result.RealSize, TgDataLen);
  while not TgLast do
  begin
    if Result.SectorMapIndex(TgNextSector) >= 0 then
      raise EFileSystemException.Create('Circual reference in file map.');
    Result.Map := Concat(Result.Map, [TgNextSector]);
    Disk.ReadSector(TgNextSector, Buf);
    DecodeFileLink(Buf, TgFileNo, TgNextSector, TgDataLen, TgSeqNo, TgLast);
    if (FVTOC.Version <= 2) and (TgFileNo <> AFileIdx) then
      raise EFileSystemException.CreateFmt('Invalid file number in file link (readed: %d, expected: %d)', [TgFileNo, AFileIdx]);
    Inc(Result.RealSize, TgDataLen);
  end;
  Result.DirIdx := AFileIdx;
  Result.DirSector := ADosDir.StartSector;
end;

function TBaseDOSFileSystem.FindFreeSector: Integer;
var
  I: Integer;
begin
  if FVTOC.FreeSectors = 0 then
    Exit(0);
  for I := 1 to 720 do
    if VTOCBitmap[I] then
      Exit(I);
  Result := 0;
end;

function TBaseDOSFileSystem.AllocSector(ASectorIndex: Integer;
  var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean;
begin
  VTOCBitmap[ASectorIndex] := False;
  Dec(FVTOC.FreeSectors);
  Inc(ADosDir.Entries[AFileIndex].SectorLength);
  Result := True;
end;

procedure TBaseDOSFileSystem.DeallocSector(ASectorIndex: Integer;
  AStream: TDOSFileStream);
var
  DosDir: TDOSDirectory;
begin
  VTOCBitmap[ASectorIndex] := True;
  Inc(FVTOC.FreeSectors);
  DosDir := ReadDir(AStream.FMap.DirSector);
  Dec(DosDir.Entries[AStream.FMap.DirIdx].SectorLength);
  WriteDir(DosDir, AStream.FMap.DirIdx);
end;

procedure TBaseDOSFileSystem.DoDelete(const AMap: TFileMap;
  var ADir: TDOSDirectory; AFileIdx: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(AMap.Map) - 1 do
    VTOCBitmap[AMap.Map[I]] := True;
  Inc(FVTOC.FreeSectors, Length(AMap.Map));
  with ADir.Entries[AFileIdx].Status do
  begin
    Value := 0;
    Deleted := True;
  end;
end;

function TBaseDOSFileSystem.DoCreateFileStram(AFileName: String; AMode: Word
  ): TFSFileStream;
var
  FIdx: Integer;
  FreeBlock: Integer;
  Buffer: array[0..255] of Byte;
  SMap: TFileMap;
  DosDir: TDOSDirectory;
begin
  if not FindDir(IncludeTrailingPathDelimiter(ExtractFilePath(AFileName)), DosDir) then
    raise EFileSystemException.Create('Path not found: ' + ExtractFilePath(AFileName));
  FIdx := FileIndex(DosDir, ExtractFileName(AFileName));
  if FIdx < 0 then
    if AMode = fmCreate then
    begin
      FIdx := DirFreeEntry(DosDir);
      if FIdx < 0 then
        raise EFileSystemException.Create('Directory is full.');
      FreeBlock := FindFreeSector;
      if FreeBlock = 0 then
        raise EFileSystemException.Create('Disk full.');
      InitAttributes(DosDir, FIdx);
      with DosDir.Entries[FIdx] do
      begin
        FirstSector := FreeBlock;
        SectorLength := 0;
        FileName := AFileName;
      end;
      AllocSector(FreeBlock, DosDir, FIdx);
      Disk.ReadSector(FreeBlock, Buffer);
      EncodeFileLink(Buffer, FIdx, 0, 0, 0, True);
      Disk.WriteSector(FreeBlock, Buffer);
      WriteDir(DosDir, FIdx);
      WriteVTOC;
      SMap := Default(TFileMap);
      SMap.DirIdx := FIdx;
      SMap.DirSector := DosDir.StartSector;
      SMap.Map := [FreeBlock];
    end
    else
      raise EFileSystemException.Create('File not found: ' + AFileName)
  else
    SMap := FileMap(DosDir, FIdx);

  Result := TDOSFileStream.Create(Self, AFileName, AMode);
  with TDOSFileStream(Result) do
  begin
    FMap := SMap;
    if (AMode = fmCreate) and (FMap.RealSize > 0) then
      Size := 0
    else
      FPosition := 0;
  end;
end;

function TBaseDOSFileSystem.StreamRead(AStream: TFSFileStream; var Buffer;
  Count: Longint): Longint;
var
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  BlockBuffer: array[0..255] of Byte;
  DosStream: TDOSFileStream absolute AStream;
begin
  Result := 0;
  if (DosStream.FPosition = DosStream.FMap.RealSize) or (Count = 0) then
    Exit;
  if Count + DosStream.FPosition > DosStream.FMap.RealSize then
    Remains := DosStream.FMap.RealSize - DosStream.FPosition
  else
    Remains := Count;
  StartBlock := DosStream.FPosition div SectorDataSize;
  EndBlock := (DosStream.FPosition + Remains) div SectorDataSize;
  if (DosStream.FPosition + Remains) mod SectorDataSize > 0 then
    Inc(EndBlock);
  CurBlock := StartBlock;
  while Remains > 0 do
  begin
    if CurBlock >= Length(DosStream.FMap.Map) then
      Break;
    if CurBlock = StartBlock then
      StartOffset := DosStream.FPosition mod SectorDataSize
    else
      StartOffset := 0;
    if Remains > SectorDataSize - StartOffset then
      DataLen := SectorDataSize - StartOffset
    else
      DataLen := Remains;
    if Disk.ReadSector(DosStream.FMap.Map[CurBlock], BlockBuffer) <> Ord(Disk.SectorSize) then
      Break;
    Move(BlockBuffer[StartOffset], PByte(@Buffer)[Result], DataLen);
    Inc(Result, DataLen);
    Dec(Remains, DataLen);
    Inc(CurBlock);
    Inc(DosStream.FPosition, DataLen);
  end;
end;

function TBaseDOSFileSystem.StreamWrite(AStream: TFSFileStream; const Buffer;
  Count: Longint): Longint;
var
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  NewBlock: Integer;
  BlockBuffer: array[0..255] of Byte;
  DosDir: TDOSDirectory;
  DosStream: TDOSFileStream absolute AStream;
begin
  Result := 0;
  DosDir.StartSector := 0;
  if Count = 0 then
    Exit;
  if Count > FreeSpace then
  begin
    Remains := FreeSpace;
    if DosStream.FMap.RealSize mod SectorDataSize > 0 then
      Remains := Remains + SectorDataSize - DosStream.FMap.RealSize mod SectorDataSize;
  end
  else
    Remains := Count;
  StartBlock := DosStream.FPosition div SectorDataSize;
  EndBlock := (DosStream.FPosition + Remains) div SectorDataSize;
  if (DosStream.FPosition + Remains) mod SectorDataSize > 0 then
    Inc(EndBlock);
  NewBlock := 0;
  CurBlock := StartBlock;
  StartOffset := DosStream.FPosition mod SectorDataSize;
  while Remains > 0 do
  begin
    if Remains > SectorDataSize - StartOffset then
      DataLen := SectorDataSize - StartOffset
    else
      DataLen := Remains;

    if (Remains - DataLen > 0) and (Succ(CurBlock) >= Length(DosStream.FMap.Map)) then
    begin
      NewBlock := FindFreeSector;
      if DosStream.FMap.SectorMapIndex(NewBlock) >= 0 then
        raise EFileSystemException.Create('Circular reference in file map.');
      if NewBlock = 0 then
        Remains := DataLen
      else
      begin
        if DosDir.StartSector = 0 then
          DosDir := ReadDir(DosStream.FMap.DirSector);
        AllocSector(NewBlock, DosDir, DosStream.FMap.DirIdx);
        DosStream.FMap.Map := Concat(DosStream.FMap.Map, [NewBlock]);
      end;
    end;

    if Disk.ReadSector(DosStream.FMap.Map[CurBlock], BlockBuffer) <> Ord(Disk.SectorSize) then
      Break;
    Move(PByte(@Buffer)[Result], BlockBuffer[StartOffset], DataLen);

    EncodeFileLink(BlockBuffer, DosStream.FMap.DirIdx,
      specialize IfThen<Integer>(CurBlock = Pred(Length(DosStream.FMap.Map)),
        0, DosStream.FMap.Map[Succ(CurBlock)]),
      specialize IfThen<Integer>(CurBlock = Pred(Length(DosStream.FMap.Map)),
        specialize IfThen<Integer>((DosStream.FPosition + DataLen) mod SectorDataSize = 0,
          SectorDataSize, (DosStream.FPosition + DataLen) mod SectorDataSize),
        SectorDataSize),
      CurBlock, CurBlock = Pred(EndBlock));

    if Disk.WriteSector(DosStream.FMap.Map[CurBlock], BlockBuffer) <> Ord(Disk.SectorSize) then
      Break;
    Inc(Result, DataLen);
    Dec(Remains, DataLen);
    Inc(CurBlock);
    Inc(DosStream.FPosition, DataLen);
    if DosStream.FPosition > DosStream.FMap.RealSize then
      DosStream.FMap.RealSize := DosStream.FPosition;
    if (NewBlock > 0) or (DosDir.StartSector > 0) then
    begin
      WriteVTOC;
      WriteDir(DosDir, DosStream.FMap.DirIdx);
    end;
    NewBlock := 0;
    StartOffset := 0;
  end;
end;

function TBaseDOSFileSystem.StreamSeek(AStream: TFSFileStream;
  const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  DosStream: TDOSFileStream absolute AStream;
begin
  case Origin of
    soBeginning: begin
      if Offset < 0 then
        Result := 0
      else
        if Offset < DosStream.FMap.RealSize then
          Result := Offset
        else
          Result := DosStream.FMap.RealSize;
    end;
    soCurrent: begin
      if (DosStream.FPosition + Offset >= 0) then
        if (DosStream.FPosition + Offset <= DosStream.FMap.RealSize) then
          Result := DosStream.FPosition + Offset
        else
          Result := DosStream.FMap.RealSize
      else
        Result := 0;
    end;
    soEnd: begin
      if Offset < 0 then
        Result := DosStream.FMap.RealSize
      else
        if DosStream.FMap.RealSize - Offset < 0 then
          Result := 0
        else
          Result := DosStream.FMap.RealSize - Offset;
    end;
  end;
  DosStream.FPosition := Result;
end;

procedure TBaseDOSFileSystem.StreamSetSize(AStream: TFSFileStream;
  NewSize: Longint);
var
  I, J: Integer;
  Buffer: array[0..255] of Byte;
  DosDir: TDOSDirectory;
  DosStream: TDOSFileStream absolute AStream;
begin
  if NewSize = DosStream.FMap.RealSize then
    Exit;

  if NewSize < DosStream.FMap.RealSize then
  begin
    I := NewSize div SectorDataSize;
    if DosStream.FMap.RealSize div SectorDataSize > I then
      while I < Length(DosStream.FMap.Map) - 1 do
      begin
        DeallocSector(DosStream.FMap.Map[Succ(I)], DosStream);
        System.Delete(DosStream.FMap.Map, Succ(I), 1);
      end;

    if Disk.ReadSector(DosStream.FMap.Map[I], Buffer) <> Ord(Disk.SectorSize) then
      raise EFileSystemException.Create('Sector read error');

    EncodeFileLink(Buffer, DosStream.FMap.DirIdx, 0, NewSize mod SectorDataSize, I, True);

    if Disk.WriteSector(DosStream.FMap.Map[I], Buffer) <> Ord(Disk.SectorSize) then
      raise EFileSystemException.Create('Sector read error');

    DosDir := ReadDir(DosStream.FMap.DirSector);
    DosDir.Entries[DosStream.FMap.DirIdx].SectorLength := Length(DosStream.FMap.Map);

    WriteVTOC;
    WriteDir(DosDir, DosStream.FMap.DirIdx);

    DosStream.FPosition := NewSize;
    DosStream.FMap.RealSize := NewSize;
  end
  else
  begin
    FillByte(Buffer, SizeOf(Buffer), 0);
    StreamSeek(AStream, 0, soEnd);
    if (NewSize - DosStream.FMap.RealSize) > SizeOf(Buffer) then
      for I := 0 to (NewSize - DosStream.FMap.RealSize) div SizeOf(Buffer) do
        StreamWrite(AStream, Buffer, SizeOf(Buffer));
    J := (NewSize - DosStream.FMap.RealSize) mod SizeOf(Buffer);
    if J > 0 then
      StreamWrite(AStream, Buffer, J);
  end;
end;

class function TBaseDOSFileSystem.GetValidChars: TValidChars;
begin
  Result := VALID_FNAME_CHARS;
end;

constructor TBaseDOSFileSystem.Create(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
begin
  inherited Create(ADisk, AOptions);
  FSectorDataSize := Ord(ADisk.SectorSize) - 3;
  ReadVTOC;
end;

function TBaseDOSFileSystem.List(ADir: String; AFillExtAttr: Boolean;
  AShowDeleted: Boolean): TFileList;
var
  F: TFileListItem;
  I: Integer;
  M: TFileMap;
  DosDir: TDOSDirectory;
begin
  Result := [];
  if not FindDir(IncludeLeadingPathDelimiter(ADir), DosDir) then
    Exit;
  for I := 0 to 63 do
    if FileIsAvaiable(DosDir, I, AShowDeleted) then
    begin
      if not DosDir.Entries[I].Status.IsDir then
      begin
        M := FileMap(DosDir, I);
        F.Size := M.RealSize;
      end
      else
        F.Size := 0;
      F.Name := DosDir.Entries[I].FileName;
      F.Attributes := DosDir.Entries[I].Attributes;
      F.Time := 0;
      if AFillExtAttr then
        F.ExtAttributes := DosDir.Entries[I].ExtAttributes
      else
        F.ExtAttributes := [];
      Result := Concat(Result, [F]);
    end;
end;

function TBaseDOSFileSystem.FileExists(AFileName: String): Boolean;
var
  DosDir: TDOSDirectory;
begin
  Result := FindDir(IncludeLeadingPathDelimiter(ExtractFilePath(AFileName)), DosDir);
  if Result then
    Result := FileIsAvaiable(DosDir, FileIndex(DosDir, ExtractFileName(AFileName)), False);
end;

function TBaseDOSFileSystem.DirExists(ADir: String): Boolean;
begin
  Result := (ADir = '') or ((Length(ADir) = 1) and (ADir[1] in ['/','\']));
end;

procedure TBaseDOSFileSystem.Rename(AOldName, ANewName: String);
var
  FIdx: Integer;
  DosDir: TDOSDirectory;
begin
  if not FindDir(IncludeLeadingPathDelimiter(ExtractFilePath(AOldName)), DosDir) then
    raise EFileSystemException.Create('Path not found: ' + ExtractFilePath(AOldName));
  FIdx := FileIndex(DosDir, ExtractFileName(AOldName));
  if (FIdx < 0) or (not FileIsAvaiable(DosDir, FIdx, False)) then
    raise EFileSystemException.Create('File not found: ' + AOldName);
  if FileIndex(DosDir, ExtractFileName(ANewName)) >= 0 then
    raise EFileSystemException.Create('File allready exists: ' + ANewName);
  DosDir.Entries[FIdx].FileName := ExtractFileName(ANewName);
  WriteDir(DosDir, FIdx);
end;

procedure TBaseDOSFileSystem.Delete(AFileName: String);
var
  FIdx: Integer;
  M: TFileMap;
  DosDir: TDOSDirectory;
begin
  if not FindDir(IncludeLeadingPathDelimiter(ExtractFilePath(AFileName)), DosDir) then
    raise EFileSystemException.Create('Path not found: ' + ExtractFilePath(AFileName));
  FIdx := FileIndex(DosDir, ExtractFileName(AFileName));
  if (FIdx < 0) or (not FileIsAvaiable(DosDir, FIdx, False)) then
    raise EFileSystemException.Create('File not found: ' + AFileName);
  if DosDir.Entries[FIdx].Status.ReadOnly then
    raise EFileSystemException.Create('File is read-only: ' + AFileName);
  M := FileMap(DosDir, FIdx);
  DoDelete(M, DosDir, FIdx);
  WriteVTOC;
  WriteDir(DosDir, FIdx);
end;

procedure TBaseDOSFileSystem.Attrib(AFileName: String;
  AAttributes: TExtAttributes);
var
  A: TExtAttribute;
  FIdx: Integer;
  DosDir: TDOSDirectory;
begin
  if not FindDir(IncludeLeadingPathDelimiter(ExtractFilePath(AFileName)), DosDir) then
    raise EFileSystemException.Create('Path not found: ' + ExtractFilePath(AFileName));
  FIdx := FileIndex(DosDir, AFileName);
  if (FIdx < 0) or (not FileIsAvaiable(DosDir, FIdx, False)) then
    raise EFileSystemException.Create('File not found: ' + AFileName);
  for A in AAttributes do
    case A.Attribute of
      EA_READ_ONLY: DosDir.Entries[FIdx].Status.ReadOnly := A.Value;
      EA_OPEN_FOR_WRITE: DosDir.Entries[FIdx].Status.OpenForWrite := A.Value;
    end;
  WriteDir(DosDir, FIdx);
end;

function TBaseDOSFileSystem.ValidateFileName(AFileName: String): Boolean;
begin
  Result := DIT.FileSystem.Atari8.Utils.ValidateFileName(AFileName, GetValidChars);
end;

function TBaseDOSFileSystem.NormalizeFileName(AFileName: String): String;
begin
  Result := DIT.FileSystem.Atari8.Utils.NormalizeFileName(AFileName, GetValidChars);
end;

function TBaseDOSFileSystem.FreeSpace: Integer;
begin
  Result := FVTOC.FreeSectors * SectorDataSize;
end;

function TBaseDOSFileSystem.Capacity: Integer;
begin
  Result := FVTOC.MaxSectors * SectorDataSize;
end;

class function TBaseDOSFileSystem.AvaiableAttributes: TExtAttributesInfo;
const
  ATTR: TExtAttributesInfo = (
    (
      Attribute: EA_READ_ONLY;
      Name: 'ReadOnly';
      Description: 'File is read-only';
      ValueType: eatBoolean;
      ReadOnly: False;
    ),
    (
      Attribute: EA_FILE_EXIST;
      Name: 'FileExist';
      Description: 'File file exist';
      ValueType: eatBoolean;
      ReadOnly: True;
    ),
    (
      Attribute: EA_DELETED;
      Name: 'Deleted';
      Description: 'File deleted';
      ValueType: eatBoolean;
      ReadOnly: True;
    ),
    (
      Attribute: EA_START_SECTOR;
      Name: 'StartSector';
      Description: 'First sector of file';
      ValueType: eatInteger;
      ReadOnly: True;
    ),
    (
      Attribute: EA_SECTOR_COUNT;
      Name: 'SectorCount';
      Description: 'Number of sectors';
      ValueType: eatInteger;
      ReadOnly: True;
    ),
    (
      Attribute: EA_RAW_FILENAME;
      Name: 'RawName';
      Description: 'Raw file name';
      ValueType: eatString;
      ReadOnly: True;
    ),
    (
      Attribute: EA_RAW_EXT;
      Name: 'RawExt';
      Description: 'Raw file extension';
      ValueType: eatString;
      ReadOnly: True;
    ));
begin
  Result := ATTR;
end;

{ TDOSFileEntry }

function TDOSFileEntry.GetAttributes: TFileAttributes;
begin
  Result := [];
  if Status.ReadOnly then
    Include(Result, faReadOnly);
  if Status.IsDir then
    Include(Result, faDirectory);
end;

function TDOSFileEntry.GetExtAttributes: TExtAttributes;
begin
  Result := [];
  Result.Add(EA_OPEN_FOR_WRITE, Status.OpenForWrite);
  Result.Add(EA_DOS2_FLAG, Status.Dos2Flag);
  Result.Add(EA_READ_ONLY, Status.ReadOnly);
  Result.Add(EA_FILE_EXIST, Status.FileExists);
  Result.Add(EA_DELETED, Status.Deleted);
  Result.Add(EA_START_SECTOR, FirstSector);
  Result.Add(EA_SECTOR_COUNT, SectorLength);
  Result.Add(EA_RAW_FILENAME, Name);
  Result.Add(EA_RAW_EXT, Ext);
end;

function TDOSFileEntry.GetFileName: String;
begin
  Result := Trim(Name);
  if Trim(Ext) <> '' then
    Result := Result + '.' + Trim(Ext);
  if Result = '' then
    Result := '.';
end;

procedure TDOSFileEntry.SetFileName(AValue: String);
var
  S: String;
begin
  S := Trim(ExtractFileNameOnly(AValue));
  Name := PadRight(Copy(S, 1, 8), 8);
  S := Trim(ExtractFileExt(AValue));
  if (Length(S) >= 1) and (S[1] = '.') then
    Delete(S, 1, 1);
  S := Copy(S, 1, 3);
  Ext := PadRight(S, 3);
end;

{ TDOSVTOC }

function TDOSVTOC.GetSectorBit(AIndex: Integer): Boolean;
begin
  Result := Bitmap[AIndex div 8].Bits[7 - (AIndex mod 8)];
end;

procedure TDOSVTOC.SetSectorBit(AIndex: Integer; AValue: Boolean);
begin
  Bitmap[AIndex div 8].Bits[7 - (AIndex mod 8)] := AValue;
end;

{ TDOS25VTOC }

function TDOS25VTOC.GetSectorBit(AIndex: Integer): Boolean;
begin
  Result := Bitmap[AIndex div 8].Bits[7 - (AIndex mod 8)];
end;

procedure TDOS25VTOC.SetSectorBit(AIndex: Integer; AValue: Boolean);
begin
  Bitmap[AIndex div 8].Bits[7 - (AIndex mod 8)] := AValue;
end;

{ TDOS1FileSystem }

function TDOS1FileSystem.FileIsAvaiable(ADosDir: TDOSDirectory;
  AFileIdx: Integer; AShowDeleted: Boolean): Boolean;
begin
  Result := inherited FileIsAvaiable(ADosDir, AFileIdx, AShowDeleted)
    and ADosDir.Entries[AFileIdx].Status.FileExists;
end;

procedure TDOS1FileSystem.DecodeFileLink(const ABuf; var AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; var ALast: Boolean);
begin
  with PDOS1SectorTag(@ABuf + SectorDataSize)^ do
  begin
    AFileNo := FileNo;
    ANextSector := NextSector;
    ALast := FullFlag;
    if ALast then
    begin
      ADataSize := SeqNoOrSize;
      ASeqNo := -1;
    end
    else
    begin
      ADataSize := 125;
      ASeqNo := SeqNoOrSize;
    end;
  end;
end;

procedure TDOS1FileSystem.EncodeFileLink(var ABuf; AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; ALast: Boolean);
begin
  with PDOS1SectorTag(@ABuf + SectorDataSize)^ do
  begin
    FileNo := AFileNo;
    NextSector := ANextSector;
    FullFlag := ALast;
    if ALast then
      SeqNoOrSize := ADataSize
    else
      SeqNoOrSize := ASeqNo and %1111111;
  end;
end;

procedure TDOS1FileSystem.InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer
  );
begin
  ADosDir.Entries[AIndex].Status.Value := 0;
  ADosDir.Entries[AIndex].Status.FileExists := True;
end;

class function TDOS1FileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
var
  Buf: array[0..127] of Byte;
  VTOC: TDOSVTOC absolute Buf;
begin
  if ADisk.SectorSize <> ss128 then
    Exit(False);
  if (ADisk.SectorCount < 368) then
    Exit(False);
  ADisk.ReadSector(1, Buf);
  if Buf[0] <> 0 then
    Exit(False);
  FillChar(Buf, SizeOf(Buf), 0);
  ADisk.ReadSector(SECTOR_NO_VTOC, Buf);
  Result := (VTOC.Version = 1) and (VTOC.MaxSectors >= VTOC.FreeSectors)
    and (VTOC.SectorBit[SECTOR_NO_VTOC] = False) and (VTOC.SectorBit[SECTOR_NO_DIR] = False);
end;

class function TDOS1FileSystem.FileSystemName: String;
begin
  Result := 'Atari DOS 1';
end;

class function TDOS1FileSystem.FileSystemAlias: String;
begin
  Result := 'ADOS1';
end;

class procedure TDOS1FileSystem.Format(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
var
  Buf: array[0..255] of Byte;
  I: Integer;
  VTOC: TDOSVTOC absolute Buf;
  BootSects: Integer;
begin
  if (ADisk.SectorSize <> ss128) or (ADisk.SectorCount <> 720) then
    raise EFileSystemException.Create('Unsupported disk geometry.');
  FillByte(Buf, SizeOf(Buf), 0);
  for I := 1 to ADisk.SectorCount do
    if ADisk.WriteSector(I, Buf) <> Ord(ADisk.SectorSize) then
      raise EFileSystemException.Create('Block write error.');
  if ADisk.WriteSector(1, DOS1_BOOTSECTOR) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
  if Assigned(AOptions) and (AOptions is TAtariDOS1Options) then
    BootSects := TAtariDOS1Options(AOptions).BootSectorCount
  else
    BootSects := 1;
  if BootSects < 1 then
    BootSects := 1;
  VTOC.Version := 1;
  VTOC.MaxSectors := 710 - BootSects;
  VTOC.FreeSectors := 710 - BootSects;
  VTOC.SectorBit[0] := False;
  for I := 1 to BootSects do
    VTOC.SectorBit[I] := False;
  for I := 1 + BootSects to 359 do
    VTOC.SectorBit[I] := True;
  for I := 360 to 368 do
    VTOC.SectorBit[I] := False;
  for I := 369 to 719 do
    VTOC.SectorBit[I] := True;
  if ADisk.WriteSector(360, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
end;

class function TDOS1FileSystem.FileSystemOptionsClass: TFileSystemOptionsClass;
begin
  Result := TAtariDOS1Options;
end;

{ TDOS2FileSystem }

function TDOS2FileSystem.FileIsAvaiable(ADosDir: TDOSDirectory;
  AFileIdx: Integer; AShowDeleted: Boolean): Boolean;
begin
  Result := inherited FileIsAvaiable(ADosDir, AFileIdx, AShowDeleted) and ADosDir.Entries[AFileIdx].Status.FileExists;
end;

procedure TDOS2FileSystem.DecodeFileLink(const ABuf; var AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; var ALast: Boolean);
begin
  with PDOS2SectorTag(@ABuf + SectorDataSize)^ do
  begin
    AFileNo := FileNo;
    ANextSector := NextSector;
    ALast := ANextSector = 0;
    ADataSize := Size;
    ASeqNo := -1;
  end;
end;

procedure TDOS2FileSystem.EncodeFileLink(var ABuf; AFileNo, ANextSector,
  ADataSize, ASeqNo: Integer; ALast: Boolean);
begin
  with PDOS2SectorTag(@ABuf + SectorDataSize)^ do
  begin
    FileNo := AFileNo;
    NextSector := ANextSector;
    Size := ADataSize;
  end;
end;

procedure TDOS2FileSystem.InitAttributes(var ADosDir: TDOSDirectory; AIndex: Integer
  );
begin
  with ADosDir.Entries[AIndex].Status do
  begin
    Value := 0;
    FileExists := True;
    Dos2Flag := True;
  end;
end;

class procedure TDOS2FileSystem.InitVTOC(ADisk: TDiskImage; BootScts: Integer);
var
  Buf: array[0..255] of Byte;
  I: Integer;
  VTOC: TDOSVTOC absolute Buf;
begin
  FillByte(Buf, SizeOf(Buf), 0);
  VTOC.Version := 2;
  VTOC.MaxSectors := 710 - BootScts;
  VTOC.FreeSectors := 710 - BootScts;
  for I := 0 to BootScts do
    VTOC.SectorBit[I] := False;
  for I := BootScts + 1 to 359 do
    VTOC.SectorBit[I] := True;
  for I := 360 to 368 do
    VTOC.SectorBit[I] := False;
  for I := 369 to 719 do
    VTOC.SectorBit[I] := True;
  if ADisk.WriteSector(360, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
end;

class function TDOS2FileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
var
  Buf: array[0..255] of Byte;
  VTOC: TDOSVTOC absolute Buf;
begin
  if (ADisk.SectorSize <> ss128) and (ADisk.SectorSize <> ss256) then
    Exit(False);
  if (ADisk.SectorCount <> 720) then
    Exit(False);
  ADisk.ReadSector(1, Buf);
  if Buf[0] <> 0 then
    Exit(False);
  FillChar(Buf, SizeOf(Buf), 0);
  ADisk.ReadSector(SECTOR_NO_VTOC, Buf);
  Result := (VTOC.Version = 2) and (VTOC.MaxSectors >= VTOC.FreeSectors)
    and (VTOC.SectorBit[SECTOR_NO_VTOC] = False) and (VTOC.SectorBit[SECTOR_NO_DIR] = False);
end;

class function TDOS2FileSystem.FileSystemName: String;
begin
  Result := 'Atari DOS 2';
end;

class function TDOS2FileSystem.FileSystemAlias: String;
begin
  Result := 'ADOS2';
end;

class function TDOS2FileSystem.AvaiableAttributes: TExtAttributesInfo;
const
  ATTR: TExtAttributesInfo = (
    (
      Attribute: EA_OPEN_FOR_WRITE;
      Name: 'OpenForWrite';
      Description: 'File opened for write';
      ValueType: eatBoolean;
      ReadOnly: False;
    ),
    (
      Attribute: EA_DOS2_FLAG;
      Name: 'Dos2Flag';
      Description: 'File created with DOS 2>';
      ValueType: eatBoolean;
      ReadOnly: True;
    ));
begin
  Result := Concat(inherited AvaiableAttributes, ATTR);
end;

class procedure TDOS2FileSystem.Format(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
var
  Buf: array[0..255] of Byte;
  I: Integer;
  BootSects: Integer;
begin
  if ((ADisk.SectorSize <> ss128) and (ADisk.SectorSize <> ss256)) or (ADisk.SectorCount < 720) then
    raise EFileSystemException.Create('Unsupported disk geometry.');
  FillByte(Buf, SizeOf(Buf), 0);
  for I := 1 to ADisk.SectorCount do
    if ADisk.WriteSector(I, Buf) <> Ord(ADisk.SectorSize) then
      raise EFileSystemException.Create('Block write error.');
  Move(DOS2_BOOTSECTOR1, Buf, SizeOf(DOS2_BOOTSECTOR1));
  if ADisk.WriteSector(1, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
  FillByte(Buf, SizeOf(Buf), 0);
  Move(DOS2_BOOTSECTOR2, Buf, SizeOf(DOS2_BOOTSECTOR1));
  if ADisk.WriteSector(2, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
  if Assigned(AOptions) and (AOptions is TAtariDOS1Options) then
    BootSects := TAtariDOS1Options(AOptions).BootSectorCount
  else
    BootSects := 3;
  if BootSects < 1 then
    BootSects := 1;
  InitVTOC(ADisk, BootSects);
end;

class function TDOS2FileSystem.FileSystemOptionsClass: TFileSystemOptionsClass;
begin
  Result := TAtariDOS2Options;
end;

{ TDOS25FileSystem }

procedure TDOS25FileSystem.ReadVTOC;
var
  Buf: array[0..255] of Byte;
begin
  inherited ReadVTOC;
  Disk.ReadSector(SECTOR_NO_VTOC25, Buf);
  Move(Buf, FVTOC2, SizeOf(TDOS25VTOC));
end;

procedure TDOS25FileSystem.WriteVTOC;
var
  Buf: array[0..255] of Byte;
begin
  inherited WriteVTOC;
  Disk.ReadSector(SECTOR_NO_VTOC25, Buf);
  Move(FVTOC2, Buf, SizeOf(TDOS25VTOC));
  Disk.WriteSector(SECTOR_NO_VTOC25, Buf);
end;

function TDOS25FileSystem.AllocSector(ASectorIndex: Integer;
  var ADosDir: TDOSDirectory; AFileIndex: Integer): Boolean;
begin
  Result := False;
  if ASectorIndex < 720 then
  begin
    Result := inherited AllocSector(ASectorIndex, ADosDir, AFileIndex);
    if Result and (ASectorIndex >= 48) then
      FVTOC2.SectorBit[ASectorIndex - 48] := False;
  end
  else
  begin
    FVTOC2.SectorBit[ASectorIndex - 48] := False;
    Dec(FVTOC2.FreeSectors);
    Inc(ADosDir.Entries[AFileIndex].SectorLength);
    if ASectorIndex > 720 then
      with ADosDir.Entries[AFileIndex].Status do
      begin
        FileExists := False;
        OpenForWrite := True;
        Dos2Flag := True;
      end;
    Result := True;
  end;
end;

procedure TDOS25FileSystem.DeallocSector(ASectorIndex: Integer;
  AStream: TDOSFileStream);
var
  I: Integer;
  IsDOS25File: Boolean;
  DosDir: TDOSDirectory;
begin
  if ASectorIndex < 720 then
    inherited DeallocSector(ASectorIndex, AStream);
  if ASectorIndex >= 48 then
    FVTOC2.SectorBit[ASectorIndex - 48] := True;
  if ASectorIndex > 720 then
    Inc(FVTOC2.FreeSectors);
  IsDOS25File := False;
  for I in AStream.FMap.Map do
    if I > 720 then
    begin
      IsDOS25File := True;
      Break;
    end;
  DosDir := ReadDir(AStream.FMap.DirSector);
  with DosDir.Entries[AStream.FMap.DirIdx].Status do
    if IsDOS25File then
    begin
      Dos2Flag := True;
      OpenForWrite := True;
      FileExists := False;
    end
    else
    begin
      Dos2Flag := False;
      OpenForWrite := False;
      FileExists := True;
    end;
  WriteDir(DosDir, AStream.FMap.DirIdx);
end;

function TDOS25FileSystem.FindFreeSector: Integer;
const
  START_IDX = 720 - 48;
  END_IDX = Pred($80 * 8);
var
  I: Integer;
begin
  Result := inherited FindFreeSector;
  if Result = 0 then
  begin
    for I := START_IDX to END_IDX do
      if FVTOC2.SectorBit[I] then
        Exit(I + 48);
  end;
end;

function TDOS25FileSystem.FileIsAvaiable(ADosDir: TDOSDirectory;
  AFileIdx: Integer; AShowDeleted: Boolean): Boolean;
begin
  Result := inherited FileIsAvaiable(ADosDir, AFileIdx, AShowDeleted) or
    (ADosDir.Entries[AFileIdx].Status.Dos2Flag and ADosDir.Entries[AFileIdx].Status.OpenForWrite);
end;

procedure TDOS25FileSystem.DoDelete(const AMap: TFileMap;
  var ADir: TDOSDirectory; AFileIdx: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(AMap.Map) - 1 do
  begin
    VTOCBitmap[AMap.Map[I]] := True;
    if AMap.Map[I] <= 720 then
      Inc(FVTOC.FreeSectors)
    else
      Inc(FVTOC2.FreeSectors);
  end;
  with ADir.Entries[AFileIdx].Status do
  begin
    Value := 0;
    Deleted := True;
  end;
end;

function TDOS25FileSystem.GetVTOCBitmap(AIndex: Integer): Boolean;
var
  VT2: Boolean;
begin
  if AIndex < 720 then
  begin
    Result := inherited GetVTOCBitmap(AIndex);
    if AIndex >= 48 then
    begin
      VT2 := FVTOC2.SectorBit[AIndex - 48];
      if VT2 <> Result then
        ; // Co robimy?
    end;
  end
  else
    Result := FVTOC2.SectorBit[AIndex - 48];
end;

procedure TDOS25FileSystem.SetVTOCBitmap(AIndex: Integer; AValue: Boolean);
begin
  if AIndex < 720 then
  begin
    inherited SetVTOCBitmap(AIndex, AValue);
    if AIndex >= 48 then
      FVTOC2.SectorBit[AIndex - 48] := AValue;
  end else
    FVTOC2.SectorBit[AIndex - 48] := AValue;
end;

class procedure TDOS25FileSystem.InitVTOC(ADisk: TDiskImage; BootScts: Integer);
var
  Buf: array[0..255] of Byte;
  I: Integer;
  VTOC: TDOSVTOC absolute Buf;
  VTOC2: TDOS25VTOC absolute Buf;
begin
  FillByte(Buf, SizeOf(Buf), 0);
  VTOC.Version := 2;
  if ADisk.SectorCount > 720 then
    VTOC.MaxSectors := 1013 - BootScts
  else
    VTOC.MaxSectors := 710 - BootScts;
  VTOC.FreeSectors := 710 - BootScts;
  for I := 0 to BootScts do
    VTOC.SectorBit[I] := False;
  for I := BootScts + 1 to 359 do
    VTOC.SectorBit[I] := True;
  for I := 360 to 368 do
    VTOC.SectorBit[I] := False;
  for I := 369 to 719 do
    VTOC.SectorBit[I] := True;
  if ADisk.WriteSector(360, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
  if ADisk.SectorCount > 720 then
  begin
    FillByte(Buf, SizeOf(Buf), 0);
    for I := 48 - 48 to 1023 - 48 do
      VTOC2.SectorBit[I] := True;
    for I := 360 - 48 to 368 - 48 do
      VTOC2.SectorBit[I] := False;
    VTOC2.SectorBit[720 - 48] := False;
    VTOC2.FreeSectors := $12f;
    if ADisk.WriteSector(1024, Buf) <> Ord(ADisk.SectorSize) then
      raise EFileSystemException.Create('Block write error.');
  end;
end;

function TDOS25FileSystem.FreeSpace: Integer;
begin
  Result := inherited FreeSpace + FVTOC2.FreeSectors * SectorDataSize;
end;

class function TDOS25FileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
var
  Buf: array[0..255] of Byte;
  VTOC: TDOSVTOC absolute Buf;
begin
  if (ADisk.SectorSize <> ss128) {and (ADisk.SectorSize <> ss256)} then   // Atari DOS 2.5 nie obsluguje sektorow 256 ??
    Exit(False);
  if (ADisk.SectorCount <> 720) and (ADisk.SectorCount <> 1040) then
    Exit(False);
  ADisk.ReadSector(1, Buf);
  if Buf[0] <> 0 then
    Exit(False);
  FillChar(Buf, SizeOf(Buf), 0);
  ADisk.ReadSector(SECTOR_NO_VTOC, Buf);
  Result := (VTOC.Version = 2) and (VTOC.MaxSectors >= VTOC.FreeSectors)
    and (VTOC.SectorBit[SECTOR_NO_VTOC] = False) and (VTOC.SectorBit[SECTOR_NO_DIR] = False)
    and (VTOC.MaxSectors > 720);
end;

class procedure TDOS25FileSystem.Format(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
begin
  if ADisk.SectorSize <> ss128 then // Atari DOS 2.5 nie obsluguje sektorow 256 ???
    raise EFileSystemException.Create('Unsupported disk geometry.');
  inherited Format(ADisk, AOptions);
end;

class function TDOS25FileSystem.FileSystemName: String;
begin
  Result := 'Atari DOS 2.5';
end;

class function TDOS25FileSystem.FileSystemAlias: String;
begin
  Result := 'ADOS2.5';
end;

initialization
  RegisterFileSystem([TDOS1FileSystem, TDOS2FileSystem, TDOS25FileSystem, TMyDOSFileSystem]);

end.

