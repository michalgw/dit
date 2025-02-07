{ DIT.FileSystem.Atari8.DOS3

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

unit DIT.FileSystem.Atari8.DOS3;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, DIT.FileSystem.Base, DIT.FileSystem.Base.FAT,
  DIT.Image.Base, DIT.FileSystem.Atari8.Utils;

const
  ADOS3_FAT_END = $fd;
  ADOS3_FAT_FREE = $fe;
  ADOS3_FAT_RESERVED = $ff;

  ADOS3_FAT_END_A: TFAT8Table.TElements = (ADOS3_FAT_END);
  ADOS3_FAT_FREE_A: TFAT8Table.TElements = (ADOS3_FAT_FREE);
  ADOS3_FAT_RESERVED_A: TFAT8Table.TElements = (ADOS3_FAT_RESERVED);

  ADOS3_FAT_SIZE_SD = 87;
  ADOS3_FAT_SIZE_ED = 127;

  EA_ADOS3_OPEN_FOR_WRITE = EA_USER + 1;
  EA_ADOS3_READ_ONLY = EA_USER + 2;
  EA_ADOS3_FILE_EXIST = EA_USER + 3;
  EA_ADOS3_ENTRY_EXIST = EA_USER + 4;
  EA_ADOS3_BLOCK_COUNT = EA_USER + 5;
  EA_ADOS3_FIRST_BLOCK = EA_USER + 6;
  EA_ADOS3_FILE_SIZE = EA_USER + 7;
  EA_ADOS3_RAW_FILE_NAME = EA_USER + 8;
  EA_ADOS3_RAW_FILE_EXT = EA_USER + 9;

type
  TAtariDOS3StatusByte = bitpacked record
    case Boolean of
      False: (
        Value: Byte;
      );
      True: (
        OpenForWrite,
        ReadOnly,
        Unused2,
        Unused3,
        Unused4,
        Unused5,
        FileExists,
        EntryExists: Boolean;
      )
  end;

  { TAtariDOS3DirEntry }

  TAtariDOS3DirEntry = packed record
  private
    function GetAttributes: TFileAttributes;
    function GetExtAttributes: TExtAttributes;
    function GetFileName: String;
    function GetRealSize: Integer;
    procedure SetFileName(AValue: String);
    procedure SetRealSize(AValue: Integer);
  public
    Status: TAtariDOS3StatusByte;
    Name: array[0..7] of Char;
    Ext: array[0..2] of Char;
    BlockCount: Byte;
    FirstBlock: Byte;
    FileSize: Word;
    property FileName: String read GetFileName write SetFileName;
    property Attributes: TFileAttributes read GetAttributes;
    property ExtAttributes: TExtAttributes read GetExtAttributes;
    property RealSize: Integer read GetRealSize write SetRealSize;
  end;

  TAtariDOS3Dir = array[0..63] of TAtariDOS3DirEntry;

  { TAtariDOSFAT8Table }

  TAtariDOSFAT8Table = class(TFAT8Table)
  public
    class function FreeElement: TElementType; override;
    class function FreeElements: TElements; override;
    class function EndElement: TElementType; override;
    class function EndElements: TElements; override;
    class function ReservedElement: TElementType; override;
    class function ReservedElements: TElements; override;
    class function IsDataElement(AElement: TElementType): Boolean; override;
  end;

  { TAtariDOS3FileMap }

  TAtariDOS3FileMap = record
    Map: TAtariDOSFAT8Table.TFileMap;
    RealSize: Integer;
    DirIndex: Integer;
    function SectorMapIndex(ACluster: TAtariDOSFAT8Table.TElementType): Integer;
  end;

  TAtariDOS3FileStream = class(TFSFileStream)
  private
    FMap: TAtariDOS3FileMap;
    FPosition: Integer;
  end;

  { TAtariDOS3FileSystem }

  TAtariDOS3FileSystem = class(TClusterBasedFileSystem)
  private
    FMapData: TAtariDOSFAT8Table.PTable;
    FFatMap: TAtariDOSFAT8Table;
    FDir: TAtariDOS3Dir;
    procedure ReadVTOC;
    procedure WriteDir(AIndex: Integer);
    procedure WriteFAT;
    function FileIsAvaiable(AIndex: Integer): Boolean;
    function FileIndex(AFileName: String): Integer;
    function FindFreeDirEntry: Integer;
  protected
    function DoCreateFileStram(AFileName: String; AMode: Word): TFSFileStream; override;
    function StreamRead(AStream: TFSFileStream; var Buffer; Count: Longint): Longint; override;
    function StreamWrite(AStream: TFSFileStream; const Buffer; Count: Longint): Longint; override;
    function StreamSeek(AStream: TFSFileStream; const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure StreamSetSize(AStream: TFSFileStream; NewSize: Longint); override;
  public
    constructor Create(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); override;
    destructor Destroy; override;
    function List(ADir: String = ''; AFillExtAttr: Boolean = False; AShowDeleted: Boolean = False): TFileList; override;
    function DirExists(ADir: String): Boolean; override;
    function FileExists(AFileName: String): Boolean; override;
    function ValidateFileName(AFileName: String): Boolean; override;
    function NormalizeFileName(AFileName: String): String; override;
    procedure Delete(AFileName: String); override;
    procedure Rename(AOldName, ANewName: String); override;
    procedure Attrib(AFileName: String; AAttibutes: TExtAttributes); override;
    function FreeSpace: Integer; override;
    function Capacity: Integer; override;
    class function Detect(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil): Boolean; override;
    class procedure Format(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); override;
    class function FileSystemName: String; override;
    class function AvaiableAttributes: TExtAttributesInfo; override;
  end;

implementation

uses
  LazFileUtils, StrUtils;

{ TAtariDOS3FileMap }

function TAtariDOS3FileMap.SectorMapIndex(
  ACluster: TAtariDOSFAT8Table.TElementType): Integer;
begin
  for Result := 0 to Length(Map) - 1 do
    if Map[Result] = ACluster then
      Exit;
  Result := -1;
end;

{ TAtariDOS3DirEntry }

function TAtariDOS3DirEntry.GetAttributes: TFileAttributes;
begin
  Result := [];
  if Status.ReadOnly then
    Include(Result, faReadOnly);
end;

function TAtariDOS3DirEntry.GetExtAttributes: TExtAttributes;
begin
  Result := [];
  Result.Add(EA_ADOS3_OPEN_FOR_WRITE, Status.OpenForWrite);
  Result.Add(EA_ADOS3_READ_ONLY, Status.ReadOnly);
  Result.Add(EA_ADOS3_FILE_EXIST, Status.FileExists);
  Result.Add(EA_ADOS3_ENTRY_EXIST, Status.EntryExists);
  Result.Add(EA_ADOS3_BLOCK_COUNT, BlockCount);
  Result.Add(EA_ADOS3_FIRST_BLOCK, FirstBlock);
  Result.Add(EA_ADOS3_FILE_SIZE, FileSize);
  Result.Add(EA_ADOS3_RAW_FILE_NAME, Name);
  Result.Add(EA_ADOS3_RAW_FILE_EXT, Ext);
end;

function TAtariDOS3DirEntry.GetFileName: String;
begin
  Result := Trim(Name);
  if Trim(Ext) <> '' then
    Result := Result + '.' + Trim(Ext);
  if Result = '' then
    Result := '.';
end;

function TAtariDOS3DirEntry.GetRealSize: Integer;
begin
  Result := (LongWord(BlockCount * 1024) and $ffff0000) or FileSize;
end;

procedure TAtariDOS3DirEntry.SetFileName(AValue: String);
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

procedure TAtariDOS3DirEntry.SetRealSize(AValue: Integer);
begin
  BlockCount := AValue div 1024;
  if AValue mod 1024 > 0 then
    Inc(BlockCount);
  FileSize := AValue and $ffff;
end;

{ TAtariDOSFAT8Table }

class function TAtariDOSFAT8Table.FreeElement: TElementType;
begin
  Result := ADOS3_FAT_FREE;
end;

class function TAtariDOSFAT8Table.FreeElements: TElements;
begin
  Result := ADOS3_FAT_FREE_A;
end;

class function TAtariDOSFAT8Table.EndElement: TElementType;
begin
  Result := ADOS3_FAT_END;
end;

class function TAtariDOSFAT8Table.EndElements: TElements;
begin
  Result := ADOS3_FAT_END_A;
end;

class function TAtariDOSFAT8Table.ReservedElement: TElementType;
begin
  Result := ADOS3_FAT_RESERVED;
end;

class function TAtariDOSFAT8Table.ReservedElements: TElements;
begin
  Result := ADOS3_FAT_RESERVED_A;
end;

class function TAtariDOSFAT8Table.IsDataElement(AElement: TElementType
  ): Boolean;
begin
  Result := not (AElement in [ADOS3_FAT_FREE, ADOS3_FAT_END, ADOS3_FAT_RESERVED]);
end;

{ TAtariDOS3FileSystem }

procedure TAtariDOS3FileSystem.ReadVTOC;
var
  I: Integer;
begin
  FillByte(FMapData^, 128, 0);
  FillByte(FDir, SizeOf(TAtariDOS3Dir), 0);
  for I := 0 to 7 do
    Disk.ReadSector(16 + I, FDir[I * 8]);
  Disk.ReadSector(24, FMapData^);
end;

procedure TAtariDOS3FileSystem.WriteDir(AIndex: Integer);
var
  S, E, I: Integer;
begin
  if AIndex < 0 then
  begin
    S := 0;
    E := 7;
  end
  else
  begin
    S := AIndex div 8;
    E := S;
  end;
  for I := S to E do
    Disk.WriteSector(16 + I, FDir[I * 8]);
end;

procedure TAtariDOS3FileSystem.WriteFAT;
begin
  Disk.WriteSector(24, FMapData^);
end;

function TAtariDOS3FileSystem.FileIsAvaiable(AIndex: Integer): Boolean;
begin
  Result := (AIndex > 0) and (AIndex <= 63) and FDir[AIndex].Status.FileExists
    and FDir[AIndex].Status.EntryExists;
end;

function TAtariDOS3FileSystem.FileIndex(AFileName: String): Integer;
begin
  for Result := 1 to 63 do
    if FileIsAvaiable(Result) and (FDir[Result].FileName = AFileName) then
      Exit;
  Result := -1;
end;

function TAtariDOS3FileSystem.FindFreeDirEntry: Integer;
begin
  for Result := 1 to 63 do
    if not FileIsAvaiable(Result) then
      Exit;
  Result := -1;
end;

function TAtariDOS3FileSystem.DoCreateFileStram(AFileName: String; AMode: Word
  ): TFSFileStream;
var
  Map: TAtariDOS3FileMap;
  FirstEl: TAtariDOSFAT8Table.TElementType;
begin
  Map.DirIndex := FileIndex(ExtractFileName(AFileName));
  if Map.DirIndex < 0 then
    if AMode = fmCreate then
    begin
      if not FFatMap.FindFreeBlock(FirstEl) then
        raise EFileSystemException.Create('Disk full.');
      Map.DirIndex := FindFreeDirEntry;
      if Map.DirIndex < 1 then
        raise EFileSystemException.Create('Directory is full.');
      Map.Map := [FirstEl];
      Map.RealSize := 0;
      with FDir[Map.DirIndex] do
      begin
        FileName := NormalizeFileName(ExtractFileName(AFileName));
        FirstBlock := FirstEl;
        FileSize := 0;
        BlockCount := 1;
        Status.Value := 0;
        Status.EntryExists := True;
        Status.FileExists := True;
      end;
      FFatMap.Table[FirstEl] := ADOS3_FAT_END;
      WriteDir(Map.DirIndex);
      WriteFAT;
    end
    else
      raise EFileSystemException.Create('File not found: ' + AFileName)
  else
  begin
    Map.Map := FFatMap.GetMap(FDir[Map.DirIndex].FirstBlock);
    Map.RealSize := FDir[Map.DirIndex].RealSize;
  end;

  Result := TAtariDOS3FileStream.Create(Self, AFileName, AMode);
  with TAtariDOS3FileStream(Result) do
  begin
    FMap := Map;
    FPosition := 0;
  end;
end;

function TAtariDOS3FileSystem.StreamRead(AStream: TFSFileStream; var Buffer;
  Count: Longint): Longint;
var
  DosStream: TAtariDOS3FileStream absolute AStream;
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  BlockBuffer: array[0..1023] of Byte;
begin
  if (Count = 0) or (DosStream.FPosition = DosStream.FMap.RealSize) then
    Exit(0);
  if Count + DosStream.FPosition > DosStream.FMap.RealSize then
    Remains := DosStream.FMap.RealSize - DosStream.FPosition
  else
    Remains := Count;
  StartBlock := DosStream.FPosition div ClusterSize;
  EndBlock := (DosStream.FPosition + Remains) div ClusterSize;
  if (DosStream.FPosition + Remains) mod ClusterSize > 0 then
    Inc(EndBlock);
  CurBlock := StartBlock;
  while Remains > 0 do
  begin
    if CurBlock >= Length(DosStream.FMap.Map) then
      Break;
    if CurBlock = StartBlock then
      StartOffset := DosStream.FPosition mod ClusterSize
    else
      StartOffset := 0;
    if Remains > ClusterSize - StartOffset then
      DataLen := ClusterSize - StartOffset
    else
      DataLen := Remains;
    if ReadCluster(DosStream.FMap.Map[CurBlock], BlockBuffer) <> ClusterSize then
      Break;
    Move(BlockBuffer[StartOffset], PByte(@Buffer)[Result], DataLen);
    Inc(Result, DataLen);
    Dec(Remains, DataLen);
    Inc(CurBlock);
    Inc(DosStream.FPosition, DataLen);
  end;
end;

function TAtariDOS3FileSystem.StreamWrite(AStream: TFSFileStream; const Buffer;
  Count: Longint): Longint;
var
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  NewBlock: TAtariDOSFAT8Table.TElementType;
  BlockAllocated: Boolean;
  BlockBuffer: array[0..1023] of Byte;
  DosStream: TAtariDOS3FileStream absolute AStream;
begin
  Result := 0;
  if Count = 0 then
    Exit;
  if Count > FreeSpace then
  begin
    Remains := FreeSpace;
    if DosStream.FMap.RealSize mod ClusterSize > 0 then
      Remains := Remains + ClusterSize - DosStream.FMap.RealSize mod ClusterSize;
  end
  else
    Remains := Count;
  StartBlock := DosStream.FPosition div ClusterSize;
  EndBlock := (DosStream.FPosition + Remains) div ClusterSize;
  if (DosStream.FPosition + Remains) mod ClusterSize > 0 then
    Inc(EndBlock);
  BlockAllocated := False;
  CurBlock := StartBlock;
  StartOffset := DosStream.FPosition mod ClusterSize;
  while Remains > 0 do
  begin
    if Remains > ClusterSize - StartOffset then
      DataLen := ClusterSize - StartOffset
    else
      DataLen := Remains;

    if (Remains - DataLen > 0) and (Succ(CurBlock) >= Length(DosStream.FMap.Map)) then
    begin
      if not FFatMap.FindFreeBlock(NewBlock) then
        raise EFileSystemException.Create('Can not allocate free cluster.');
      if DosStream.FMap.SectorMapIndex(NewBlock) >= 0 then
        raise EFileSystemException.Create('Circular reference in file map.');
      FFatMap.Table[DosStream.FMap.Map[Length(DosStream.FMap.Map) - 1]] := NewBlock;
      FFatMap.Table[NewBlock] := ADOS3_FAT_END;
      DosStream.FMap.Map := Concat(DosStream.FMap.Map, [NewBlock]);
      BlockAllocated := True;
    end;

    if ReadCluster(DosStream.FMap.Map[CurBlock], BlockBuffer) <> ClusterSize then
      Break;
    Move(PByte(@Buffer)[Result], BlockBuffer[StartOffset], DataLen);

    if WriteCluster(DosStream.FMap.Map[CurBlock], BlockBuffer) <> ClusterSize then
      Break;
    Inc(Result, DataLen);
    Dec(Remains, DataLen);
    Inc(CurBlock);
    Inc(DosStream.FPosition, DataLen);
    if DosStream.FPosition > DosStream.FMap.RealSize then
      DosStream.FMap.RealSize := DosStream.FPosition;
    if BlockAllocated then
      WriteFAT;
    if BlockAllocated or (FDir[DosStream.FMap.DirIndex].RealSize <> DosStream.FMap.RealSize) then
    begin
      FDir[DosStream.FMap.DirIndex].RealSize := DosStream.FMap.RealSize;
      WriteDir(DosStream.FMap.DirIndex);
    end;
    BlockAllocated := False;
    StartOffset := 0;
  end;
end;

function TAtariDOS3FileSystem.StreamSeek(AStream: TFSFileStream;
  const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  DosStream: TAtariDOS3FileStream absolute AStream;
begin
  case Origin of
    soBeginning: begin
      if Offset < DosStream.FMap.RealSize then
      begin
        DosStream.FPosition := Offset;
        Result := Offset;
      end
      else
      begin
        DosStream.FPosition := DosStream.FMap.RealSize;
        Result := DosStream.FMap.RealSize;
      end;
    end;
    soCurrent: begin
      if (DosStream.FPosition + Offset >= 0) then
        if (DosStream.FPosition + Offset <= DosStream.FMap.RealSize) then
        begin
          DosStream.FPosition := DosStream.FPosition + Offset;
          Result := DosStream.FPosition;
        end
        else
        begin
          DosStream.FPosition := DosStream.FMap.RealSize;
          Result := DosStream.FMap.RealSize;
        end
      else
      begin
        DosStream.FPosition := 0;
        Result := 0;
      end;
    end;
    soEnd: begin
      if Offset + DosStream.FMap.RealSize < 0 then
      begin
        DosStream.FPosition := 0;
        Result := 0;
      end
      else
      begin
        DosStream.FPosition := DosStream.FMap.RealSize - Offset;
        Result := DosStream.FPosition;
      end;
    end;
  end;
end;

procedure TAtariDOS3FileSystem.StreamSetSize(AStream: TFSFileStream;
  NewSize: Longint);
var
  I, J: Integer;
  Buffer: array[0..1023] of Byte;
  DosStream: TAtariDOS3FileStream absolute AStream;
begin
  if NewSize = DosStream.FMap.RealSize then
    Exit;

  if NewSize < DosStream.FMap.RealSize then
  begin
    I := NewSize div ClusterSize;
    if I = 0 then
      I := 1;
    if DosStream.FMap.RealSize div ClusterSize > I then
      while I < Length(DosStream.FMap.Map) - 1 do
      begin
        FFatMap.Table[DosStream.FMap.Map[Length(DosStream.FMap.Map) - 1]] := ADOS3_FAT_FREE;
        System.Delete(DosStream.FMap.Map, Length(DosStream.FMap.Map) - 1, 1);
        FFatMap.Table[DosStream.FMap.Map[Length(DosStream.FMap.Map) - 1]] := ADOS3_FAT_END;
      end;

    FDir[DosStream.FMap.DirIndex].RealSize := NewSize;

    WriteFAT;
    WriteDir(DosStream.FMap.DirIndex);

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

constructor TAtariDOS3FileSystem.Create(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
begin
  inherited Create(ADisk, AOptions);
  ClusterSize := 1024;
  SectorsPerCluster := 8;
  FirstClusterOffset := 25;
  ClusterCount := (Disk.SectorCount - 24) div 8;
  FMapData := AllocMem(128);
  FillByte(FMapData^, 128, 0);
  FFatMap := TAtariDOSFAT8Table.Create(specialize IfThen<Integer>(ADisk.SectorCount = 720, 87, 127), FMapData);
  ReadVTOC;
end;

destructor TAtariDOS3FileSystem.Destroy;
begin
  FFatMap.Free;
  FreeMemAndNil(FMapData);
  inherited Destroy;
end;

function TAtariDOS3FileSystem.List(ADir: String; AFillExtAttr: Boolean;
  AShowDeleted: Boolean): TFileList;
var
  I: Integer;
  F: TFileListItem;
begin
  Result := [];
  for I := 1 to 63 do
    if FileIsAvaiable(I) then
      with FDir[I] do
      begin
        F := Default(TFileListItem);
        F.Name := FileName;
        F.Attributes := Attributes;
        F.Size := RealSize;
        F.Time := 0;
        if AFillExtAttr then
          F.ExtAttributes := FDir[I].ExtAttributes;
        Result := Concat(Result, [F]);
      end;
end;

function TAtariDOS3FileSystem.DirExists(ADir: String): Boolean;
begin
  Result := (ADir = '') or ((Length(ADir) = 1) and (ADir[1] in ['/','\']));
end;

function TAtariDOS3FileSystem.FileExists(AFileName: String): Boolean;
begin
  Result := FileIndex(ExtractFileName(AFileName)) > 0;
end;

function TAtariDOS3FileSystem.ValidateFileName(AFileName: String): Boolean;
begin
  Result := DIT.FileSystem.Atari8.Utils.ValidateFileName(AFileName, ['0'..'9', 'A'..'Z']);
end;

function TAtariDOS3FileSystem.NormalizeFileName(AFileName: String): String;
begin
  Result := DIT.FileSystem.Atari8.Utils.NormalizeFileName(AFileName, ['0'..'9', 'A'..'Z']);
end;

procedure TAtariDOS3FileSystem.Delete(AFileName: String);
var
  DirIdx, I: Integer;
  Map: TAtariDOSFAT8Table.TFileMap;
begin
  DirIdx := FileIndex(ExtractFileName(AFileName));
  if DirIdx < 1 then
    raise EFileSystemException.Create('File not found: ' + AFileName);
  Map := FFatMap.GetMap(FDir[DirIdx].FirstBlock);
  for I := 0 to Length(Map) - 1 do
    FFatMap.Table[Map[I]] := ADOS3_FAT_FREE;
  FDir[DirIdx].Status.Value := 0;
  FDir[DirIdx].Status.EntryExists := True;
  WriteDir(DirIdx);
  WriteFAT;
end;

procedure TAtariDOS3FileSystem.Rename(AOldName, ANewName: String);
var
  Idx: Integer;
begin
  Idx := FileIndex(ExtractFileName(AOldName));
  if Idx > 0 then
  begin
    FDir[Idx].FileName := NormalizeFileName(ExtractFileName(ANewName));
    WriteDir(Idx);
  end
  else
    raise EFileSystemException.Create('File not found: ' + AOldName);
end;

procedure TAtariDOS3FileSystem.Attrib(AFileName: String;
  AAttibutes: TExtAttributes);
var
  Idx: Integer;
  A: TExtAttribute;
begin
  Idx := FileIndex(AFileName);
  if Idx > 0 then
  begin
    for A in AAttibutes do
      case A.Attribute of
        EA_ADOS3_READ_ONLY: FDir[Idx].Status.ReadOnly := A.Value;
      end;
    WriteDir(Idx);
  end
  else
    raise EFileSystemException.Create('File not foind: ' + AFileName);
end;

function TAtariDOS3FileSystem.FreeSpace: Integer;
begin
  Result := FFatMap.CountFreeBlocks * 1024;
end;

function TAtariDOS3FileSystem.Capacity: Integer;
begin
  Result := FFatMap.CountAvaiableBlocks * 1024;
end;

class function TAtariDOS3FileSystem.Detect(ADisk: TDiskImage;
  AOptions: TFileSystemOptions): Boolean;
var
  Buf: array[0..127] of Byte;
begin
  Result := False;
  if (ADisk.SectorSize <> ss128) or
    ((ADisk.SectorCount <> 720) and (ADisk.SectorCount <> 1040)) then
    Exit;
  ADisk.ReadSector(1, Buf);
  if (Buf[0] <> $01)
    or (Buf[1] <> $09)
    or (Buf[2] <> $00)
    or (Buf[3] <> $32)
    or (Buf[4] <> $06)
    or (Buf[5] <> $32) then
    Exit;
  ADisk.ReadSector(16, Buf);
  if ((Buf[14] <> $57) and (Buf[14] <> $7f)) or (Buf[15] <> $a5) then
    Exit;
  Result := True;
end;

class procedure TAtariDOS3FileSystem.Format(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
var
  Buf: array[0..127] of Byte;
  I: Integer;
begin
  if (ADisk.SectorSize <> ss128) or ((ADisk.SectorCount <> 720) and (ADisk.SectorCount <> 1040)) then
    EFileSystemException.Create('Invalid disk format.');

  FillByte(Buf, SizeOf(Buf), 0);
  for I := 1 to ADisk.SectorCount do
    if ADisk.WriteSector(I, Buf) <> Ord(ADisk.SectorSize) then
      raise EFileSystemException.Create('Block write error.');

  Buf[0] := $01;
  Buf[1] := $09;
  Buf[2] := $00;
  Buf[3] := $32;
  Buf[4] := $06;
  Buf[5] := $32;
  Buf[6] := $A2;
  Buf[7] := $00;
  Buf[8] := $38;
  Buf[9] := $60;
  if ADisk.WriteSector(1, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');

  FillByte(Buf, SizeOf(Buf), 0);
  if ADisk.SectorCount = 720 then
    Buf[$E] := $57
  else
    Buf[$E] := $7F;
  Buf[$F] := $A5;
  if ADisk.WriteSector(16, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');

  FillByte(Buf, SizeOf(Buf), ADOS3_FAT_RESERVED);
  if ADisk.SectorCount = 720 then
    I := 87
  else
    I := 127;
  FillByte(Buf, I, ADOS3_FAT_FREE);
  if ADisk.WriteSector(24, Buf) <> Ord(ADisk.SectorSize) then
    raise EFileSystemException.Create('Block write error.');
end;

class function TAtariDOS3FileSystem.FileSystemName: String;
begin
  Result := 'Atari DOS 3';
end;

class function TAtariDOS3FileSystem.AvaiableAttributes: TExtAttributesInfo;
const
  ATTR: TExtAttributesInfo = (
    (
      Attribute: EA_ADOS3_OPEN_FOR_WRITE;
      Name: 'OpenForWrite';
      Description: 'File is open for write';
      ValueType: eatBoolean;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_READ_ONLY;
      Name: 'ReadOnly';
      Description: 'File is read-only';
      ValueType: eatBoolean;
      ReadOnly: False;
    ),
    (
      Attribute: EA_ADOS3_FILE_EXIST;
      Name: 'FileExist';
      Description: 'File file exist';
      ValueType: eatBoolean;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_ENTRY_EXIST;
      Name: 'Deleted';
      Description: 'Entry exist';
      ValueType: eatBoolean;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_BLOCK_COUNT;
      Name: 'ClusterCount';
      Description: 'Number of clusters';
      ValueType: eatInteger;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_FIRST_BLOCK;
      Name: 'FirstCluster';
      Description: 'First cluster';
      ValueType: eatInteger;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_FILE_SIZE;
      Name: 'RawFileSize';
      Description: 'File size (first 16 bits)';
      ValueType: eatInteger;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_RAW_FILE_NAME;
      Name: 'RawName';
      Description: 'Raw file name';
      ValueType: eatString;
      ReadOnly: True;
    ),
    (
      Attribute: EA_ADOS3_RAW_FILE_EXT;
      Name: 'RawExt';
      Description: 'Raw file extension';
      ValueType: eatString;
      ReadOnly: True;
    ));
begin
  Result := ATTR;
end;

//initialization
//  RegisterFileSystem(TAtariDOS3FileSystem);

end.

