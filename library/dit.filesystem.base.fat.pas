{ DIT.FileSystem.Base.FAT

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


unit DIT.FileSystem.Base.FAT;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, DIT.FileSystem.Base, DIT.Image.Base;

type
  TBaseFATTable = class

  end;

  { TAbstractFATTable }

  generic TAbstractFATTable<FATElement> = class(TBaseFATTable)
  public type
    TElementType = FATElement;
    TTable = bitpacked array[0..0] of TElementType;
    PTable = ^TTable;
    TFileMap = array of TElementType;
    TElements = array of TElementType;
  private
    FSize: Integer;
    function GetItem(AIndex: TElementType): TElementType;
    procedure SetItem(AIndex: TElementType; AValue: TElementType);
  protected
    FTable: PTable;
  public
    constructor Create(ASize: Integer; AData: Pointer);
    destructor Destroy; override;
    class function ElementIn(const A: TElementType; const B: TElements): Boolean;
    function GetMap(AFirstCluster: TElementType): TFileMap;
    function Alloc(ACount: Integer): TFileMap;
    function FindFreeBlock(out ABlock: TElementType): Boolean;
    function CountFreeBlocks: Integer;
    function CountAvaiableBlocks: Integer;

    class function FreeElements: TElements; virtual; abstract;
    class function EndElements: TElements; virtual; abstract;
    class function ReservedElements: TElements; virtual; abstract;
    class function FreeElement: TElementType; virtual; abstract;
    class function EndElement: TElementType; virtual; abstract;
    class function ReservedElement: TElementType; virtual; abstract;
    class function IsDataElement(AElement: TElementType): Boolean; virtual; abstract;
    class function BitSize: Integer;

    property Table[AIndex: TElementType]: TElementType read GetItem write SetItem;
    property Size: Integer read FSize write FSize;
    property TableData: PTable read FTable write FTable;
  end;

  TFAT8TableElement = 0..$ff;
  TFAT8Table = specialize TAbstractFATTable<TFAT8TableElement>;

  TFAT12TableElement = 0..$fff;
  TFAT12Table = specialize TAbstractFATTable<TFAT12TableElement>;

  TFAT16TableElement = 0..$ffff;
  TFAT16Table = specialize TAbstractFATTable<TFAT16TableElement>;

  TFAT32TableElement = 0..$ffffffff;
  TFAT32Table = specialize TAbstractFATTable<TFAT32TableElement>;

  TBaseFATDir = class
  end;

  { TAbstractFATFileSystem }

  generic TAbstractFATFileSystem<FATElement; DIRClass: TBaseFATDir> = class(TClusterBasedFileSystem)
  public type
    TFATElementType = FATElement;
    TFATRawTable = bitpacked array[0..0] of TFATElementType;
    PFATRawTable = ^TFATRawTable;
    TFATElements = array of TFATElementType;
    TFATFileEntry = record
      Dir: DIRClass;
      Map: TFATElements;
      InternalPos: Integer;
      RealSize: Integer;
    end;
    TFATFileStream = class(TFSFileStream)
    protected
      FATFileEntry: TFATFileEntry;
    public
      destructor Destroy; override;
    end;
  private
    function GetFATTableElement(AIndex: TFATElementType): TFATElementType;
    procedure SetFATTableElement(AIndex: TFATElementType; AValue: TFATElementType);
  protected
    FFATData: PFATRawTable;

    function DoCreateFileStram(AFileName: String; AMode: Word): TFSFileStream; override;
    function StreamRead(AStream: TFSFileStream; var Buffer; Count: Longint): Longint; override;
    function StreamWrite(AStream: TFSFileStream; const Buffer; Count: Longint): Longint; override;
    function StreamSeek(AStream: TFSFileStream; const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure StreamSetSize(AStream: TFSFileStream; NewSize: Longint); override;

    function GetFileEntry(AFilePath: String; ACreate: Boolean): TFATFileEntry; virtual; abstract;
    function GetMap(AStart: TFATElementType): TFATElements; virtual;
    function FindFreeElement(out AElement: TFATElementType): Boolean; virtual; abstract;
    function AllocNewBlock(AStream: TFATFileStream; out AElement: TFATElementType): Boolean; virtual; abstract;
    function DeallocLastBlock(AStream: TFATFileStream): Boolean; virtual; abstract;

    class function FreeElements: TFATElements; virtual; abstract;
    class function EndElements: TFATElements; virtual; abstract;
    class function ReservedElements: TFATElements; virtual; abstract;
    class function IsDataElement(AElement: TFATElementType): Boolean; virtual; abstract;

    property FATTable[AIndex: TFATElementType]: TFATElementType read GetFATTableElement write SetFATTableElement;
  public
    constructor Create(ADisk: TDiskImage; AOptions: TFileSystemOptions = nil); override;
    destructor Destroy; override;
  end;

implementation

{ TAbstractFATFileSystem.TFATFileStream }

destructor TAbstractFATFileSystem.TFATFileStream.Destroy;
begin
  if Assigned(FATFileEntry.Dir) then
    FATFileEntry.Dir.Free;
  inherited Destroy;
end;

{ TAbstractFATFileSystem }

function TAbstractFATFileSystem.GetFATTableElement(AIndex: TFATElementType
  ): TFATElementType;
begin
  Result := FFATData[AIndex];
end;

procedure TAbstractFATFileSystem.SetFATTableElement(AIndex: TFATElementType;
  AValue: TFATElementType);
begin
  FFATData[AIndex] := AValue;
end;

function TAbstractFATFileSystem.DoCreateFileStram(AFileName: String; AMode: Word
  ): TFSFileStream;
var
  Entry: TFATFileEntry;
begin
  Entry := GetFileEntry(AFileName, AMode and fmCreate = fmCreate);
  Result := TFATFileStream.Create(Self, AFileName, AMode);
  TFATFileStream(Result).FATFileEntry := Entry;
end;

function TAbstractFATFileSystem.StreamRead(AStream: TFSFileStream; var Buffer;
  Count: Longint): Longint;
var
  DosStream: TFATFileStream absolute AStream;
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  BlockBuffer: array of Byte;
begin
  if (Count = 0) or (DosStream.FATFileEntry.InternalPos = DosStream.FATFileEntry.RealSize) then
    Exit(0);
  if Count + DosStream.FATFileEntry.InternalPos > DosStream.FATFileEntry.RealSize then
    Remains := DosStream.FATFileEntry.RealSize - DosStream.FATFileEntry.InternalPos
  else
    Remains := Count;
  StartBlock := DosStream.FATFileEntry.InternalPos div ClusterSize;
  EndBlock := (DosStream.FATFileEntry.InternalPos + Remains) div ClusterSize;
  if (DosStream.FATFileEntry.InternalPos + Remains) mod ClusterSize > 0 then
    Inc(EndBlock);
  CurBlock := StartBlock;
  SetLength(BlockBuffer, ClusterSize);
  while Remains > 0 do
  begin
    if CurBlock >= Length(DosStream.FATFileEntry.Map) then
      Break;
    if CurBlock = StartBlock then
      StartOffset := DosStream.FATFileEntry.InternalPos mod ClusterSize
    else
      StartOffset := 0;
    if Remains > ClusterSize - StartOffset then
      DataLen := ClusterSize - StartOffset
    else
      DataLen := Remains;
    if ReadCluster(DosStream.FATFileEntry.Map[CurBlock], BlockBuffer[0]) <> ClusterSize then
      Break;
    Move(BlockBuffer[StartOffset], PByte(@Buffer)[Result], DataLen);
    Inc(Result, DataLen);
    Dec(Remains, DataLen);
    Inc(CurBlock);
    Inc(DosStream.FATFileEntry.InternalPos, DataLen);
  end;
end;

function TAbstractFATFileSystem.StreamWrite(AStream: TFSFileStream;
  const Buffer; Count: Longint): Longint;
var
  StartBlock, EndBlock, CurBlock: Integer;
  StartOffset, DataLen: Integer;
  Remains: Integer;
  NewBlock: TFATElementType;
  BlockAllocated: Boolean;
  BlockBuffer: array of Byte;
  DosStream: TFATFileStream absolute AStream;
begin
  Result := 0;
  if Count = 0 then
    Exit;
  if Count > FreeSpace then
  begin
    Remains := FreeSpace;
    if DosStream.FATFileEntry.RealSize mod ClusterSize > 0 then
      Remains := Remains + ClusterSize - DosStream.FATFileEntry.RealSize mod ClusterSize;
  end
  else
    Remains := Count;
  StartBlock := DosStream.FATFileEntry.InternalPos div ClusterSize;
  EndBlock := (DosStream.FATFileEntry.InternalPos + Remains) div ClusterSize;
  if (DosStream.FATFileEntry.InternalPos + Remains) mod ClusterSize > 0 then
    Inc(EndBlock);
  BlockAllocated := False;
  CurBlock := StartBlock;
  StartOffset := DosStream.FATFileEntry.InternalPos mod ClusterSize;
  while Remains > 0 do
  begin
    if Remains > ClusterSize - StartOffset then
      DataLen := ClusterSize - StartOffset
    else
      DataLen := Remains;

    if (Remains - DataLen > 0) and (Succ(CurBlock) >= Length(DosStream.FATFileEntry.Map)) then
    begin
      if not AllocNewBlock(DosStream, NewBlock) then
        raise EFileSystemException.Create('Can not allocate free cluster.');
      BlockAllocated := True;
    end;
(*
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
*)
  end;
end;

function TAbstractFATFileSystem.StreamSeek(AStream: TFSFileStream;
  const Offset: Int64; Origin: TSeekOrigin): Int64;
begin

end;

procedure TAbstractFATFileSystem.StreamSetSize(AStream: TFSFileStream;
  NewSize: Longint);
begin

end;

function TAbstractFATFileSystem.GetMap(AStart: TFATElementType): TFATElements;
var
  I: TFATElementType;
begin
  Result := [AStart];
  I := FATTable[AStart];
  while IsDataElement(I) do
  begin
    Result := Concat(Result, [I]);
    I := FATTable[I];
  end;
end;

constructor TAbstractFATFileSystem.Create(ADisk: TDiskImage;
  AOptions: TFileSystemOptions);
begin
  inherited Create(ADisk, AOptions);
end;

destructor TAbstractFATFileSystem.Destroy;
begin
  if FFATData <> nil then
    FreeMemAndNil(FFATData);
  inherited Destroy;
end;

function TAbstractFATTable.GetItem(AIndex: TElementType): TElementType;
begin
  if AIndex >= FSize then
    raise EFileSystemException.Create('FAT Index out of bounds');
  Result := FTable^[AIndex];
end;

procedure TAbstractFATTable.SetItem(AIndex: TElementType; AValue: TElementType);
begin
  if AIndex >= FSize then
    raise EFileSystemException.Create('FAT Index out of bounds');
  FTable^[AIndex] := AValue;
end;

constructor TAbstractFATTable.Create(ASize: Integer; AData: Pointer);
begin
  Size := ASize;
  FTable := AData;
end;

destructor TAbstractFATTable.Destroy;
begin
  Size := 0;
  inherited Destroy;
end;

class function TAbstractFATTable.ElementIn(const A: TElementType; const B: TElements): Boolean;
var
  I: TElementType;
begin
  for I in B do
    if I = A then
      Exit(True);
  Result := False;
end;

function TAbstractFATTable.GetMap(AFirstCluster: TElementType): TFileMap;
var
  I: TElementType;
begin
  Result := [AFirstCluster];
  I := Table[AFirstCluster];
  while IsDataElement(I) do
  begin
    Result := Concat(Result, [I]);
    I := Table[I];
  end;
end;

function TAbstractFATTable.Alloc(ACount: Integer): TFileMap;
var
  FreeEl, PrevEl: TElementType;
begin
  Result := [];
  PrevEl := EndElement;
  while (ACount > 0) and FindFreeBlock(FreeEl) do
  begin
    if ACount = 1 then
      Table[FreeEl] := EndElement
    else if PrevEl <> EndElement then
      Table[PrevEl] := FreeEl;
    Result := Concat(Result, [FreeEl]);
    Dec(ACount);
    PrevEl := FreeEl;
  end;
end;

function TAbstractFATTable.FindFreeBlock(out ABlock: TElementType): Boolean;
begin
  ABlock := 0;
  while not ElementIn(Table[ABlock], FreeElements) and (ABlock < Size) do
    Inc(ABlock);
  Result := ElementIn(Table[ABlock], FreeElements);
end;

function TAbstractFATTable.CountFreeBlocks: Integer;
var
  El: TElementType;
begin
  Result := 0;
  for El := 0 to Size - 1 do
    if ElementIn(Table[El], FreeElements) then
      Inc(Result);
end;

function TAbstractFATTable.CountAvaiableBlocks: Integer;
var
  El: TElementType;
begin
  Result := 0;
  for El := 0 to Size - 1 do
    if not ElementIn(Table[El], ReservedElements) then
      Inc(Result);
end;

class function TAbstractFATTable.BitSize: Integer;
var
  I: LongWord;
begin
  Result := 0;
  I := High(TElementType);
  while I > 0 do
  begin
    I := I shr 1;
    Inc(Result);
  end;
end;

end.

