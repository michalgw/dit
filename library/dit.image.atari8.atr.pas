{ DIT.Image.Atari8.ATR

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

unit DIT.Image.Atari8.ATR;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, DIT.Image.Base;

const
  ATR_MAGIC = $0296;

type

  { TATRHeader }

  TATRHeader = packed record
  private
    function GetSize: Integer;
    procedure SetSize(AValue: Integer);
  public
    Magic: Word;
    SizeLo: Word;
    SectorSize: Word;
    SizeHi: Byte;
    Unused: array[0..8] of Byte;
    property Size: Integer read GetSize write SetSize;
  end;

  { TATRFormatOptions }

  TATRFormatOptions = class(TDiskImageFormatOptions)
  private
    FShortFormat: Boolean;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShortFormat: Boolean read FShortFormat write FShortFormat;
  end;

  { TATRDiskImage }

  TATRDiskImage = class(TRAWDiskImage)
  private
    FHeader: TATRHeader;
    FAtrType: Integer;
    procedure WriteHeader;
  protected
    function DataOffset: Integer; override;
    function GetSectorSize: TSectorSize; override;
    procedure SetSectorSize(AValue: TSectorSize); override;
    function GetSectorCount: Integer; override;
    procedure SetSectorCount(AValue: Integer); override;
    function DoReadSector(ASectorNo: Integer; var Buffer): Integer; override;
    function DoWriteSector(ASectorNo: Integer; const Buffer): Integer; override;
  public
    constructor Create(AStream: TStream); override;
    procedure InitImage(ASectorSize: TSectorSize; ASectorCount: Integer;
      AFormatOptions: TDiskImageFormatOptions = nil); override;
    procedure Load(AOptions: TDiskImageFormatOptions = nil); override;
    class function FormatInfo: TDiskImageFormatInfo; override;
    class function Detect(AStream: TStream): Boolean; override;
    class function FormatOptionsClass: TDiskImageFormatOptionsClass; override;
  end;

implementation

const
  ATR_FormatInfo: TDiskImageFormatInfo = (
    Name: 'ATR Atari disk image';
    FormatAlias: 'ATR';
    FileExt: ('.atr')
  );

{ TATRHeader }

function TATRHeader.GetSize: Integer;
begin
  Result := (SizeHi shl 16) + SizeLo;
end;

procedure TATRHeader.SetSize(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  SizeLo := AValue and $FFFF;
  SizeHi := (AValue and $FF0000) shr 16;
end;

{ TATRFormatOptions }

constructor TATRFormatOptions.Create;
begin
  ShortFormat := False;
end;

procedure TATRFormatOptions.Assign(Source: TPersistent);
begin
  //inherited Assign(Source);
  if Source is TATRFormatOptions then
    ShortFormat := TATRFormatOptions(Source).ShortFormat;
end;

{ TATRDiskImage }

procedure TATRDiskImage.WriteHeader;
begin
  Stream.Position := 0;
  Stream.Write(FHeader, SizeOf(FHeader));
end;

function TATRDiskImage.DataOffset: Integer;
begin
  Result := SizeOf(TATRHeader);
end;

function TATRDiskImage.GetSectorSize: TSectorSize;
begin
  Result := TSectorSize(FHeader.SectorSize);
end;

procedure TATRDiskImage.SetSectorSize(AValue: TSectorSize);
begin
  FHeader.SectorSize := Ord(AValue);
  WriteHeader;
end;

function TATRDiskImage.GetSectorCount: Integer;
var
  Size: Integer;
begin
  Size := FHeader.Size * 16;
  if FAtrType = 2 then
    Size := Size + 384;
  Result := Size div Ord(SectorSize);
end;

procedure TATRDiskImage.SetSectorCount(AValue: Integer);
var
  Size: Integer;
begin
  Size := AValue * Ord(SectorSize);
  if FAtrType = 2 then
    Size := Size + 384;
  FHeader.Size := Size div 16;
  Stream.Size := Size + DataOffset;
  WriteHeader;
end;

function TATRDiskImage.DoReadSector(ASectorNo: Integer; var Buffer): Integer;
begin
  case FAtrType of
    1: begin
      Stream.Position := ASectorNo * Ord(SectorSize) + DataOffset;
      Result := Stream.Read(Buffer, Ord(SectorSize));
    end;
    2: begin
      if ASectorNo >= 3 then
      begin
        Stream.Position := (ASectorNo - 3) * 256 + DataOffset + 384;
        Result := Stream.Read(Buffer, Ord(SectorSize));
      end
      else
      begin
        FillByte(Buffer, 256, 0);
        Stream.Position := ASectorNo * 128 + DataOffset;
        Result := Stream.Read(Buffer, 128);
        Inc(Result, 128);
      end;
    end;
    otherwise
      Result := inherited DoReadSector(ASectorNo, Buffer)
  end;
end;

function TATRDiskImage.DoWriteSector(ASectorNo: Integer; const Buffer): Integer;
var
  Buf: array[0..127] of Byte;
begin
  case FAtrType of
    1: begin
      Stream.Position := ASectorNo * Ord(SectorSize) + DataOffset;
      Result := Stream.Write(Buffer, Ord(SectorSize));
    end;
    2: begin
      if ASectorNo >= 3 then
      begin
        Stream.Position := (ASectorNo - 3) * 256 + DataOffset + 384;
        Result := Stream.Write(Buffer, Ord(SectorSize));
      end
      else
      begin
        Stream.Position := ASectorNo * 128 + DataOffset;
        Result := Stream.Write(Buffer, 128);
        FillByte(Buf, SizeOf(Buf), 0);
        Result := Result + Stream.Write(Buf, SizeOf(Buf));
      end;
    end;
    otherwise
      Result := inherited DoWriteSector(ASectorNo, Buffer);
  end;
end;

constructor TATRDiskImage.Create(AStream: TStream);
begin
  inherited Create(AStream);
  SectorOffset := -1;
end;

procedure TATRDiskImage.InitImage(ASectorSize: TSectorSize;
  ASectorCount: Integer; AFormatOptions: TDiskImageFormatOptions);
var
  Ofs: Integer;
begin
  if Assigned(AFormatOptions) and (ASectorSize = ss256) and (AFormatOptions is TATRFormatOptions)
    and (TATRFormatOptions(AFormatOptions).ShortFormat) then
  begin
    FAtrType := 2;
    Ofs := 384;
  end
  else
  begin
    FAtrType := 1;
    Ofs := 0;
  end;
  FillByte(FHeader, SizeOf(FHeader), 0);
  FHeader.Magic := ATR_MAGIC;
  FHeader.Size := (ASectorCount * Ord(ASectorSize) - Ofs) div 16;
  FHeader.SectorSize := Ord(ASectorSize);
  Stream.Size := ASectorCount * Ord(ASectorSize) + DataOffset - Ofs;
  WriteHeader;
end;

procedure TATRDiskImage.Load(AOptions: TDiskImageFormatOptions);
begin
  if Stream.Size < DataOffset then
    raise EDiskImageException.Create('Invalid ATR file');
  Stream.Position := 0;
  Stream.Read(FHeader, DataOffset);
  if FHeader.Magic <> ATR_MAGIC then
    raise EDiskImageException.Create('Invalid ATR magic number');
  if FHeader.SectorSize = 256 then
  begin
    if (FHeader.Size * 16) mod 256 = 0 then
      FAtrType := 1
    else if (FHeader.Size * 16) mod 256 = 128 then
      FAtrType := 2
    else
      FAtrType := 0;
  end
  else
    FAtrType := 1;
end;

class function TATRDiskImage.FormatInfo: TDiskImageFormatInfo;
begin
  Result := ATR_FormatInfo;
end;

class function TATRDiskImage.Detect(AStream: TStream): Boolean;
var
  W: Word;
begin
  AStream.Seek(0, soFromBeginning);
  AStream.Read(W, SizeOf(W));
  AStream.Seek(0, soFromBeginning);
  Result := W = ATR_MAGIC;
end;

class function TATRDiskImage.FormatOptionsClass: TDiskImageFormatOptionsClass;
begin
  Result := TATRFormatOptions;
end;

initialization
  RegisterDiskImageFormat(TATRDiskImage);

end.

