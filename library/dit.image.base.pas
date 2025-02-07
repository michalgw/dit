{ DIT.Image.Base.pas

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

unit DIT.Image.Base;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  EDiskImageException = class(Exception);

  TSectorSize = (ss128 = 128, ss256 = 256, ss512 = 512);

  { TDiskImageFormatInfo }

  TDiskImageFormatInfo = record
    Name: String;
    FormatAlias: String;
    FileExt: array of String;
    function SupportedFileExt(AExt: String): Boolean;
  end;

  { TDiskImageFormatOptions }

  TDiskImageFormatOptions = class(TPersistent)
    constructor Create; virtual;
  end;

  TDiskImageFormatOptionsClass = class of TDiskImageFormatOptions;

  { TDiskImage }

  TDiskImage = class
  private
    FSectorOffset: Integer;
  protected
    function GetReadOnly: Boolean; virtual; abstract;
    function GetSectorCount: Integer; virtual; abstract;
    function GetSectorSize: TSectorSize; virtual; abstract;
    procedure SetReadOnly(AValue: Boolean); virtual; abstract;
    procedure SetSectorCount(AValue: Integer); virtual; abstract;
    procedure SetSectorSize(AValue: TSectorSize); virtual; abstract;
    function DoReadSector(ASectorNo: Integer; var Buffer): Integer; virtual; abstract;
    function DoWriteSector(ASectorNo: Integer; const Buffer): Integer; virtual; abstract;
  public
    procedure InitImage(ASectorSize: TSectorSize; ASectorCount: Integer; AFormatOptions: TDiskImageFormatOptions = nil); virtual; abstract;
    procedure Load(AOptions: TDiskImageFormatOptions = nil); virtual; abstract;
    function ReadSector(ASectorNo: Integer; var Buffer): Integer; virtual;
    function WriteSector(ASectorNo: Integer; const Buffer): Integer; virtual;
    class function FormatInfo: TDiskImageFormatInfo; virtual;
    class function FormatOptionsClass: TDiskImageFormatOptionsClass; virtual;
    class function Detect(AStream: TStream): Boolean; virtual;
    property SectorSize: TSectorSize read GetSectorSize write SetSectorSize;
    property SectorCount: Integer read GetSectorCount write SetSectorCount;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SectorOffset: Integer read FSectorOffset write FSectorOffset;
  end;

  { TStreamDiskImage }

  TStreamDiskImage = class(TDiskImage)
  private
    FStream: TStream;
  protected
    function DataOffset: Integer; virtual;
    function DoReadSector(ASectorNo: Integer; var Buffer): Integer; override;
    function DoWriteSector(ASectorNo: Integer; const Buffer): Integer; override;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream); virtual;
  end;

  TDiskImageClass = class of TStreamDiskImage;

  { TRAWDiskImage }

  TRAWDiskImage = class(TStreamDiskImage)
  private
    FReadOnly: Boolean;
    FSectorSize: TSectorSize;
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(AValue: Boolean); override;
    function GetSectorSize: TSectorSize; override;
    procedure SetSectorSize(AValue: TSectorSize); override;
    function GetSectorCount: Integer; override;
    procedure SetSectorCount(AValue: Integer); override;
  public
    class function FormatInfo: TDiskImageFormatInfo; override;
  end;

  TDiskImageFormats = specialize TList<TDiskImageClass>;

function KnownDiskImageFormats: TDiskImageFormats;
procedure RegisterDiskImageFormat(AFormat: TDiskImageClass);
function FindDiskImageClassByExt(AExt: String): TDiskImageClass;

implementation

const
  RAWDiskImageInfo: TDiskImageFormatInfo = (
    Name: 'RAW image';
    FormatAlias: 'RAW';
    FileExt: ('.raw')
  );

var
  RegisteredDiskImageFormats: TDiskImageFormats = nil;

function KnownDiskImageFormats: TDiskImageFormats;
begin
  if not Assigned(RegisteredDiskImageFormats) then
    RegisteredDiskImageFormats := TDiskImageFormats.Create;
  Result := RegisteredDiskImageFormats;
end;

procedure RegisterDiskImageFormat(AFormat: TDiskImageClass);
begin
  if not Assigned(RegisteredDiskImageFormats) then
    RegisteredDiskImageFormats := TDiskImageFormats.Create;
  RegisteredDiskImageFormats.Add(AFormat);
end;

function FindDiskImageClassByExt(AExt: String): TDiskImageClass;
begin
  if not Assigned(RegisteredDiskImageFormats) then
    Exit(nil);
  for Result in RegisteredDiskImageFormats do
    if Result.FormatInfo.SupportedFileExt(AExt) then
      Exit;
  Result := nil;
end;

{ TDiskImageFormatOptions }

constructor TDiskImageFormatOptions.Create;
begin

end;

{ TDiskImageFormatInfo }

function TDiskImageFormatInfo.SupportedFileExt(AExt: String): Boolean;
var
  S: String;
begin
  Result := False;
  if Copy(AExt, 1, 1) <> '.' then
    Exit;
  AExt := LowerCase(AExt);
  for S in FileExt do
    if AExt = LowerCase(S) then
      Exit(True);
end;

{ TRAWDiskImage }

function TRAWDiskImage.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TRAWDiskImage.SetReadOnly(AValue: Boolean);
begin
  FReadOnly := AValue;
end;

function TRAWDiskImage.GetSectorSize: TSectorSize;
begin
  Result := FSectorSize;
end;

procedure TRAWDiskImage.SetSectorSize(AValue: TSectorSize);
begin
  FSectorSize := AValue;
end;

function TRAWDiskImage.GetSectorCount: Integer;
begin
  Result := (Stream.Size - DataOffset) div Ord(SectorSize);
end;

procedure TRAWDiskImage.SetSectorCount(AValue: Integer);
begin
  Stream.Size := Ord(SectorSize) * AValue + DataOffset;
end;

class function TRAWDiskImage.FormatInfo: TDiskImageFormatInfo;
begin
  Result := RAWDiskImageInfo;
end;

{ TDiskImage }

function TDiskImage.ReadSector(ASectorNo: Integer; var Buffer): Integer;
begin
  if (ASectorNo + SectorOffset < 0) or (ASectorNo + SectorOffset > Pred(SectorCount)) or (Ord(SectorSize) = 0) then
    raise EDiskImageException.CreateFmt('ReadSector: Invalid sector number %d', [ASectorNo]);
  Result := DoReadSector(ASectorNo + SectorOffset, Buffer);
end;

function TDiskImage.WriteSector(ASectorNo: Integer; const Buffer): Integer;
begin
  if ReadOnly then
    raise EDiskImageException.Create('Disk is read-only');
  if (ASectorNo + SectorOffset < 0) or (ASectorNo + SectorOffset > Pred(SectorCount)) or (Ord(SectorSize) = 0) then
    raise EDiskImageException.CreateFmt('WriteSector: Invalid sector number %d', [ASectorNo]);
  Result := DoWriteSector(ASectorNo + SectorOffset, Buffer);
end;

class function TDiskImage.FormatInfo: TDiskImageFormatInfo;
begin
  Result := Default(TDiskImageFormatInfo);
end;

class function TDiskImage.FormatOptionsClass: TDiskImageFormatOptionsClass;
begin
  Result := nil;
end;

class function TDiskImage.Detect(AStream: TStream): Boolean;
begin
  Result := False;
end;

{ TStreamDiskImage }

function TStreamDiskImage.DataOffset: Integer;
begin
  Result := 0;
end;

function TStreamDiskImage.DoReadSector(ASectorNo: Integer; var Buffer): Integer;
begin
  Stream.Position := ASectorNo * Ord(SectorSize) + DataOffset;
  Result := Stream.Read(Buffer, Ord(SectorSize));
end;

function TStreamDiskImage.DoWriteSector(ASectorNo: Integer; const Buffer
  ): Integer;
begin
  Stream.Position := ASectorNo * Ord(SectorSize) + DataOffset;
  Result := Stream.Write(Buffer, Ord(SectorSize));
end;

constructor TStreamDiskImage.Create(AStream: TStream);
begin
  FStream := AStream;
end;

initialization
  RegisterDiskImageFormat(TRAWDiskImage);

finalization
  if Assigned(RegisteredDiskImageFormats) then
    FreeAndNil(RegisteredDiskImageFormats);

end.

