unit uFSTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DIT.Image.Base, DIT.FileSystem.Base;

type

  { TImageAndFileSystem }

  TImageAndFileSystem = class
    FileName: String;
    FileStream: TFileStream;
    Image: TStreamDiskImage;
    FileSystem: TFileSystem;
    constructor Create(AFileName: String; AMode: Word;
      AImageClass: TDiskImageClass = nil;
      AFileSystemClass: TFileSystemClass = nil);
    destructor Destroy; override;
  end;

function AttribsToStr(AFileAttr: TFileAttributes): String;

implementation

function AttribsToStr(AFileAttr: TFileAttributes): String;
begin
  if faReadOnly in AFileAttr then
    Result := 'R'
  else
    Result := '-';
  if faHidden in AFileAttr then
    Result := Result + 'H'
  else
    Result := Result + '-';
  if faSysFile in AFileAttr then
    Result := Result + 'S'
  else
    Result := Result + '-';
  if faVolumeID in AFileAttr then
    Result := Result + 'V'
  else
    Result := Result + '-';
  if faDirectory in AFileAttr then
    Result := Result + 'D'
  else
    Result := Result + '-';
  if faArchive in AFileAttr then
    Result := Result + 'A'
  else
    Result := Result + '-';
  if faSymLink in AFileAttr then
    Result := Result + 'L'
  else
    Result := Result + '-';
end;

{ TImageAndFileSystem }

constructor TImageAndFileSystem.Create(AFileName: String; AMode: Word;
  AImageClass: TDiskImageClass; AFileSystemClass: TFileSystemClass);
var
  I: Integer;
begin
  Image := nil;
  FileStream := nil;
  FileSystem := nil;
  if AImageClass = nil then
    AImageClass := FindDiskImageClassByExt(ExtractFileExt(AFileName));
  if AImageClass = nil then
    raise Exception.Create('Unknown image format...');
  FileName := AFileName;
  FileStream := TFileStream.Create(AFileName, AMode);
  if not AImageClass.Detect(FileStream) then
  begin
    FreeAndNil(FileStream);
    raise Exception.Create('Image format not recogized: ' + AImageClass.FormatInfo.Name);
  end;
  //try
    Image := AImageClass.Create(FileStream);
    Image.Load;
    if AFileSystemClass = nil then
    begin
      for I := 0 to KnownFileSystems.Count - 1 do
        if KnownFileSystems[I].Detect(Image) then
        begin
          AFileSystemClass := KnownFileSystems[I];
          Break;
        end;
    end;
    if AFileSystemClass = nil then
    begin
      FreeAndNil(Image);
      FreeAndNil(FileStream);
      raise Exception.Create('File system not recogized.');
    end;
    FileSystem := AFileSystemClass.Create(Image);
  //except
  //  if Assigned(FileSystem) then
  //    FreeAndNil(FileSystem);
  //  if Assigned(Image) then
  //    FreeAndNil(Image);
  //  if Assigned(FileStream) then
  //    FreeAndNil(FileStream);
  //  raise;
  //end;
end;

destructor TImageAndFileSystem.Destroy;
begin
  if Assigned(FileSystem) then
    FreeAndNil(FileSystem);
  if Assigned(Image) then
    FreeAndNil(Image);
  if Assigned(FileStream) then
    FreeAndNil(FileStream);
  inherited Destroy;
end;

end.

