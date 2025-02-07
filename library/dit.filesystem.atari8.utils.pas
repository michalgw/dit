{ DIT.FileSystem.Atari8.Utils

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

unit DIT.FileSystem.Atari8.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TValidChars = set of Char;

function ValidateFileName(AFileName: String; AValidChars: TValidChars): Boolean;
function NormalizeFileName(AFileName: String; AValidChars: TValidChars): String;

implementation

uses
  LazFileUtils;

function ValidateFileName(AFileName: String; AValidChars: TValidChars): Boolean;

function ValidStr(S: String): Boolean;
var
  Ch: Char;
begin
  for Ch in S do
    if not (Ch in AValidChars) then
      Exit(False);
  Result := True;
end;

var
  FName: String;
  FExt: String;
begin
  case AFileName of
    '': Exit(False);
    '.': Exit(True);
    else
    begin
      FName := ExtractFileNameOnly(AFileName);
      FExt := ExtractFileExt(AFileName);
      if Pos('.', FExt) = 1 then
        System.Delete(FExt, 1, 1);
      if ((Length(FName) > 0) and (not (FName[1] in ['A'..'Z']))) or (Length(FName) > 8)
        or (Length(FExt) > 3) then
        Exit(False);
      if (Length(FName) = 0) and (Length(FExt) = 0) then
        Exit(True); // AFileName = '.'
      Result := ValidStr(FName) and ValidStr(FExt);
    end;
  end;
end;

function NormalizeFileName(AFileName: String; AValidChars: TValidChars): String;
var
  FName, FExt: String;
  I: Integer;
begin
  Result := '';
  FName := UpperCase(ExtractFileNameOnly(AFileName));
  FExt := UpperCase(ExtractFileExt(AFileName));
  if Copy(FExt, 1, 1) = '.' then
    System.Delete(FExt, 1, 1);
  for I := 1 to Length(FName) do
  begin
    if Length(Result) = 0 then
    begin
      if FName[I] in ['A'..'Z'] then
        Result := Result + FName[I];
      Continue;
    end;
    if FName[I] in AValidChars then
      Result := Result + FName[I];
    if Length(Result) = 8 then
      Break;
  end;
  FName := '';
  for I := 1 to Length(FExt) do
    if FExt[I] in AValidChars then
    begin
      FName := FName + FExt[I];
      if Length(FName) = 3 then
        Break;
    end;
  if FName <> '' then
    Result := Result + '.' + FName;
end;

end.

