{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit diskimagetoolkit;

{$warn 5023 off : no warning about unused units}
interface

uses
  DIT.Image.Base, DIT.Platform, DIT.Image.Atari8.ATR, DIT.FileSystem.Base, 
  DIT.FileSystem.Atari8.DOS12, DIT.FileSystem.Atari8.DOS3, 
  DIT.FileSystem.Base.FAT, DIT.FileSystem.Atari8.Utils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('diskimagetoolkit', @Register);
end.
