unit uFormCreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  EditBtn, ButtonPanel, Buttons, ExtCtrls, DIT.FileSystem.Base, DIT.Image.Base;

type

  { TFormCreate }

  TFormCreate = class(TForm)
    BitBtnImgOptions: TBitBtn;
    BitBtnFSOptions: TBitBtn;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxOpen: TCheckBox;
    ComboBoxFS: TComboBox;
    ComboBoxFormat: TComboBox;
    ComboBoxImg: TComboBox;
    ComboBoxSectorSize: TComboBox;
    FileNameEditImg: TFileNameEdit;
    GroupBoxImg: TGroupBox;
    GroupBoxFormat: TGroupBox;
    GroupBoxGeometry: TGroupBox;
    LabelImgFmt: TLabel;
    LabelImgFile: TLabel;
    LabelFS: TLabel;
    LabelFormat: TLabel;
    LabelSectCnt: TLabel;
    LabelSectSize: TLabel;
    PanelFS: TPanel;
    PanelImg: TPanel;
    SpinEditSectors: TSpinEdit;
    procedure BitBtnFSOptionsClick(Sender: TObject);
    procedure BitBtnImgOptionsClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure ComboBoxFormatChange(Sender: TObject);
    procedure ComboBoxFSChange(Sender: TObject);
    procedure ComboBoxImgChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    ImgFormatOptions: TDiskImageFormatOptions;
    FSFormatOptions: TFileSystemOptions;
  end;

var
  FormCreate: TFormCreate;

implementation

{$R *.lfm}

uses
  uFormOptions;

const
  SECT_SIZES: array[0..2] of TSectorSize = (ss128, ss256, ss512);
  DISK_FMT: array[1..3] of record
    SectLen: Integer;
    SectSize: Integer;
  end = (
    (SectLen: 720; SectSize: 0),
    (SectLen: 1040; SectSize: 0),
    (SectLen: 720; SectSize: 1));

{ TFormCreate }

procedure TFormCreate.FormShow(Sender: TObject);
var
  ImgFormat: TDiskImageClass;
  FileSys: TFileSystemClass;
begin
  ImgFormatOptions := nil;
  FSFormatOptions := nil;
  for ImgFormat in KnownDiskImageFormats do
    ComboBoxImg.AddItem(ImgFormat.FormatInfo.Name, TObject(ImgFormat));
  for FileSys in KnownFileSystems do
    ComboBoxFS.AddItem(FileSys.FileSystemName, TObject(FileSys));
end;

procedure TFormCreate.BitBtnOkClick(Sender: TObject);
var
  Img: TStreamDiskImage = nil;
  FS: TFileStream = nil;
  Created: Boolean = False;
begin
  if (ComboBoxFormat.ItemIndex < 0) or (ComboBoxFS.ItemIndex < 0) or
    (ComboBoxSectorSize.ItemIndex < 0) or (SpinEditSectors.Value < 1) or
    (ComboBoxImg.ItemIndex < 0) then
  begin
    MessageDlg('Fill all fields.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if FileExists(FileNameEditImg.FileName) and
    (MessageDlg('File exists. Overwrite?', mtConfirmation, mbYesNo, 0) <> mrYes) then
    Exit;
  try
    FS := TFileStream.Create(FileNameEditImg.FileName, fmCreate);
    Img := TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).Create(FS);
    Img.InitImage(SECT_SIZES[ComboBoxSectorSize.ItemIndex], SpinEditSectors.Value, ImgFormatOptions);
    if ComboBoxFS.ItemIndex > 0 then
      TFileSystemClass(ComboBoxFS.Items.Objects[ComboBoxFS.ItemIndex]).Format(Img);
    Created := True;
  finally
    if Assigned(Img) then
      Img.Free;
    if Assigned(FS) then
      FS.Free;
  end;
  if Created then
    ModalResult := mrOK;
end;

procedure TFormCreate.BitBtnImgOptionsClick(Sender: TObject);
begin
  TFormOptions.EditOptions(ImgFormatOptions, 'Image format options');
end;

procedure TFormCreate.BitBtnFSOptionsClick(Sender: TObject);
begin
  TFormOptions.EditOptions(FSFormatOptions, 'File system options');
end;

procedure TFormCreate.ComboBoxFormatChange(Sender: TObject);
begin
  if ComboBoxFormat.ItemIndex > 0 then
  begin
    SpinEditSectors.Value := DISK_FMT[ComboBoxFormat.ItemIndex].SectLen;
    ComboBoxSectorSize.ItemIndex := DISK_FMT[ComboBoxFormat.ItemIndex].SectSize;
    SpinEditSectors.Enabled := False;
    ComboBoxSectorSize.Enabled := False;
  end
  else
  begin
    SpinEditSectors.Enabled := True;
    ComboBoxSectorSize.Enabled := True;
  end;
end;

procedure TFormCreate.ComboBoxFSChange(Sender: TObject);
begin
  CheckBoxOpen.Checked := ComboBoxFS.ItemIndex > 0;
  CheckBoxOpen.Enabled := ComboBoxFS.ItemIndex > 0;
  if Assigned(FSFormatOptions) then
    FreeAndNil(FSFormatOptions);
  if (ComboBoxFS.ItemIndex > 0) and (TFileSystemClass(ComboBoxFS.Items.Objects[ComboBoxFS.ItemIndex]).FileSystemOptionsClass <> nil) then
  begin
    FSFormatOptions := TFileSystemClass(ComboBoxFS.Items.Objects[ComboBoxFS.ItemIndex]).FileSystemOptionsClass.Create;
    BitBtnFSOptions.Enabled := True;
  end
  else
    BitBtnFSOptions.Enabled := False;
end;

procedure TFormCreate.ComboBoxImgChange(Sender: TObject);
var
  C: TDiskImageFormatOptionsClass;
begin
  FileNameEditImg.FileName := ChangeFileExt(FileNameEditImg.FileName,
    TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).FormatInfo.FileExt[0]);
  FileNameEditImg.DefaultExt := TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).FormatInfo.FileExt[0];
  FileNameEditImg.Filter :=
    TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).FormatInfo.Name + '|*' +
    TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).FormatInfo.FileExt[0] +
    '|All files|*.*';

  if Assigned(ImgFormatOptions) then
    FreeAndNil(ImgFormatOptions);
  C := TDiskImageClass(ComboBoxImg.Items.Objects[ComboBoxImg.ItemIndex]).FormatOptionsClass;
  if C <> nil then
  begin
    ImgFormatOptions := C.Create;
    BitBtnImgOptions.Enabled := True;
  end
  else
    BitBtnImgOptions.Enabled := False;
end;

procedure TFormCreate.FormDestroy(Sender: TObject);
begin
  if Assigned(ImgFormatOptions) then
    FreeAndNil(ImgFormatOptions);
  if Assigned(FSFormatOptions) then
    FreeAndNil(FSFormatOptions);
end;

end.

