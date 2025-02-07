unit uFramePageImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, Menus, ValEdit, uFramePage, DIT.FileSystem.Base,
  uFSTypes;

type

  { TFramePageImage }

  TFramePageImage = class(TFramePage)
    ActionMkDir: TAction;
    ActionUpDir: TAction;
    ActionShowInfo: TAction;
    ActionShowTree: TAction;
    ActionViewSmallIcon: TAction;
    ActionViewReport: TAction;
    ActionViewList: TAction;
    ActionViewIcon: TAction;
    ActionDelete: TAction;
    ActionCopyTo: TAction;
    ActionCopyFrom: TAction;
    ActionListImg: TActionList;
    ComboBoxPath: TComboBox;
    ListViewFiles: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PanelTop: TPanel;
    PanelRight: TPanel;
    PopupMenuList: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SplitterL: TSplitter;
    SplitterR: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TreeViewDir: TTreeView;
    ValEditInfo: TValueListEditor;
    procedure ActionCopyFromExecute(Sender: TObject);
    procedure ActionCopyToExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionMkDirExecute(Sender: TObject);
    procedure ActionShowInfoExecute(Sender: TObject);
    procedure ActionShowTreeExecute(Sender: TObject);
    procedure ActionUpDirExecute(Sender: TObject);
    procedure ActionViewIconExecute(Sender: TObject);
    procedure ComboBoxPathChange(Sender: TObject);
    procedure ListViewFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ListViewFilesResize(Sender: TObject);
    procedure ListViewFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FFileSystem: TImageAndFileSystem;
    CurFiles: TFileList;
    //ValIdxCapacity: Integer;
    ValIdxFreeSpace: Integer;
    ValIdxUsedSpace: Integer;
    procedure SetFileSystem(AValue: TImageAndFileSystem);
    procedure UpdateStatusBar;
    procedure FillInfo;
    procedure UpdateInfo;
  public
    List_Count: Integer;
    List_Size: Integer;
    List_Sel: Integer;
    List_SelSize: Integer;
    procedure OpenDir(ADir: String);
    procedure RefreshDir;
    procedure Init(AParams: TObject); override;
    procedure Done; override;
    procedure Activate; override;

    procedure CopyFrom;
    procedure CopyFileFrom(ASourceFile: String; ADestFile: String);
    procedure CopyTo;
    procedure CopyFileTo(ASourceFile: String; ADestFile: String);
    procedure Delete;
    procedure DropFiles(const AFileNames: array of String);

    function GetTitle: String; override;
    function GetTabHint: String; override;
    property FileSystem: TImageAndFileSystem read FFileSystem write SetFileSystem;
  end;

implementation

uses uFormMain, Math, LazFileUtils;

{$R *.lfm}

{ TFramePageImage }

procedure TFramePageImage.ActionCopyFromExecute(Sender: TObject);
begin
  CopyFrom;
end;

procedure TFramePageImage.ActionCopyToExecute(Sender: TObject);
begin
  CopyTo;
end;

procedure TFramePageImage.ActionDeleteExecute(Sender: TObject);
begin
  Delete;
end;

procedure TFramePageImage.ActionMkDirExecute(Sender: TObject);
var
  S: String;
begin
  if FileSystem.FileSystem.HasDirs then
  begin
    S := InputBox('New directory', 'Enter directory name', 'NEWDIR');
    if S <> '' then
    begin
      S := FileSystem.FileSystem.NormalizeFileName(S);
      try
        FileSystem.FileSystem.MakeDir(IncludeTrailingPathDelimiter(ComboBoxPath.Text) + S);
        RefreshDir;
      except
        on E: Exception do
          MessageDlg('Create directory', 'Can not create directory ' + S + LineEnding + 'Reason: ' + E.Message,
            mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TFramePageImage.ActionShowInfoExecute(Sender: TObject);
begin
  PanelRight.Visible := ActionShowInfo.Checked;
  SplitterR.Visible := ActionShowInfo.Checked;
  SplitterR.Left := PanelRight.Left - SplitterR.Width;
end;

procedure TFramePageImage.ActionShowTreeExecute(Sender: TObject);
begin
  TreeViewDir.Visible := ActionShowTree.Checked;
  SplitterL.Visible := ActionShowTree.Checked;
  SplitterL.Left := TreeViewDir.Width;
end;

procedure TFramePageImage.ActionUpDirExecute(Sender: TObject);
var
  S: String;
begin
  S := ExcludeTrailingPathDelimiter(ComboBoxPath.Text);
  S := ExcludeTrailingPathDelimiter(ExtractFilePath(S));
  if Trim(S) = '' then
    S := DirectorySeparator;
  OpenDir(S);
end;

procedure TFramePageImage.ActionViewIconExecute(Sender: TObject);
begin
  if ActionViewIcon.Checked then
    ListViewFiles.ViewStyle := vsIcon
  else if ActionViewList.Checked then
    ListViewFiles.ViewStyle := vsList
  else if ActionViewReport.Checked then
    ListViewFiles.ViewStyle := vsReport
  else if ActionViewSmallIcon.Checked then
    ListViewFiles.ViewStyle := vsSmallIcon;
end;

procedure TFramePageImage.ComboBoxPathChange(Sender: TObject);
begin
  OpenDir(ComboBoxPath.Text);
end;

procedure TFramePageImage.ListViewFilesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if TFileListItem(Item1.Data^).IsDir = TFileListItem(Item2.Data^).IsDir then
  begin
    case ListViewFiles.SortColumn of
      1: Compare := CompareValue(TFileListItem(Item1.Data^).Size, TFileListItem(Item2.Data^).Size);
      2: Compare := CompareValue(TFileListItem(Item1.Data^).Time, TFileListItem(Item2.Data^).Time);
    end;
    if (Compare = 0) or (ListViewFiles.SortColumn = 0) then
      Compare := CompareText(Item1.Caption, Item2.Caption);
    if ListViewFiles.SortDirection = sdDescending then
      Compare := Compare * -1;
  end
  else
    if TFileListItem(Item1.Data^).IsDir then
      Compare := -1
    else
      Compare := 1;
end;

procedure TFramePageImage.ListViewFilesDblClick(Sender: TObject);
begin
  if Assigned(ListViewFiles.Selected) and TFileListItem(ListViewFiles.Selected.Data^).IsDir then
    OpenDir(IncludeTrailingPathDelimiter(ComboBoxPath.Text) + ListViewFiles.Selected.Caption);
end;

procedure TFramePageImage.ListViewFilesResize(Sender: TObject);
var
  W, I: Integer;
begin
  W := 0;
  for I := 1 to ListViewFiles.ColumnCount - 1 do
    W := W + ListViewFiles.Column[I].Width;
  W := ListViewFiles.ClientWidth - W;
  if W < 160 then
    W := 160;
  ListViewFiles.Column[0].Width := W;
end;

procedure TFramePageImage.ListViewFilesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    Inc(List_Sel);
    Inc(List_SelSize, TFileListItem(Item.Data^).Size);
  end
  else
  begin
    Dec(List_Sel);
    Dec(List_SelSize, TFileListItem(Item.Data^).Size);
  end;
  UpdateStatusBar;
end;

procedure TFramePageImage.SetFileSystem(AValue: TImageAndFileSystem);
begin
  if FFileSystem = AValue then Exit;
  FFileSystem := AValue;
end;

procedure TFramePageImage.UpdateStatusBar;
begin
  FormMain.StatusBar.Panels[0].Text := FileSystem.Image.FormatInfo.Name;
  FormMain.StatusBar.Panels[1].Text := FileSystem.FileSystem.FileSystemName;
  FormMain.StatusBar.Panels[2].Text := 'SC: ' + IntToStr(FileSystem.Image.SectorCount)
    + '  SS: ' + IntToStr(Ord(FileSystem.Image.SectorSize));
  FormMain.StatusBar.Panels[3].Text := 'AS: ' + IntToStr(FileSystem.FileSystem.Capacity) + '  FS: ' + IntToStr(FileSystem.FileSystem.FreeSpace);
  FormMain.StatusBar.Panels[5].Text := IntToStr(List_Count) + ' file(s) / ' + IntToStr(List_Size) + 'B';
  if List_Sel > 0 then
    FormMain.StatusBar.Panels[5].Text := FormMain.StatusBar.Panels[5].Text + ' / sel.: '
      + IntToStr(List_Sel) + ' / ' + IntToStr(List_SelSize) + 'B';
  if FileSystem.FileSystem.Capacity - FileSystem.FileSystem.FreeSpace = 0 then
    FormMain.StatusProggres := 0
  else
    FormMain.StatusProggres := (FileSystem.FileSystem.Capacity - FileSystem.FileSystem.FreeSpace) / FileSystem.FileSystem.Capacity;
  FormMain.StatusBar.Invalidate;
end;

procedure TFramePageImage.FillInfo;
begin
  ValEditInfo.InsertRow('---Image---', '--------------------------', True);
  ValEditInfo.InsertRow('Image format', FileSystem.Image.FormatInfo.Name, True);
  ValEditInfo.InsertRow('Sector size', IntToStr(Ord(FileSystem.Image.SectorSize)), True);
  ValEditInfo.InsertRow('Sector count', IntToStr(FileSystem.Image.SectorCount), True);
  ValEditInfo.InsertRow('Image size', IntToStr(Ord(FileSystem.Image.SectorSize) * FileSystem.Image.SectorCount), True);
  ValEditInfo.InsertRow('---File system---', '--------------------------', True);
  ValEditInfo.InsertRow('File system', FileSystem.FileSystem.FileSystemName, True);
  ValEditInfo.InsertRow('Capacity', IntToStr(FileSystem.FileSystem.Capacity), True);
  ValIdxFreeSpace := ValEditInfo.InsertRow('Free space', IntToStr(FileSystem.FileSystem.FreeSpace), True);
  ValIdxUsedSpace := ValEditInfo.InsertRow('Used space', IntToStr(FileSystem.FileSystem.Capacity - FileSystem.FileSystem.FreeSpace), True);
end;

procedure TFramePageImage.UpdateInfo;
begin
  //ValEditInfo.Strings.Strings[ValIdxFreeSpace] := IntToStr(FileSystem.FileSystem.FreeSpace);
  ValEditInfo.Values['Free space'] := IntToStr(FileSystem.FileSystem.FreeSpace);
  ValEditInfo.Values['Used space'] := IntToStr(FileSystem.FileSystem.Capacity - FileSystem.FileSystem.FreeSpace);
end;

procedure TFramePageImage.OpenDir(ADir: String);
begin
  if not FileSystem.FileSystem.DirExists(ADir) then
    Exit;
  if ComboBoxPath.Items.IndexOf(ComboBoxPath.Text) < 0 then
    ComboBoxPath.Items.Add(ComboBoxPath.Text);
  ComboBoxPath.Text := ADir;
  if ComboBoxPath.Items.IndexOf(ComboBoxPath.Text) < 0 then
    ComboBoxPath.Items.Add(ComboBoxPath.Text);
  RefreshDir;
end;

procedure TFramePageImage.RefreshDir;
var
  FI: TFileListItem;
  I: Integer;
  LI: TListItem;
  S: String;
begin
  List_Count := 0;
  List_Size := 0;
  List_Sel := 0;
  List_SelSize := 0;
  try
    CurFiles := FileSystem.FileSystem.List(ComboBoxPath.Text);
  except
  end;
  ListViewFiles.BeginUpdate;
  ListViewFiles.Clear;
  for I := 0 to Length(CurFiles) - 1 do
  begin
    LI := ListViewFiles.Items.Add;
    FI := CurFiles[I];
    LI.Caption := ExtractFileName(FI.Name);
    LI.SubItems.Add(IntToStr(FI.Size));
    if FI.Time = 0 then
      S := '------'
    else
      S := DateTimeToStr(FI.Time);
    LI.SubItems.Add(S);
    LI.SubItems.Add(AttribsToStr(FI.Attributes));
    LI.Data := @CurFiles[I];
    if faDirectory in FI.Attributes then
      LI.ImageIndex := 1
    else
      LI.ImageIndex := 0;
    Inc(List_Count);
    Inc(List_Size, FI.Size);
  end;
  if ListViewFiles.SortColumn < 0 then
    ListViewFiles.SortColumn := 0;
  ListViewFiles.EndUpdate;
  ListViewFiles.Sort;
  UpdateStatusBar;
  UpdateInfo;
end;

procedure TFramePageImage.Init(AParams: TObject);
begin
  inherited Init(AParams);
  FileSystem := TImageAndFileSystem(AParams);
  FillInfo;
  OpenDir(DirectorySeparator);
  ListViewFilesResize(nil);
  if not FileSystem.FileSystem.HasDirs then
  begin
    ActionShowTree.Checked := False;
    ActionShowTreeExecute(nil);
  end;
end;

procedure TFramePageImage.Done;
begin
  if FileSystem <> nil then
    FileSystem.Free;
  inherited Done;
end;

procedure TFramePageImage.Activate;
begin
  UpdateStatusBar;
end;

procedure TFramePageImage.CopyFrom;
var
  I: Integer;
begin
  if ListViewFiles.SelCount = 0 then
    Exit;
  if ListViewFiles.SelCount = 1 then
  begin
    FormMain.SaveDialogFrom.FileName := ExtractFileName(ListViewFiles.Selected.Caption);
    if FormMain.SaveDialogFrom.Execute then
      CopyFileFrom(IncludeTrailingPathDelimiter(ComboBoxPath.Text) + ListViewFiles.Selected.Caption, FormMain.SaveDialogFrom.FileName);
  end
  else
  begin
    if FormMain.SelectDirectoryDialogTo.Execute then
      for I := 0 to ListViewFiles.Items.Count - 1 do
        if ListViewFiles.Items[I].Selected then
          CopyFileFrom(IncludeTrailingPathDelimiter(ComboBoxPath.Text) + ListViewFiles.Items[I].Caption,
            IncludeTrailingPathDelimiter(FormMain.SelectDirectoryDialogTo.FileName) +
            ExtractFileName(ListViewFiles.Items[I].Caption));
  end;
end;

procedure TFramePageImage.CopyFileFrom(ASourceFile: String; ADestFile: String);
var
  InFile: TFSFileStream;
  OutFile: TFileStream;
  Readed: Integer;
  Buf: array[0..$FFFF] of Byte;
begin
  if FileSystem.FileSystem.FileExists(ASourceFile) then
    InFile := FileSystem.FileSystem.CreateFileStram(ASourceFile, fmOpenRead)
  else
  begin
    MessageDlg('File not found: ' + ASourceFile, mtError, [mbOK], 0);
    Exit;
  end;
  OutFile := TFileStream.Create(ADestFile, fmCreate);
  while InFile.Position < InFile.Size do
  begin
    Readed := InFile.Read(Buf, SizeOf(Buf));
    if Readed > 0 then
      OutFile.Write(Buf, Readed);
  end;
  OutFile.Free;
  InFile.Free;
end;

procedure TFramePageImage.CopyTo;
var
  I: Integer;
begin
  if FormMain.OpenDialogTo.Execute then
  begin
    for I := 0 to FormMain.OpenDialogTo.Files.Count - 1 do
      CopyFileTo(FormMain.OpenDialogTo.Files[I],
        IncludeTrailingPathDelimiter(ComboBoxPath.Text) + FileSystem.FileSystem.NormalizeFileName(ExtractFileName(FormMain.OpenDialogTo.Files[I])));
    RefreshDir;
  end;
end;

procedure TFramePageImage.CopyFileTo(ASourceFile: String; ADestFile: String);
var
  InFile: TFileStream = nil;
  OutFile: TFSFileStream = nil;
  Readed, Writed: Integer;
  Buf: array[0..$FFFF] of Byte;
begin
  if not FileExists(ASourceFile) then
  begin
    MessageDlg('File not found: ' + ASourceFile, mtError, [mbOK], 0);
    Exit;
  end;
  if FileSystem.FileSystem.FileExists(ADestFile) and (MessageDlg('File ' + ADestFile + ' exists.' + LineEnding +
    'Overwrite?', mtConfirmation, mbYesNo, 0) <> mrYes) then
    Exit;
  InFile := TFileStream.Create(ASourceFile, fmOpenRead + fmShareDenyWrite);
  if InFile.Size > FileSystem.FileSystem.FreeSpace then
  begin
    InFile.Free;
    MessageDlg('Not enought space for file: ' + ASourceFile, mtError, [mbOK], 0);
    Exit;
  end;
  OutFile := FileSystem.FileSystem.CreateFileStram(ADestFile, fmCreate);
  while InFile.Position < InFile.Size do
  begin
    Readed := InFile.Read(Buf, SizeOf(Buf));
    if Readed > 0 then
    begin
      Writed := OutFile.Write(Buf, Readed);
      if Readed <> Writed then
      begin
        MessageDlg('Write error: ' + ADestFile, mtError, [mbOK], 0);
        Break;
      end;
    end;
  end;
  InFile.Free;
  OutFile.Free;
end;

procedure TFramePageImage.Delete;
var
  Msg: String;
  I: Integer;
begin
  if ListViewFiles.SelCount = 0 then
    Exit;
  if ListViewFiles.SelCount = 1 then
    Msg := 'Delete file "' + ListViewFiles.Selected.Caption + '"?'
  else
    Msg := 'Delete ' + IntToStr(ListViewFiles.SelCount) + ' files?';
  if MessageDlg(Msg, mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    for I := 0 to ListViewFiles.Items.Count - 1 do
      if ListViewFiles.Items[I].Selected then
      begin
        try
          FileSystem.FileSystem.Delete(IncludeTrailingPathDelimiter(ComboBoxPath.Text) + ListViewFiles.Items[I].Caption);
        except
          on E: Exception do
            if MessageDlg('Can not delete file "' + ListViewFiles.Items[I].Caption + '"' + LineEnding +
              'Reason: ' + E.Message, mtError, [mbOK, mbAbort], 0) = mrAbort then
              Break;
        end;
      end;
    RefreshDir;
  end;
end;

procedure TFramePageImage.DropFiles(const AFileNames: array of String);
var
  FN, TgFN: String;
begin
  for FN in AFileNames do
  begin
    if DirectoryExists(FN) then
    begin

    end
    else
    begin
      TgFN := FileSystem.FileSystem.NormalizeFileName(ExtractFileName(FN));
      CopyFileTo(FN, TgFN);
    end;
  end;
  RefreshDir;
end;

function TFramePageImage.GetTitle: String;
begin
  Result := ExtractFileName(FileSystem.FileName);
end;

function TFramePageImage.GetTabHint: String;
begin
  Result := FileSystem.FileName;
end;

end.

