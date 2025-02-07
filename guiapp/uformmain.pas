unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, XMLPropStorage, ExtendedNotebook, uFramePage;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionClose: TAction;
    ActionCreate: TAction;
    ActionOpen: TAction;
    ExtBookMain: TExtendedNotebook;
    ImageListImage: TImageList;
    ImageListFileListB: TImageList;
    ImageListFileListS: TImageList;
    ImageListBig: TImageList;
    ImageListSmall: TImageList;
    MainActionList: TActionList;
    MainMenu: TMainMenu;
    OpenDialogTo: TOpenDialog;
    OpenDialogImg: TOpenDialog;
    SaveDialogFrom: TSaveDialog;
    SelectDirectoryDialogTo: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    XMLPropStorage: TXMLPropStorage;
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionCreateExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ExtBookMainChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure MainActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    function GetActivePage: TFramePage;
    function GetPage(ATab: TTabSheet): TFramePage;
    procedure ExtBookShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ClearStatusBar;
  public
    StatusProggres: Double;
    function CreatePage(APageClass: TFramePageClass; AParams: TObject = nil): TFramePage;
    function ClosePage(ATabSheet: TTabSheet): Boolean;
    procedure OpenImage(AFileName: String);
    property ActivePage: TFramePage read GetActivePage;
    property Page[ATab: TTabSheet]: TFramePage read GetPage;
  end;

var
  FormMain: TFormMain;

implementation

uses uFramePageStart, uFormCreate, uFSTypes, uFramePageImage, DIT.Image.Base,
  DIT.FileSystem.Base, DIT.Image.Atari8.ATR, DIT.FileSystem.Atari8.DOS12,
  DIT.FileSystem.Atari8.DOS3;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ActionCreateExecute(Sender: TObject);
begin
  FormCreate := TFormCreate.Create(Application);
  if FormCreate.ShowModal = mrOK then
  begin
    if FormCreate.CheckBoxOpen.Checked then
      OpenImage(FormCreate.FileNameEditImg.FileName);
  end;
  FormCreate.Free;
end;

procedure TFormMain.ActionCloseExecute(Sender: TObject);
begin
  if ExtBookMain.ActivePage <> nil then
    ClosePage(ExtBookMain.ActivePage);
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialogImg.Execute then
    OpenImage(OpenDialogImg.FileName);
end;

procedure TFormMain.ExtBookMainChange(Sender: TObject);
begin
  AllowDropFiles := (ActivePage <> nil) and (ActivePage is TFramePageImage);
  if ActivePage <> nil then
    ActivePage.Activate;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  while ExtBookMain.PageCount > 0 do
  begin
    if not ClosePage(ExtBookMain.Pages[0]) then
    begin
      CloseAction := caNone;
      Exit;
    end;
  end;
end;

procedure TFormMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  if (ActivePage <> nil) and (ActivePage is TFramePageImage) then
    TFramePageImage(ActivePage).DropFiles(FileNames);
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  IC: TDiskImageClass;
  SF, S: String;
begin
  for IC in KnownDiskImageFormats do
  begin
    SF := '';
    for S in IC.FormatInfo.FileExt do
      SF := SF + ';*' + S;
    if SF[1] = ';' then
      Delete(SF, 1, 1);
    OpenDialogImg.Filter := OpenDialogImg.Filter + '|' + IC.FormatInfo.Name + '|' + SF;
  end;
  ExtBookMain.OnShowHint := @ExtBookShowHint;
  ExtBookMain.ShowHint := True;
end;

procedure TFormMain.MainActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
  ActionClose.Enabled := ExtBookMain.ActivePage <> nil;
end;

procedure TFormMain.StatusBarDrawPanel(AStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if (ActivePage <> nil) and (ActivePage is TFramePageImage) then
  begin
    Rect.Inflate(-2, -2);
    StatusBar.Canvas.Pen.Color := clHighlight;
    StatusBar.Canvas.Pen.Style := psSolid;
    StatusBar.Canvas.Brush.Style := bsClear;
    StatusBar.Canvas.Rectangle(Rect);
    StatusBar.Canvas.Brush.Color := clHighlight;
    StatusBar.Canvas.Brush.Style := bsSolid;
    StatusBar.Canvas.FillRect(Rect.Left, Rect.Top,
      Round(Rect.Width * StatusProggres) + Rect.Left, Rect.Bottom);
  end;
end;

function TFormMain.GetActivePage: TFramePage;
begin
  Result := Page[ExtBookMain.ActivePage];
end;

function TFormMain.GetPage(ATab: TTabSheet): TFramePage;
begin
  if Assigned(ATab) and (ATab.Tag <> 0) and (TObject(ATab.Tag) is TFramePage) then
    Result := TFramePage(ATab.Tag)
  else
    Result := nil;
end;

procedure TFormMain.ExtBookShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  TabIndex: Integer;
begin
  if ExtBookMain.PageCount = 0 then
    Exit;
  TabIndex := ExtBookMain.IndexOfTabAt(ExtBookMain.ScreenToClient(Mouse.CursorPos));
  if TabIndex < 0 then
    Exit;
  if (ActivePage = nil) then
    Exit;
  HintInfo^.HintStr := ActivePage.GetTabHint;
end;

procedure TFormMain.ClearStatusBar;
var
  P: TCollectionItem;
begin
  for P in StatusBar.Panels do
    if P is TStatusPanel then
      (P as TStatusPanel).Text := '';
  StatusProggres := 0;
  StatusBar.Invalidate;
end;

function TFormMain.CreatePage(APageClass: TFramePageClass; AParams: TObject
  ): TFramePage;
var
  T: TTabSheet;
begin
  T := ExtBookMain.AddTabSheet;
  Result := APageClass.Create(T);
  Result.Init(AParams);
  T.Caption := Result.GetTitle;
  Result.Parent := T;
  Result.Align := alClient;
  T.Tag := PtrInt(Result);
  ExtBookMain.ActivePage := T;
  Result.Activate;
  if Result is TFramePageImage then
    AllowDropFiles := True;
end;

function TFormMain.ClosePage(ATabSheet: TTabSheet): Boolean;
begin
  Result := True;
  if (ATabSheet.Tag <> 0) and (TObject(ATabSheet.Tag) is TFramePage) then
  begin
    Result := TFramePage(ATabSheet.Tag).CanClose;
    if Result then
      TFramePage(ATabSheet.Tag).Done;
  end;
  if Result then
    ATabSheet.Free;
  AllowDropFiles := (ActivePage <> nil) and (ActivePage is TFramePageImage);
  if ActivePage = nil then
    ClearStatusBar;
end;

procedure TFormMain.OpenImage(AFileName: String);
var
  FSI: TImageAndFileSystem;
  Created: Boolean = False;
begin
  try
    FSI := TImageAndFileSystem.Create(AFileName, fmOpenReadWrite + fmShareExclusive);
    Created := True;
  except
    on E: Exception do
      MessageDlg('Exception: ' + E.Message, mtError, [mbOK], 0);
  end;
  if Created then
    CreatePage(TFramePageImage, FSI);
end;

end.

