unit uFramePageFileEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uFramePage,
  uFramePageImage, DIT.FileSystem.Base;

type
  TImagePageLink = class
    ImagePage: TFramePageImage;
    Stream: TFSFileStream;
  end;

  { TFramePageFileEdit }

  TFramePageFileEdit = class(TFramePage)
  protected
    FImagePage: TFramePageImage;
    FStream: TFSFileStream;
  public
    procedure Init(AParams: TObject); override;
    function GetTitle: String; override;
    function GetTabHint: String; override;
    procedure Done; override;
  end;

implementation

{$R *.lfm}

{ TFramePageFileEdit }

procedure TFramePageFileEdit.Init(AParams: TObject);
begin
  inherited Init(AParams);
  if Assigned(AParams) and (AParams is TImagePageLink) then
    with AParams as TImagePageLink do
    begin
      FImagePage := ImagePage;
      FStream := Stream;
    end;
end;

function TFramePageFileEdit.GetTitle: String;
begin
  Result := FImagePage.GetTitle + DirectorySeparator + ExtractFileName(FStream.FileName);
end;

function TFramePageFileEdit.GetTabHint: String;
begin
  Result := FImagePage.GetTitle + DirectorySeparator + FStream.FileName;
end;

procedure TFramePageFileEdit.Done;
begin
  FreeAndNil(FStream);
  inherited Done;
end;

end.

