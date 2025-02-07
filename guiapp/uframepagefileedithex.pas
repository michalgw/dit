unit uFramePageFileEditHex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  uFramePageFileEdit, MPHexEditorEx;

type

  { TFramePageFileEditHex }

  TFramePageFileEditHex = class(TFramePageFileEdit)
    MPHexEditorEx1: TMPHexEditorEx;
    ToolBar1: TToolBar;
  private

  public
    procedure Init(AParams: TObject); override;
  end;

implementation

{$R *.lfm}

{ TFramePageFileEditHex }

procedure TFramePageFileEditHex.Init(AParams: TObject);
begin
  inherited Init(AParams);
  MPHexEditorEx1.LoadFromStream(FStream);
end;

end.

