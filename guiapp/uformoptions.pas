unit uFormOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, RTTIGrids;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    TIPropertyGrid1: TTIPropertyGrid;
  private

  public
    class function EditOptions(AData: TPersistent; ATitle: String): Boolean;
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

{ TFormOptions }

class function TFormOptions.EditOptions(AData: TPersistent; ATitle: String): Boolean;
var
  F: TFormOptions;
  TmpO: TPersistent;
begin
  F := TFormOptions.Create(Application);
  F.Caption := ATitle;
  TmpO := AData.ClassType.Create as TPersistent;
  TmpO.Assign(AData);
  F.TIPropertyGrid1.TIObject := TmpO;
  if F.ShowModal = mrOK then
  begin
    AData.Assign(TmpO);
    Result := True;
  end
  else
    Result := False;
  TmpO.Free;
  F.Free;
end;

end.

