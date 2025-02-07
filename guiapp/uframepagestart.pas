unit uFramePageStart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uFramePage;

type

  { TFramePageStart }

  TFramePageStart = class(TFramePage)
  private

  public
    function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

{ TFramePageStart }

function TFramePageStart.GetTitle: String;
begin
  Result := 'Start';
end;

end.

