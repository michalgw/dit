unit uFramePage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type

  { TFramePage }

  TFramePage = class(TFrame)
  private

  public
    function GetTitle: String; virtual;
    procedure Init(AParams: TObject); virtual;
    procedure Done; virtual;
    procedure Activate; virtual;
    function CanClose: Boolean; virtual;
    function GetTabHint: String; virtual;
  end;

  TFramePageClass = class of TFramePage;

implementation

{$R *.lfm}

{ TFramePage }

function TFramePage.GetTitle: String;
begin
  Result := '(TFramePage)';
end;

procedure TFramePage.Init(AParams: TObject);
begin

end;

procedure TFramePage.Done;
begin

end;

procedure TFramePage.Activate;
begin

end;

function TFramePage.CanClose: Boolean;
begin
  Result := True;
end;

function TFramePage.GetTabHint: String;
begin
  Result := '';
end;

end.

