unit FrmOptionBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfOptionBase = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadOption;
    procedure SaveOption;
  end;

var
  fOptionBase: TfOptionBase;

implementation

{$R *.dfm}

{ TfOptionBase }

procedure TfOptionBase.LoadOption;
begin

end;

procedure TfOptionBase.SaveOption;
begin

end;

end.
