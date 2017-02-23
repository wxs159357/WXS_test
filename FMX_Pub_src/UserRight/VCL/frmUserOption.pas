unit frmUserOption;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.CategoryButtons;

type
  TfUserOption = class(TForm)
    btnCtlList: TCategoryButtons;
    pnlOptions: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnCtlListButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserOption: TfUserOption;

implementation

uses
  uUserControl, frmUserList, frmUserGroupList;

{$R *.dfm}

procedure TfUserOption.btnCtlListButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  with TForm( Button.Data ) do
  begin
    Show;
  end;
end;

procedure TfUserOption.FormCreate(Sender: TObject);
var
  AForm : TForm;
begin
  with btnCtlList.Categories[ 0 ].Items.Add do
  begin
    AForm := TfUserList.Create( Self );
    AForm.Parent := pnlOptions;
    AForm.BorderStyle := bsNone;
    AForm.Align := alClient;
    Caption := AForm.Caption;
    Data := AForm;
  end;

  with btnCtlList.Categories[ 0 ].Items.Add do
  begin
    AForm := TfUserGroupList.Create( Self );
    AForm.Parent := pnlOptions;
    AForm.BorderStyle := bsNone;
    AForm.Align := alClient;
    Caption := AForm.Caption;
    Data := AForm;
  end;
end;

procedure TfUserOption.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for I := 0 to pnlOptions.ComponentCount - 1 do
    TForm(pnlOptions.Components[i]).Free;
end;

procedure TfUserOption.FormShow(Sender: TObject);
begin
  btnCtlList.SelectedItem := btnCtlList.Categories[0].Items[ 0 ];
  btnCtlListButtonClicked( Self, btnCtlList.Categories[0].Items[ 0 ] );
end;

end.
