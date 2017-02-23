unit FrmOption;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CategoryButtons, ExtCtrls, StdCtrls, FrmOptionBase;

type
  TfOption = class(TForm)
    pnlCatagory: TPanel;
    btnCateGoryList: TCategoryButtons;
    pnl1: TPanel;
    pnlOption: TPanel;
    pnl2: TPanel;
    btnOK: TButton;
    btnCancle: TButton;
    pnl3: TPanel;
    procedure btnCateGoryListButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancleClick(Sender: TObject);
  private
    { Private declarations }

//    procedure AddCategoryItem( btnCategory : TButtonCategory; f : TfOptionBase );
  public
    { Public declarations }
    procedure LoadS21Options;
  end;

var
  fOption: TfOption;

implementation

{$R *.dfm}

{ TfOption }

//procedure TfOption.AddCategoryItem(btnCategory: TButtonCategory; f: TfOptionBase);
//begin
//  f.Parent := pnlOption;
//  f.LoadOption;
//  with btnCategory.Items.Add do
//  begin
//    Caption := f.Caption;
//    Data := f;
//  end;
//end;

procedure TfOption.btnCancleClick(Sender: TObject);
begin
  Close;
end;

procedure TfOption.btnCateGoryListButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  pnl3.Caption := TForm( Button.Data ).Caption;
  TForm( Button.Data ).Show;
end;

procedure TfOption.btnOKClick(Sender: TObject);
begin

  Close;
end;

procedure TfOption.FormShow(Sender: TObject);
begin
  if (btnCateGoryList.Categories.Count > 0) and
    (btnCateGoryList.Categories[0].Items.count > 0) then
    btnCateGoryListButtonClicked( nil,btnCateGoryList.Categories[0].Items[0] );
end;

procedure TfOption.LoadS21Options;
var
  btnCategory : TButtonCategory;
begin
  btnCateGoryList.Categories.Clear;
  btnCategory := btnCateGoryList.Categories.Add;
  btnCategory.Caption := 'œµÕ≥…Ë÷√';
end;

end.
