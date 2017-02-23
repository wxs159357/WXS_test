unit uUserOption;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.TabControl, uFramUserList, uFramGroupList;

type
  TfUserOption = class(TForm)
    TabUserOption: TTabControl;
    TabItem1: TTabItem;
    pnlUserlOption: TPanel;
    TabItem2: TTabItem;
    pnlGroupOption: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure TabUserOptionChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    AUserFram  : TfamUserList;
    AGroupFram : TfamGroupList;
    procedure ShowInfo(nIndex : Integer);
  public
    { Public declarations }
  end;

var
  fUserOption: TfUserOption;

implementation

{$R *.fmx}

procedure TfUserOption.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  AUserFram.Free;
  AGroupFram.Free;
end;

procedure TfUserOption.FormCreate(Sender: TObject);
begin
  AUserFram := TfamUserList.Create( Self );
  AUserFram.Parent := pnlUserlOption;

  AGroupFram := TfamGroupList.Create( Self );
  AGroupFram.Parent := pnlGroupOption;
end;

procedure TfUserOption.FormShow(Sender: TObject);
begin
  TabUserOption.TabIndex := 0;
  ShowInfo(TabUserOption.TabIndex);
end;

procedure TfUserOption.ShowInfo(nIndex: Integer);
begin
  case nIndex of
    0 :
      begin
        if Assigned(AUserFram) then
        begin
          AUserFram.LoadData;
        end;
      end;
    1 :
      begin
        AGroupFram.LoadGroupData;
      end;
  end;
end;

procedure TfUserOption.TabUserOptionChange(Sender: TObject);
begin
  ShowInfo(TabUserOption.TabIndex);
end;

end.
