unit uSortInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.Controls.Presentation, xSortInfo;

type
  TfSortInfo = class(TForm)
    lbl1: TLabel;
    edtSortName: TEdit;
    lbl2: TLabel;
    mmoSortRemark: TMemo;
    pnl3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
    FSortInfo : TSortInfo;
  public
    { Public declarations }
    /// <summary>
    /// 显示信息
    /// </summary>
    procedure ShowInfo(ASortInfo : TSortInfo);

    /// <summary>
    /// 保存信息
    /// </summary>
    procedure SaveInfo;
  end;

var
  fSortInfo: TfSortInfo;

implementation

{$R *.fmx}

{ TfSortInfo }

procedure TfSortInfo.SaveInfo;
begin
  if Assigned(FSortInfo) then
  begin
    FSortInfo.SortName := edtSortName.Text;
    FSortInfo.SortRemark := mmoSortRemark.Text;
  end;
end;

procedure TfSortInfo.ShowInfo(ASortInfo: TSortInfo);
begin
  if Assigned(ASortInfo) then
  begin
    FSortInfo := ASortInfo;

    edtSortName.Text := FSortInfo.SortName;
    mmoSortRemark.Text := FSortInfo.SortRemark;
  end;
end;

end.
