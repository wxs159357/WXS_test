unit FrmQuestionListC;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uQuestionList, uQuestionInfo, FrmQInfoC;

type
  TfQuestionListC = class(TfQuestionList)
  private
    { Private declarations }

  protected
    /// <summary>
    /// 创建考题信息界面
    /// </summary>
    function CreateQuestionFrom : TfQuestionInfo; override;

  public
    { Public declarations }
  end;

var
  fQuestionListC: TfQuestionListC;

implementation

{$R *.dfm}

{ TfQuestionListC }

function TfQuestionListC.CreateQuestionFrom: TfQuestionInfo;
begin
  Result := TfQInfo.Create(Application);
end;

end.
