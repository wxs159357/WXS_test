{===============================================================================
  数据库连接基类  

===============================================================================}
unit xDBConn;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.FMXUI.Wait,
   FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  {$IFDEF MSWINDOWS}
  FireDAC.Phys.MSSQLDef, FireDAC.Phys.MSSQL, FireDAC.Phys.MSAccDef, FireDAC.Phys.MySQLDef,
  FireDAC.Phys.MySQL, FireDAC.Phys.ODBCBase, FireDAC.Phys.MSAcc,
  {$ENDIF}
  FireDAC.Comp.UI, Data.DB, FireDAC.Comp.Client, xVCL_FMX;

type
  TDBConn = class
  private
    FGUIxWaitCursor: TFDGUIxWaitCursor;
  {$IFDEF MSWINDOWS}
    FMSAccessDriverLink: TFDPhysMSAccessDriverLink;
    FMySQLDriverLink: TFDPhysMySQLDriverLink;
    FMSSQLDriverLink: TFDPhysMSSQLDriverLink;
  {$ENDIF}

    FSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FConnection: TFDConnection;
    function GetConnStr: string;
    procedure SetConnStr(const Value: string);
  protected

  public
    constructor Create;
    destructor Destroy; override;

    property Connection: TFDConnection read FConnection;

    /// <summary>
    /// 连接字符串
    /// SQLite   :'DriverID=SQLite; Database=C:\Temp\FDDemo.sdb'
    /// MSAccess :'DriverID=MSAcc;Database=C:\Temp\dbdemos.mdb'
    /// MSSQL    :'DriverID=MSSQL;Database=2008ES;Password=ckm2008byx;Server=127.0.0.1;User_Name=sa'
    /// MySQL    :''
    /// </summary>
    property ConnStr : string read GetConnStr write SetConnStr;
  end;
var
  ADBConn : TDBConn;

implementation

{ TDBConn }

constructor TDBConn.Create;
begin
  FGUIxWaitCursor    := TFDGUIxWaitCursor.Create(Application);
{$IFDEF MSWINDOWS}
  FMSAccessDriverLink:= TFDPhysMSAccessDriverLink.Create(Application);
  FMySQLDriverLink   := TFDPhysMySQLDriverLink.Create(Application);
  FMSSQLDriverLink   := TFDPhysMSSQLDriverLink.Create(Application);
{$ENDIF}
  FSQLiteDriverLink  := TFDPhysSQLiteDriverLink.Create(Application);
  FConnection        := TFDConnection.Create(Application);
end;

destructor TDBConn.Destroy;
begin

  inherited;
end;

function TDBConn.GetConnStr: string;
begin
  Result := FConnection.ConnectionString;
end;

procedure TDBConn.SetConnStr(const Value: string);
begin
  FConnection.ConnectionString := Value;
  FConnection.Connected := True;
end;

end.
