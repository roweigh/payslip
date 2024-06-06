unit dmCD_U;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB, DB;

type
  TdatamoduleCD = class(TDataModule)
    conCD: TADOConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
//    conCD: TADOConnection;
    tblCD: TADOTable;
    dscCD: TDataSource;

    qryQuery: TADOQuery;
    dscQuery: TDataSource;

  end;

var
  datamoduleCD: TdatamoduleCD;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdatamoduleCD.DataModuleCreate(Sender: TObject);
begin
//  conCD := TADOConnection.Create( datamoduleCD );
  tblCD := TADOTable.Create( datamoduleCD );
  dscCD := TDataSource.Create( datamoduleCD );

  qryQuery := TADOQuery.Create( datamoduleCD );
  dscQuery := TDataSource.Create( datamoduleCD );

  conCD.ConnectionString := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=testdb;Data Source=DESKTOP-T4TKDF8\SQLEXPRESS01';
  conCD.LoginPrompt := False;
  conCD.Connected := True;
  conCD.Open();

  tblCD.Connection := conCD;
  tblCD.TableName := 'Employees';

  qryQuery.Connection := conCD;
  dscCD.DataSet:= tblCD;

  tblCD.Open;

end;

end.
