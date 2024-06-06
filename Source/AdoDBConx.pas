unit AdoDBConx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, DateUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Data.Win.ADODB, PaySlipCommon;

type
  TfrmADOCollections = class(TForm)
    ADOConnection: TADOConnection;
    spGenNew_Id: TADOStoredProc;
    ADOGetEmployees: TADOQuery;
    ADOAddEmployee: TADOQuery;
    ADOUpdateEmployee: TADOQuery;
    ADODeleteEmployee: TADOQuery;
    ADOGetLeaves: TADOQuery;
    ADOAddLeave: TADOQuery;
    ADOUpdateLeave: TADOQuery;
    ADOGetLeaveTypes: TADOQuery;
    ADOAddLeaveTypes: TADOQuery;
    ADOUpdateLeaveTypes: TADOQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GenNewRecID(sGenName: string): Integer;
  end;

function CheckDbConnection: Boolean;

procedure GetEmployees(EmpLst : TStringList);
procedure AddEmployee(pEmp : TEmployeeInfo);
procedure UpdateEmployee(pEmp : PEmployeeInfo);
procedure DeleteEmployee(id: Integer);

procedure GetLeaveTypes(LeaveTypeList : TStringList);
procedure AddLeaveType(pLeaveType: PLeaveTypeInfo);
procedure UpdateLeaveType(pLeaveType: PLeaveTypeInfo);

procedure GetLeaves(LeaveList : TStringList);
procedure AddLeave(pLeave: PLeaveInfo);
procedure UpdateLeave(pLeave: PLeaveInfo);

procedure BeginTransaction;
procedure CommitMainTransaction;
procedure RollbackMainTransaction;

Procedure ADOSetAsInteger(Parm : TParameters; ParName : String; AdoVal : integer);
Procedure ADOSetAsDateTime(Parm : TParameters; ParName : String; AdoVal : TDateTime);
Procedure ADOSetAsString(Parm : TParameters; ParName : String; AdoVal : String);
Procedure ADOSetAsFloat(Parm : TParameters; ParName : String; AdoVal : double);
Procedure ADOSetAsWString(Parm : TParameters; ParName : String; AdoVal : String);
Procedure ADOSetAsInt64(Parm : TParameters; ParName : String; AdoVal : int64);
procedure ClearEmpMem(EmpLst : TStringlist);
var frmADOCollections: TfrmADOCollections = Nil;

implementation

{$R *.dfm}

// Initialise
//-------------------------------------------------------------------------------
procedure TfrmADOCollections.FormCreate(Sender: TObject);
begin
  ADOAddEmployee.Parameters.ParseSQL(ADOAddEmployee.SQL.Text, True);
end;


// Connect to DB
function CheckDbConnection: Boolean;
var sErrMsg, ConnStr: String;
  ServerNm, DBName : String;
  Suc : boolean;
begin
  if frmADOCollections = Nil then
    frmADOCollections := TfrmADOCollections.Create(Nil);

    GetDBServerInf(ServerNm, DBName);
    ConnStr := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=' + DBName +
      ';Data Source=' + ServerNm + ';Auto Translate=True;Tag with column collation when possible=False';

  frmADOCollections.ADOConnection.ConnectionString := ConnStr;

  repeat
    sErrMsg := '';
    suc := True;
    try
      frmADOCollections.ADOConnection.Connected := False;
      frmADOCollections.ADOConnection.LoginPrompt := False;
      frmADOCollections.ADOConnection.Connected := True;

    except
      On E: Exception do begin
        sErrMsg := E.Message;
        suc := False;
      end;
    end;

    if not suc then begin
      ShowMessage('Failed to connect to Sql server.');
      Break;
    end;
  until suc;

  result := suc;
end;


// Employees CRUD
//--------------------------------------------------------------------------------
procedure GetEmployees(EmpLst : TStringList);
var pEmp : PEmployeeInfo;
  years : Integer;
begin
  ClearEmpMem(EmpLst);

  with frmADOCollections.ADOGetEmployees do try
    Open;
    while not EOF do begin
      new(pEmp);
      pEmp^.EmployeeID := FieldByName('employeeID').AsInteger;
      pEmp^.EmployeeName := FieldByName('name').AsString;
      pEmp^.IRDNum := FieldByName('irdNo').AsString;
      pEmp^.TaxCode := FieldByName('taxCode').AsString;
      pEmp^.HourRate := FieldByName('hourRate').AsFloat;
      pEmp^.Occupation := FieldByName('occupation').AsString;
      pEmp^.Credit := FieldByName('credit').AsString;
      pEmp^.Hours := FieldByName('hours').AsFloat;
      pEmp^.HasKiwiSaver := (FieldByName('kiwiSaver').AsString = 'T');
      pEmp^.HasStudentLoan := (FieldByName('studentLoan').AsString) = 'T';
      pEmp^.StartDate := FieldByName('startDate').AsDateTime;

      // Check if an employee is elligible for annual leave
      // If accrued leave is not divisible by 20 then total leave information is out of date
      years := YearsBetween(pEmp^.StartDate, now);
      if years > 0 then begin
        if (FieldByName('totalLeave').AsFloat)/years <> 20 then begin
          pEmp^.TotalLeave := years * 20;
          UpdateEmployee(pEmp);
        end;
      end else begin
        pEmp^.TotalLeave := FieldByName('totalLeave').AsFloat;
      end;

      EmpLst.AddObject(pEmp^.EmployeeName, TObject(pEmp));
      Next;
    end;
    finally
    Close;
  end;
end;


procedure AddEmployee(pEmp : TEmployeeInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOAddEmployee.Parameters;
  with frmAdoCollections.ADOAddEmployee do try
    ADOSetAsInteger(Params, 'employeeID', frmADOCollections.GenNewRecID('employeeID'));
    ADOSetAsString(Params, 'name', pEmp.EmployeeName);
    ADOSetAsString(Params, 'irdNo', pEmp.IRDNum);
    ADOSetAsString(Params, 'taxCode', pEmp.TaxCode);
    ADOSetAsFloat(Params, 'hourRate', pEmp.HourRate);
    ADOSetAsString(Params, 'occupation', pEmp.Occupation);
    ADOSetAsString(Params, 'credit', pEmp.Credit);
    ADOSetAsFloat(Params, 'hours', pEmp.Hours);
    ADOSetAsString(Params, 'kiwiSaver', BooleanToTF(pEmp.HasKiwiSaver));
    ADOSetAsString(Params, 'studentLoan', BooleanToTF(pEmp.HasStudentLoan));
    ADOSetAsFloat(Params, 'totalLeave', pEmp.TotalLeave);
    ADOSetAsDateTime(Params, 'startDate', pEmp.StartDate);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Employee Data.');
  end;
end;


procedure UpdateEmployee(pEmp : PEmployeeInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOUpdateEmployee.Parameters;
  with frmAdoCollections.ADOUpdateEmployee do try
    ADOSetAsInteger(Params, 'employeeID', pEmp^.EmployeeId);
    ADOSetAsString(Params, 'name', pEmp^.EmployeeName);
    ADOSetAsString(Params, 'irdNo', pEmp^.IRDNum);
    ADOSetAsString(Params, 'taxCode', pEmp^.TaxCode);
    ADOSetAsFloat(Params, 'hourRate', pEmp^.HourRate);
    ADOSetAsString(Params, 'occupation', pEmp^.Occupation);
    ADOSetAsString(Params, 'credit', pEmp^.Credit);
    ADOSetAsFloat(Params, 'hours', pEmp^.Hours);
    ADOSetAsString(Params, 'kiwiSaver', BooleanToTF(pEmp^.HasKiwiSaver));
    ADOSetAsString(Params, 'studentLoan', BooleanToTF(pEmp^.HasStudentLoan));
    ADOSetAsDateTime(Params, 'startDate', pEmp^.StartDate);
    ADOSetAsFloat(Params, 'totalLeave', pEmp^.TotalLeave);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Employee Data.');
  end;
end;


procedure DeleteEmployee(id: Integer);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADODeleteEmployee.Parameters;
  with frmAdoCollections.ADODeleteEmployee do try
    ADOSetAsInteger(Params, 'employeeID', id);
    
    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Employee Data.');
  end;
end;


// Leave Types
//--------------------------------------------------------------------------------
procedure GetLeaveTypes(LeaveTypeList : TStringList);
var pLT : PLeaveTypeInfo;
begin
  LeaveTypeList.Clear;

  with frmADOCollections.ADOGetLeaveTypes do try
    Open;
    while not EOF do begin
      new(pLT);
      pLT^.id := FieldByName('leaveTypeID').AsInteger;
      pLT^.desc := FieldByName('description').AsString;

      LeaveTypeList.AddObject(pLT^.id.ToString(), TObject(pLT));
      Next;
    end;
    finally
    Close;
  end;
end;

procedure AddLeaveType(pLeaveType: PLeaveTypeInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOAddLeaveTypes.Parameters;
  with frmAdoCollections.ADOAddLeaveTypes do try
    ADOSetAsInteger(Params, 'leaveTypeID', frmADOCollections.GenNewRecID('leaveTypeID'));
    ADOSetAsString(Params, 'description', pLeaveType^.desc);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Leave Data.');
  end;
end;

procedure UpdateLeaveType(pLeaveType: PLeaveTypeInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOUpdateLeaveTypes.Parameters;
  with frmAdoCollections.ADOUpdateLeaveTypes do try
    ADOSetAsInteger(Params, 'leaveTypeID', pLeaveType^.id);
    ADOSetAsString(Params, 'description', pLeaveType^.desc);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Leave Data.');
  end;
end;

// Leaves CRUD
//--------------------------------------------------------------------------------
procedure GetLeaves(LeaveList : TStringList);
var pLeave : PLeaveInfo;
begin
  LeaveList.Clear;
  with frmADOCollections.ADOGetLeaves do try
    Open;
    while not EOF do begin
      new(PLeave);
      pLeave^.LeaveID := FieldByName('leaveID').AsInteger;
      pLeave^.StartDate := FieldByName('startDate').AsDateTime;
      pLeave^.EndDate := FieldByName('endDate').AsDateTime;
      pLeave^.LeaveType:= FieldByName('type').AsInteger;
      pLeave^.DurationDays:= FieldByName('durationDays').AsInteger;
      pLeave^.EmployeeID := FieldByName('employeeID').AsInteger;
      pLeave^.EmployeeName := FieldByName('name').AsString;

      LeaveList.AddObject(pLeave^.LeaveID.ToString, TObject(PLeave));
      Next;
    end;
    finally
    Close;
  end;
end;


procedure AddLeave(pLeave: PLeaveInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOAddLeave.Parameters;
  with frmAdoCollections.ADOAddLeave do try
    ADOSetAsInteger(Params, 'leaveID', frmADOCollections.GenNewRecID('leaveID'));
    ADOSetAsDateTime(Params, 'startDate', pLeave^.StartDate);
    ADOSetAsDateTime(Params, 'endDate', pLeave^.EndDate);
    ADOSetAsInteger(Params, 'type', pLeave^.LeaveType);
    ADOSetAsInteger(Params, 'durationDays', pLeave^.DurationDays);
    ADOSetAsInteger(Params, 'employeeID', pLeave^.EmployeeID);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Leave Data.');
  end;
end;


procedure UpdateLeave(pLeave: PLeaveInfo);
var Params: TParameters;
begin
  BeginTransaction;
  Params:= frmADOCollections.ADOUpdateLeave.Parameters;
  with frmAdoCollections.ADOUpdateLeave do try
    ADOSetAsInteger(Params, 'leaveID', pLeave^.LeaveID);
    ADOSetAsDateTime(Params, 'startDate', pLeave^.StartDate);
    ADOSetAsDateTime(Params, 'endDate', pLeave^.EndDate);
    ADOSetAsInteger(Params, 'type', pLeave^.LeaveType);
    ADOSetAsInteger(Params, 'durationDays', pLeave^.DurationDays);
    ADOSetAsInteger(Params, 'employeeID', pLeave^.EmployeeID);

    ExecSQL;
    CommitMainTransaction;
  except
    RollbackMainTransaction;
    ShowMessage('Error Saving Leave Data.');
  end;
end;


// DB Utils
//--------------------------------------------------------------------------------
procedure TfrmADOCollections.FormDestroy(Sender: TObject);
begin
  //
end;

function TfrmADOCollections.GenNewRecID(sGenName: string): Integer;
begin
  try
    spGenNew_Id.Parameters.Refresh;
    with spGenNew_Id do try
      Parameters.ParamByName('@GENNAME').Value  := sGenName;
      Parameters.ParamByName('@INCVALUE').Value := 1;
      Parameters.ParamByName('@NEXTID').Value   := 0;
      ExecProc;
      result := Parameters.ParamByName('@RETURN_VALUE').Value;
    finally
      Close;
    end;
  except
    result := -1;
  end;
end;


procedure BeginTransaction;
begin
  frmADOCollections.ADOConnection.BeginTrans;
end;


procedure CheckMainTransaction;
begin
  frmADOCollections.ADOConnection.BeginTrans;
end;


procedure CommitMainTransaction;
begin
  frmADOCollections.ADOConnection.CommitTrans;
end;


procedure CommitAndCheckMainTransaction;
begin
  //if frmGR.tran.InTransaction then frmGR.tran.Commit;
  //CheckMainTransaction;
end;


procedure RollbackMainTransaction;
begin
  frmADOCollections.ADOConnection.RollbackTrans;
end;


Procedure ADOSetAsInteger(Parm : TParameters; ParName : String; AdoVal : integer);
begin
  Parm.ParamByName(ParName).DataType := ftInteger;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


Procedure ADOSetAsInt64(Parm : TParameters; ParName : String; AdoVal : int64);
begin
  Parm.ParamByName(ParName).DataType := ftLargeint;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


Procedure ADOSetAsDateTime(Parm : TParameters; ParName : String; AdoVal : TDateTime);
//var Parm : TParameters;
begin
  //Par := Parm.AddParameter;  Par.Name  := ParName;
  Parm.ParamByName(ParName).DataType := ftDateTime;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


Procedure ADOSetAsString(Parm : TParameters; ParName : String; AdoVal : String);
begin
  Parm.ParamByName(ParName).DataType := ftString;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


Procedure ADOSetAsWString(Parm : TParameters; ParName : String; AdoVal : String);
begin
  Parm.ParamByName(ParName).DataType := ftWideString;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


Procedure ADOSetAsFloat(Parm : TParameters; ParName : String; AdoVal : double);
begin
  Parm.ParamByName(ParName).DataType := ftFloat;
  Parm.ParamByName(ParName).Value := AdoVal;
end;


// Disposal
//------------------------------------------------------------------------------
procedure ClearEmpMem(EmpLst : TStringlist);
var I: Integer;
  pEmp: PEmployeeInfo;
begin
  for I := 0 to EmpLst.Count -1 do begin
    pEmp := PEmployeeInfo(EmpLst.Objects[I]);
    dispose(pEmp);
  end;
  EmpLst.Clear;
end;

end.
