unit PaySlipCommon;

                                interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses Classes, SysUtils;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

const TaxNum = 5;


//OO concept : Object 1 - Tax Infomation
//Saved in file as pSlipConfig.ini
//------------------------------------------------------------------------------
type TTaxInfo = Record
    Thresholds : array [0..TaxNum - 1] of Integer; //0,14000,48000,70000,180000
    IrdTaxRates : array [0..TaxNum - 1] of Double;    //10.5,17.5,30,33,39
    SLoanThreshold : Integer; //fortnightly income  $780
    SLoanRate : Double;       //12%
    KiwiSaverRate : Double;   //3%
    AccRate : Double;         //1.39%
  end;



//OO concept : Object 2 - Personal infomation
//Saved in file as Employees.ini
//------------------------------------------------------------------------------
type PEmployeeInfo = ^TEmployeeInfo;
    TEmployeeInfo = Record
      EmployeeID : Integer;
      EmployeeName : String;
      IRDNum : String;
      TaxCode : String;
      Occupation : String;
      HourRate : double;
      Hours : double;
      Credit : String;
      HasKiwiSaver : boolean;
      HasStudentLoan : boolean;
      StartDate: TDateTime;
      TotalLeave: double;
      //--------------
      TotalTax, NetPayment, grossIncome14D : double;
      Incentive, KiwiSvrTax, StudentLoanTax : double;
  end;

  type PLeaveTypeInfo = ^TLeaveTypeInfo;
    TLeaveTypeInfo = Record
      id : Integer;
      desc : String;
  end;

//OO concept : Object 1 - Tax Infomation
//Saved in file as pSlipConfig.ini
//------------------------------------------------------------------------------
type PLeaveInfo = ^TLeaveInfo;
    TLeaveInfo = Record
    LeaveID : Integer;
    StartDate : TDateTime;
    EndDate : TDateTime;
    LeaveType : Integer;
    DurationDays : Integer;
    EmployeeID : Integer;
    EmployeeName: String;
  end;

//------------------------------------------------------------------------------
const ConfigIniFile : String = '.\pSlipConfig.ini';
      EmployeeIniFile : String = '.\Employees.ini';
      DBIniFile : String = '.\DBConnection.ini';
      DefThresholds : array [0..TaxNum - 1] of Integer = (0,14000,48000,70000,180000);
      DefIrdTaxRates : array [0..TaxNum - 1] of Double = (10.5,17.5,30,33,39);
      DefSLoanThreshold : Integer = 780;
      DefSLoanRate : Double = 0.12;
      DefKiwiSaverRate : Double = 0.03;
      DefAccRate : Double = 0.0139;

function GetDBServerInf(var ServerNm, DBName : String): Boolean;
Procedure WriteTaxInfoToIniFile(var TaxInfo : TTaxInfo);
function  ReadTaxInfo(var TaxInfo : TTaxInfo): Boolean;
Procedure RdDefDirFromIniFile(var sDefDir : String);
Procedure WriteDefDirToIniFile(sDefDir : String);
Procedure InitialTaxInfo(var TaxInfo : TTaxInfo);
function  BooleanToTF(b: Boolean): String;
Procedure SaveEmployees(EmployeeLst : TStringList);


function StrToDoubleDef(Txt: String; DefV : double = 0): Double;
Function StrToIntDef(txt : String; DefV : integer = 0) : Integer;
function IsValidIntegerStr(Txt: String) : Boolean;
function IsValidDoubleStr(Txt: String) : Boolean;


                               implementation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses IniFiles;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//------------------------------------------------------------------------------
function GetDBServerInf(var ServerNm, DBName : String): Boolean;
var bankAcc : String;
begin
  result := False;
  if not FileExists(DBIniFile) then exit;

  result := True;
  with TIniFile.Create(DBIniFile) do try
    ServerNm := Trim(ReadString('Payslip', 'Server', ''));
    DBName := Trim(ReadString('Payslip', 'DBName', ''));

  finally
    Free;
  end;

end;

//------------------------------------------------------------------------------
Function StrToIntDef(txt : String; DefV : integer = 0) : Integer;
var edtV, ErrPosi: Integer;
  Suc : Boolean;
begin
  Val(Txt, edtV, ErrPosi);
  result := edtV;
  suc := (ErrPosi = 0);
  if not suc then
    result := DefV;
end;

//------------------------------------------------------------------------------
function StrToDoubleDef(Txt: String; DefV : double = 0): Double;
var edtV : Double;
    ErrPosi: Integer;
begin
  Val(Txt, edtV, ErrPosi);
  if (ErrPosi > 0) then
    edtV := DefV;
  result := edtV;
end;

//------------------------------------------------------------------------------
function IsValidIntegerStr(Txt: String) : Boolean;
var edtV, ErrPosi: Integer;
begin
  Val(Txt, edtV, ErrPosi);
  result := (ErrPosi = 0);
end;

//------------------------------------------------------------------------------
function IsValidDoubleStr(Txt: String) : Boolean;
var edtV : Double;
    ErrPosi: Integer;
begin
  Val(Txt, edtV, ErrPosi);
  result := (ErrPosi = 0);
end;

//------------------------------------------------------------------------------
Procedure initialTaxInfo(var TaxInfo : TTaxInfo);
var I : integer;
begin
  For I := 0 to TaxNum - 1 do begin
    TaxInfo.Thresholds[I] := DefThresholds[I];
    TaxInfo.IrdTaxRates[I] := DefIrdTaxRates[I]/100;
  end;

  TaxInfo.SLoanThreshold := DefSLoanThreshold;
  TaxInfo.SLoanRate := DefSLoanRate;
  TaxInfo.KiwiSaverRate := DefKiwiSaverRate;
  TaxInfo.AccRate := DefAccRate;
end;

//------------------------------------------------------------------------------
function ReadTaxInfo(var TaxInfo : TTaxInfo): Boolean;
var sIncomtax, sTaxRates, sStudentThreshold, sStudentRate, sKiwiSaverR, sACCR: String;
    IncomtaxLst, TaxRatesLst : TStringList;
    I : integer;
    ThresholdV : integer;
    TaxRate : Double;
begin
  result := False;
  if not FileExists(ConfigIniFile) then exit;

  IncomtaxLst := TStringList.Create();
  TaxRatesLst := TStringList.Create();
  with TIniFile.Create(ConfigIniFile) do try
    sIncomtax := ReadString('IncomeTax', 'Thresholds', '');
    sTaxRates := ReadString('IncomeTax', 'TaxRates', '');
    IncomtaxLst.CommaText := sIncomtax;
    TaxRatesLst.CommaText := sTaxRates;
    sStudentThreshold := ReadString('StudentLoan', 'threshold', '0');
    sStudentRate := ReadString('StudentLoan', 'TaxRates', '0');
    sKiwiSaverR := ReadString('KiwiSaver', 'ContributeRate', '');
    sACCR := ReadString('ACC', 'ACCRate', '0');
  finally
    Free;
  end;


  if (IncomtaxLst.Count >= TaxNum) and (TaxRatesLst.count >= TaxNum) then begin
    For I := 0 to TaxNum - 1 do begin
      ThresholdV := StrToIntDef(IncomtaxLst[I]);
      TaxInfo.Thresholds[I] := ThresholdV;

      TaxRate := StrToDoubleDef(TaxRatesLst[I]);
      TaxInfo.IrdTaxRates[I] := TaxRate/100;
    end;
  end;

  TaxInfo.SLoanThreshold := StrToIntDef(sStudentThreshold);
  TaxInfo.SLoanRate := StrToDoubleDef(sStudentRate)/100;
  TaxInfo.KiwiSaverRate := StrToDoubleDef(sKiwiSaverR)/100;
  TaxInfo.AccRate := StrToDoubleDef(sACCR)/100;

  IncomtaxLst.Free;
  TaxRatesLst.Free;

  result := True;
end;

//------------------------------------------------------------------------------
Procedure WriteTaxInfoToIniFile(var TaxInfo : TTaxInfo);
var sValue: string;
    I : integer;
begin
  with TIniFile.Create(ConfigIniFile) do try

    sValue := '0';
    For I := 1 to TaxNum - 1 do
      sValue := sValue + ',' + FormatFloat('#.##', TaxInfo.Thresholds[I]);
    WriteString('IncomeTax', 'Thresholds', sValue);

    sValue := FormatFloat('#.##', TaxInfo.IrdTaxRates[0] * 100);
    For I := 1 to TaxNum - 1 do
      sValue := sValue + ',' + FormatFloat('#.##', TaxInfo.IrdTaxRates[I] * 100);
    WriteString('IncomeTax', 'TaxRates', sValue);


    sValue := IntToStr(TaxInfo.SLoanThreshold);
    WriteString('StudentLoan', 'threshold', sValue);

    sValue := FormatFloat('#.##', TaxInfo.SLoanRate * 100);
    WriteString('StudentLoan', 'TaxRates', sValue);

    sValue := FormatFloat('#.##', TaxInfo.KiwiSaverRate * 100);
    WriteString('KiwiSaver', 'ContributeRate', sValue);

    sValue := FormatFloat('#.##', TaxInfo.AccRate * 100);
    WriteString('ACC', 'ACCRate', sValue);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
Procedure RdDefDirFromIniFile(var sDefDir : String);
begin
  with TIniFile.Create(ConfigIniFile) do try
    sDefDir := ReadString('DefaultDir', 'DefDir', sDefDir);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
Procedure WriteDefDirToIniFile(sDefDir : String);
begin
  with TIniFile.Create(ConfigIniFile) do try
    WriteString('DefaultDir', 'DefDir', sDefDir);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
function BooleanToTF(b: Boolean): String;
begin
  if b then Result := 'T' else Result := 'F';
end;


//------------------------------------------------------------------------------
Procedure SaveEmployees(EmployeeLst : TStringList);
var I: Integer;
    pEmp : PEmployeeInfo;
    sline: String;
    myFile: Textfile;
begin
  AssignFile(myFile, EmployeeIniFile);
  Rewrite(myFile);

  for I := 0 to EmployeeLst.Count - 1 do begin
    pEmp := PEmployeeInfo(EmployeeLst.Objects[I]);
    if pEmp <> Nil then begin
      sline := pEmp^.EmployeeName;
      sline := sline + ',' + FormatFloat('#.##', pEmp^.Hours);
      sline := sline + ',' + FormatFloat('#.##', pEmp^.HourRate);
      sline := sline + ',' + BooleanToTF(pEmp^.HasKiwiSaver);
      sline := sline + ',' + pEmp^.IRDNum;
      sline := sline + ',' + pEmp^.TaxCode;
      sline := sline + ',' + pEmp^.Occupation;
      sline := sline + ',' + pEmp^.Credit;
      sline := sline + ',' + BooleanToTF(pEmp^.HasStudentLoan);
    end;

    WriteLn(myFile, sline);
  end;
  CloseFile(myFile);
end;

end.
