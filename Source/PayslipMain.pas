unit PayslipMain;

                                        interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ExtCtrls, FR_View, ComCtrls, StdCtrls, gtXportIntf, gtFRXportIntf, gtClasses, gtCstDocEng,
  gtCstPlnEng, gtCstPDFEng, gtExPDFEng, gtPDFEng, FR_Class, Buttons, PaySlipCommon, Data.DB,
  Vcl.Grids, Vcl.DBGrids, ADODB, AdoDBConx, frxClass, frxPreview, frxExportPDF;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type
  TMain = class(TForm)
    PnlLeft: TPanel;
    PnlRight: TPanel;
    Panel3: TPanel;
    PnlBot: TPanel;
    Panel5: TPanel;
    PnlPrintEmp: TPanel;
    Panel7: TPanel;
    Splitter1: TSplitter;
    PnlPrintData: TPanel;
    LVPrintEmployee: TListView;
    Menu: TMainMenu;
    miSettings: TMenuItem;
    miSetDefDir: TMenuItem;
    miEditEmployee: TMenuItem;
    edtCalcHour: TEdit;
    lblCalcHour: TLabel;
    edtCalcHR: TEdit;
    lblCalcHR: TLabel;
    edtIncentive: TEdit;
    lblIncentive: TLabel;
    EndDate: TDateTimePicker;
    lblEndDate: TLabel;
    StartDate: TDateTimePicker;
    lblStartDate: TLabel;
    btnReCalc: TButton;
    gtPDFPayslip: TgtPDFEngine;
    gtFRFilter: TgtFRExportInterface;
    frReport: TfrReport;
    OpenDialog1: TOpenDialog;
    miTax: TMenuItem;
    miAbout: TMenuItem;
    sbPrint: TSpeedButton;
    LeaveBalances: TMenuItem;
    frxPayslipRpt: TfrxReport;
    frxPreviewPSlip: TfrxPreview;
    frxPDFGenerator: TfrxPDFExport;
    LeaveTypes1: TMenuItem;
    procedure miTaxClick(Sender: TObject);
    procedure edtIncentiveExit(Sender: TObject);
    procedure edtIncentiveChange(Sender: TObject);
    procedure edtCalcHRExit(Sender: TObject);
    procedure edtCalcHRChange(Sender: TObject);
    procedure edtIncentiveKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCalcHRKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCalcHourKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCalcHourChange(Sender: TObject);
    procedure edtCalcHourExit(Sender: TObject);
    procedure EndDateChange(Sender: TObject);
    procedure StartDateChange(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure btnReCalcClick(Sender: TObject);
    procedure LVPrintEmployeeSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miEditEmployeeClick(Sender: TObject);
    procedure miSetDefDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Calculate(pEmp : PEmployeeInfo);
    procedure Preview(pEmp : PEmployeeInfo);
    procedure LeaveBalancesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LeaveTypes1Click(Sender: TObject);
  private
    DataChanged : Boolean;
    DefPdfDir : String;
    function  GetCurrentSelEmp: PEmployeeInfo;
    procedure LoadEmployeesToLV;
    procedure ReCalcPayslip(pEmp : PEmployeeInfo);
    procedure PrintPayslipFr5(pEmp: PEmployeeInfo);
    procedure SetPayslipRptFr5(pEmp: PEmployeeInfo);
  public
    TaxInfo : TTaxInfo;
    EmployeeList : TStringList;
  end;

var Main: TMain;

                                     implementation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses FileCtrl, DefaultDir, EmployeeInfo, TaxRate, LeaveInfo, LeaveTypeInfo;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{$R *.dfm}

// Initialise
//------------------------------------------------------------------------------
procedure TMain.FormCreate(Sender: TObject);
begin
  // Create new config.ini file if there is none in directory
  if Not Fileexists(ConfigIniFile) then begin
    InitialTaxInfo(TaxInfo);
    WriteTaxInfoToIniFile(TaxInfo);
    DefPdfDir := '.\Reports';
    WriteDefDirToIniFile(DefPdfDir);

  end else begin
    ReadTaxInfo(TaxInfo);
    RdDefDirFromIniFile(DefPdfDir);
  end;

  // Create 'Reports' folder in the same directory of program by default
  if Not DirectoryExists('Reports') then
    CreateDir('Reports');

  DataChanged := False;
  startDate.Date:= Date;
  endDate.Date:= Date + 14;
  edtIncentive.Text:= '0';
  btnReCalc.Enabled:= False;

  EmployeeList := TStringList.Create();
  LoadEmployeesToLV;

  if LVPrintEmployee.Items.Count > 0 then
    LVPrintEmployee.ItemIndex := 0;  // this will trigger OnSelectItem event : LVPrintEmployeeSelectItem
end;


// Disposal
//------------------------------------------------------------------------------
procedure TMain.FormDestroy(Sender: TObject);
begin
  ClearEmpMem(EmployeeList);
  EmployeeList.Free;
  frmADOCollections.Free;
end;

//------------------------------------------------------------------------------
procedure TMain.LoadEmployeesToLV;
var LstItem: TListItem;
  i: Integer;
  pEmp: PEmployeeInfo;
begin
  GetEmployees(EmployeeList);

//  exit;

  LVPrintEmployee.Clear;
  for i := 0 to EmployeeList.Count - 1 do begin
    // For each item in Employee list, create list item and populate with
    // caption + employee data object
    pEmp := PEmployeeInfo(EmployeeList.Objects[i]);
    if pEmp <> Nil then begin
      LstItem := LVPrintEmployee.Items.Add;
      LstItem.Caption := pEmp^.EmployeeName;
      LstItem.Data := TObject(pEmp);
    end;
  end;
end;

// Menu Items
//------------------------------------------------------------------------------

// Chooses new save directory for future reports
//--------------------------------------------------------------------------------
procedure TMain.miSetDefDirClick(Sender: TObject);
var sDir: String;
    suc : boolean;
begin
  RdDefDirFromIniFile(DefPdfDir);
  sDir := DefPdfDir;
  suc := SetDefDirectory(sDir);
  if not suc then exit;
  DefPdfDir := sDir;
end;

// Open Employee config, then fetch employees once config is closed
//------------------------------------------------------------------------------
procedure TMain.miEditEmployeeClick(Sender: TObject);
begin
  ShowEmployees;
  LoadEmployeesToLV;
  if LVPrintEmployee.Items.Count > 0 then
    LVPrintEmployee.ItemIndex := 0;
end;

// Leave config
//------------------------------------------------------------------------------
procedure TMain.LeaveBalancesClick(Sender: TObject);
begin
  ShowLeaves;
end;

//--------------------------------------------------------------------------------
procedure TMain.LeaveTypes1Click(Sender: TObject);
begin
  ShowLeaveTypes;
end;

// Tac rate config
//------------------------------------------------------------------------------
procedure TMain.miTaxClick(Sender: TObject);
begin
  ShowTaxRate(TaxInfo);
end;

// Actions
//------------------------------------------------------------------------------
procedure TMain.sbPrintClick(Sender: TObject);
var pEmp : PEmployeeInfo;
begin
  pEmp := GetCurrentSelEmp;
  PrintPayslipFr5(pEmp);
  ReCalcPayslip(Nil);
end;

// Helper functions
//------------------------------------------------------------------------------
// Return currently selected employee
Function TMain.GetCurrentSelEmp : PEmployeeInfo;
var lst : TListItem;
    pEmp : PEmployeeInfo;
begin
  pEmp := Nil;
  lst := LVPrintEmployee.Selected;
  if lst <> Nil then
    pEmp := PEmployeeInfo(lst.Data);

  result := pEmp;
end;


//Recalculates payslip information with information in input fields
//------------------------------------------------------------------------------
procedure TMain.btnReCalcClick(Sender: TObject);
begin
  ReCalcPayslip(Nil);
end;


// Event listeners
//------------------------------------------------------------------------------
// Prepares selected employee report on employee list item click
procedure TMain.LVPrintEmployeeSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var pEmp : PEmployeeInfo;
begin
  pEmp := PEmployeeInfo(Item.Data);
  if pEmp = Nil then exit;
  edtCalcHour.text:= FormatFloat('0.##',pEmp^.Hours);
  edtCalcHR.text:= FormatFloat('0.##',pEmp^.HourRate);
  edtIncentive.Text:= '0';
  ReCalcPayslip(pEmp);
end;


// Date range
//--------------------------------------------------------------------------------
procedure TMain.StartDateChange(Sender: TObject);
begin
  if DataChanged then
    ReCalcPayslip(Nil);
end;

//--------------------------------------------------------------------------------
procedure TMain.EndDateChange(Sender: TObject);
begin
  if DataChanged then
    ReCalcPayslip(Nil);
end;

// Hours
//--------------------------------------------------------------------------------
procedure TMain.edtCalcHourChange(Sender: TObject);
begin
  DataChanged := True;
end;

//--------------------------------------------------------------------------------
procedure TMain.edtCalcHourExit(Sender: TObject);
begin
  if DataChanged then
    ReCalcPayslip(Nil);
end;

//--------------------------------------------------------------------------------
procedure TMain.edtCalcHourKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> 13 then exit;
  if DataChanged then
    ReCalcPayslip(Nil);
end;

// Hour Rate
//--------------------------------------------------------------------------------
procedure TMain.edtCalcHRChange(Sender: TObject);
begin
  DataChanged := True;
end;

//--------------------------------------------------------------------------------
procedure TMain.edtCalcHRExit(Sender: TObject);
begin
  if DataChanged then
    ReCalcPayslip(Nil);
end;

//--------------------------------------------------------------------------------
procedure TMain.edtCalcHRKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> 13 then exit;
  if DataChanged then
    ReCalcPayslip(Nil);
end;

// Incentive
//--------------------------------------------------------------------------------
procedure TMain.edtIncentiveChange(Sender: TObject);
begin
  DataChanged := True;
end;

//--------------------------------------------------------------------------------
procedure TMain.edtIncentiveExit(Sender: TObject);
begin
  if DataChanged then
    ReCalcPayslip(Nil);
end;

//--------------------------------------------------------------------------------
procedure TMain.edtIncentiveKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> 13 then exit;
  if DataChanged then
    ReCalcPayslip(Nil);
end;


// Methods
//------------------------------------------------------------------------------
// Calculates payslip information to be displayed on report
procedure TMain.Calculate(pEmp : PEmployeeInfo);
var I: Integer;
    IncomeTax, ACCLevy, annualIncome : double;
begin
  //Collects numeric variables and converts to float type
  pEmp^.Incentive:= StrToFloat(edtIncentive.Text);
  pEmp^.Hours := StrToFloat(edtCalcHour.Text);
  pEmp^.HourRate := StrToFloat(edtCalcHR.Text);

  //Calculates Gross Income, Annual Income, ACC Levy
  pEmp^.GrossIncome14D := (pEmp^.Hours * pEmp^.HourRate) + pEmp^.Incentive;
  AnnualIncome := pEmp^.GrossIncome14D * 26;
  ACCLevy := TaxInfo.AccRate * AnnualIncome;

  //Calculates income tax for each tax bracket
  incomeTax:= 0;
  for I := 1 to TaxNum - 1 do begin
    if AnnualIncome > TaxInfo.Thresholds[I] then
      incomeTax:= incomeTax + (TaxInfo.Thresholds[i] - TaxInfo.Thresholds[i-1]) * TaxInfo.IrdTaxRates[i-1];
    if (annualIncome > TaxInfo.Thresholds[i-1]) and (annualIncome < TaxInfo.Thresholds[i]) then
      incomeTax:= incomeTax + (annualIncome - TaxInfo.Thresholds[i-1]) * TaxInfo.IrdTaxRates[i-1];
  end;

  //Calculates Total Tax and Net Payment
  pEmp^.TotalTax := (ACCLevy + incomeTax) / 26;
  pEmp^.netPayment := pEmp^.GrossIncome14D - pEmp^.TotalTax;

  //Calculates KiwiSaver Tax
  pEmp^.KiwiSvrTax:= 0;
  if pEmp^.HasKiwiSaver then begin
    pEmp^.KiwiSvrTax := pEmp^.GrossIncome14D * TaxInfo.KiwiSaverRate;
    pEmp^.netPayment:= pEmp^.netPayment - pEmp^.KiwiSvrTax;
  end;

  //Calculates Student Loan Tax
  pEmp^.StudentLoanTax := 0;
  if pEmp^.HasStudentLoan then begin
    if pEmp^.GrossIncome14D > TaxInfo.SLoanThreshold then begin
      pEmp^.StudentLoanTax:= (pEmp^.GrossIncome14D - TaxInfo.SLoanThreshold) * TaxInfo.SLoanRate;
      pEmp^.netPayment := pEmp^.netPayment - pEmp^.StudentLoanTax;
    end;
  end;

  //Adds holiday pay to net pay
  //pEmp^.netPayment := pEmp^.netPayment + pEmp^.Incentive;
  btnReCalc.Enabled:= True;
end;

//--------------------------------------------------------------------------------
procedure TMain.ReCalcPayslip(pEmp : PEmployeeInfo);
begin
  if pEmp = Nil then
    pEmp := GetCurrentSelEmp;

  Calculate(pEmp);
  Preview(pEmp);
  DataChanged := False;
end;

// Displays preview of report on preview panel
//--------------------------------------------------------------------------------
procedure TMain.Preview(pEmp : PEmployeeInfo);
begin
  SetPayslipRptFr5(pEmp);
  frxPayslipRpt.Preview := frxPreviewPSlip;

  if frxPayslipRpt.PrepareReport then
    frxPayslipRpt.ShowPreparedReport;
end;

//--------------------------------------------------------------------------------
procedure TMain.PrintPayslipFr5(pEmp : PEmployeeInfo);
begin
  SetPayslipRptFr5(pEmp);

  with frxPayslipRpt do begin
    if PrepareReport then begin
      showmessage(DefPdfDir);
      frxPDFGenerator.DefaultPath := DefPdfDir;
      frxPDFGenerator.FileName := StringReplace(DateToStr(startDate.Date), '/', '', [rfReplaceAll, rfIgnoreCase]) + '_' + Trim(pEmp^.EmployeeName) + '.pdf';
      frxPDFGenerator.ShowDialog := True;
      frxPDFGenerator.UseFMIDxfFineMode := True;
      frxPayslipRpt.Export(frxPDFGenerator);
    end;
  end;
end;

// Loads employee information to report for preview/print
//--------------------------------------------------------------------------------
procedure TMain.SetPayslipRptFr5(pEmp : PEmployeeInfo);
begin
  with frxPayslipRpt do begin
    Script.Variables['CompanyName'] := 'Supreme Doors Ltd.';
    Script.Variables['StartDate'] := DatetoStr(startDate.Date);
    Script.Variables['EndDate']:= DatetoStr(endDate.Date);
    Script.Variables['Name'] := pEmp^.EmployeeName;
    Script.Variables['IRD'] := pEmp^.IRDNum;
    Script.Variables['TaxCode'] := pEmp^.TaxCode;
    Script.Variables['Occupation'] := pEmp^.Occupation;
    Script.Variables['Hours'] := format('%.2f', [pEmp^.Hours]);
    Script.Variables['Rate'] := FormatFloat('$#,##0.00', pEmp^.HourRate);//format('%.2f', [pEmp^.HourRate]);
    Script.Variables['TotalPay'] := FormatFloat('$#,##0.00', pEmp^.GrossIncome14D);//format('%.2f', [pEmp^.GrossIncome14D]);
    Script.Variables['IncomeTax'] := FormatFloat('$#,##0.00', pEmp^.TotalTax);//format('%.2f', [pEmp^.TotalTax]);
    Script.Variables['KiwiSaver'] := format('%.2f', [pEmp^.KiwiSvrTax]);
    Script.Variables['NetPay'] := FormatFloat('$#,##0.00', pEmp^.NetPayment);//format('$%.2f', [pEmp^.NetPayment]);
    Script.Variables['Credit']:= pEmp^.Credit;
    Script.Variables['HolidayPay']:= format('%.2f', [pEmp^.Incentive]);
    Script.Variables['StudentLoan']:= format('%.2f', [pEmp^.StudentLoanTax]);
  end;
end;


end.
