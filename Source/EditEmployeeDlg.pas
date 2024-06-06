unit EditEmployeeDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;
type
  TEditEmployeeForm = class(TForm)
    RelativePanel1: TRelativePanel;
    NameLbl: TLabel;
    IrdLbl: TLabel;
    TaxCodeLbl: TLabel;
    HourRateLbl: TLabel;
    HoursLbl: TLabel;
    EmployeeNameEdit: TEdit;
    IrdNumberEdit: TEdit;
    TaxCodeEdit: TEdit;
    HourRateEdit: TEdit;
    OccupationEdit: TEdit;
    HoursEdit: TEdit;
    OccupationLbl: TLabel;
    CreditEdit: TEdit;
    CreditLbl: TLabel;
    StartDate: TDateTimePicker;
    StartDateLbl: TLabel;
    RadioKS: TRadioGroup;
    RadioSL: TRadioGroup;
    SaveButton: TButton;
    DeleteButton: TButton;
    procedure SaveButtonClick(Sender: TObject);
    function ValidateSave: Boolean;
    procedure DeleteButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditEmployeeForm: TEditEmployeeForm;
  pEmp: PEmployeeInfo;

Procedure ShowEditEmployee(UniqueIRDs: TStringList; Item: TListItem);

implementation

// Setup and populate fields
//------------------------------------------------------------------------------
Procedure ShowEditEmployee(UniqueIRDs: TStringList; Item: TListItem);
begin
  With TEditEmployeeForm.Create(Nil) do try
    pEmp := PEmployeeInfo(Item.Data);

    EmployeeNameEdit.Text := pEmp^.EmployeeName;
    IrdNumberEdit.Text:= pEmp^.IRDNum;
    TaxCodeEdit.Text := pEmp^.TaxCode;
    HourRateEdit.Text := pEmp^.HourRate.ToString;
    HoursEdit.Text := pEmp^.Hours.ToString;
    OccupationEdit.Text := pEmp^.Occupation;
    CreditEdit.Text := pEmp^.Credit;
    StartDate.DateTime := pEmp^.StartDate;

    if pEmp^.HasKiwiSaver then
      RadioKS.ItemIndex := 0
    else
      RadioKS.ItemIndex := 1;

    if pEmp^.HasStudentLoan then
      RadioSL.ItemIndex := 0
    else
      RadioSL.ItemIndex := 1;

    ShowModal;
  finally
    free;
  end;
end;

// Validate input fields
//------------------------------------------------------------------------------
function TEditEmployeeForm.ValidateSave: Boolean;
var
  validProfile: Boolean;
begin
  validProfile:= False;
  if Trim(EmployeeNameEdit.Text) = '' then begin
    Showmessage('Please enter a name');
  end else if Trim(IrdNumberEdit.Text) = '' then begin
    Showmessage('Please enter IRD number');
  end else if (Trim(HoursEdit.Text) = '') and (not IsValidIntegerStr(HoursEdit.Text)) then begin
    Showmessage('Please enter number of hours');
  end else if (Trim(HourRateEdit.Text) = '') and (not IsValidIntegerStr(HourRateEdit.Text)) then begin
    Showmessage('Please enter hourly rate');
  end else if Trim(TaxCodeEdit.Text) = '' then begin
    Showmessage('Please enter Tax Code');
  end else if Trim(OccupationEdit.Text) = '' then begin
    Showmessage('Please select Occupation');
  end else if RadioKS.ItemIndex = -1 then begin
    Showmessage('Please select KiwiSaver status');
  end else if RadioSL.ItemIndex = -1 then begin
    Showmessage('Please select Student Loan status');
  end else if RadioKS.ItemIndex = -1 then begin
    Showmessage('Please select KiwiSaver status');
  end else begin
    validProfile:= True;
  end;

  result := validProfile;
end;

// Update Employee
//------------------------------------------------------------------------------
procedure TEditEmployeeForm.SaveButtonClick(Sender: TObject);
begin
  if not ValidateSave then exit;

  // Prepare payload
  pEmp^.EmployeeName := EmployeeNameEdit.Text;
  pEmp^.IRDNum := IrdNumberEdit.Text;
  pEmp^.Hours := StrToDoubleDef(HoursEdit.Text);
  pEmp^.HourRate := StrToDoubleDef(HourRateEdit.Text);
  pEmp^.TaxCode := TaxCodeEdit.Text;
  pEmp^.Occupation := OccupationEdit.Text;
  pEmp^.Credit := CreditEdit.Text;
  pEmp^.StartDate := StartDate.DateTime;
  pEmp^.HasKiwiSaver := (RadioKS.ItemIndex = 0);
  pEmp^.HasStudentLoan := (RadioSL.ItemIndex = 0);

  UpdateEmployee(pEmp);
  Close;
end;

// Delete Employee by ID
//------------------------------------------------------------------------------
procedure TEditEmployeeForm.DeleteButtonClick(Sender: TObject);
begin
  DeleteEmployee(pEmp^.EmployeeId);
  Close;
end;

{$R *.dfm}

end.
