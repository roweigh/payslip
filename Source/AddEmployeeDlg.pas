unit AddEmployeeDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;
type
  TAddEmployeeForm = class(TForm)
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
    procedure SaveButtonClick(Sender: TObject);
    function ValidateSave: Boolean;
  private
    { Private declarations }
    IRDList: TStringList;
  public
    { Public declarations }
  end;

var
  AddEmployeeForm: TAddEmployeeForm;

Procedure ShowAddEmployee(UniqueIRDs: TStringList);

                     implementation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//uses ;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Procedure ShowAddEmployee(UniqueIRDs: TStringList);
begin
  With TAddEmployeeForm.Create(Nil) do try
    IRDList := UniqueIRDs;
    ShowModal
  finally
    free;
  end;
end;

// Update Employee
//------------------------------------------------------------------------------
procedure TAddEmployeeForm.SaveButtonClick(Sender: TObject);
var EmpRec : TEmployeeInfo;
begin
  EmpRec.EmployeeName := EmployeeNameEdit.Text;
  EmpRec.IRDNum := IrdNumberEdit.Text;
  EmpRec.Hours := StrToDoubleDef(HoursEdit.Text);
  EmpRec.HourRate := StrToDoubleDef(HourRateEdit.Text);
  EmpRec.TaxCode := TaxCodeEdit.Text;
  EmpRec.Occupation := OccupationEdit.Text;
  EmpRec.Credit := CreditEdit.Text;
  EmpRec.StartDate := StartDate.DateTime;
  EmpRec.HasKiwiSaver := (RadioKS.ItemIndex = 0);
  EmpRec.HasStudentLoan := (RadioSL.ItemIndex = 0);

  AddEmployee(EmpRec);
  Close;
end;

// Validate input fields
//------------------------------------------------------------------------------
function TAddEmployeeForm.ValidateSave: Boolean;
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

{$R *.dfm}

end.
