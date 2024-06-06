unit AddLeaveDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;

type
  TAddLeaveForm = class(TForm)
    RelativePanel1: TRelativePanel;
    EmployeeSelect: TComboBox;
    EmployeeLbl: TLabel;
    LeaveSelect: TComboBox;
    LeaveLbl: TLabel;
    StartDate: TDateTimePicker;
    StartDateLbl: TLabel;
    EndDate: TDateTimePicker;
    EndDateLbl: TLabel;
    DurationEdit: TEdit;
    Label1: TLabel;
    AddButton: TButton;
    procedure AddButtonClick(Sender: TObject);
  private
    fEmployeeList, fLeaveTypeList: TStringList;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddLeaveForm: TAddLeaveForm;

Procedure ShowAddLeave;

implementation
uses LeaveInfo;

// Load dialog
//------------------------------------------------------------------------------
Procedure ShowAddLeave;
begin
  With TAddLeaveForm.Create(Nil) do try
    // Fetch and load dropdowns
    fEmployeeList := TStringList.Create();
    fLeaveTypeList := TStringList.Create();
    GetEmployees(fEmployeeList);
    GetLeaveTypes(fLeaveTypeList);

    EmployeeSelect.Items.Assign(fEmployeeList);
    LeaveSelect.Items.Assign(fLeaveTypeList); // How to display text instead of enum?

    ShowModal;
  finally
    fEmployeeList.Free;
    fLeaveTypeList.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAddLeaveForm.AddButtonClick(Sender: TObject);
var
  pLeave : PLeaveInfo;
  pEmp : PEmployeeInfo;
begin
  new(pLeave);
//  pEmp := EmployeeSelect.Items[EmployeeSelect.ItemIndex];
//  showMessage(pEmp^.EmployeeID.ToString());
  pLeave^.EmployeeID:= StrToInt(EmployeeSelect.Items[EmployeeSelect.ItemIndex]); // How to access .data? dont want caption
  pLeave^.LeaveType := StrToInt(LeaveSelect.Items[LeaveSelect.ItemIndex]);
  pLeave^.StartDate := StartDate.DateTime;
  pLeave^.EndDate := EndDate.DateTime;
  pLeave^.DurationDays := StrToInt(DurationEdit.Text);

  AddLeave(pLeave);  // Need to change AddLeave.pas name
  Close;
end;

{$R *.dfm}

end.
