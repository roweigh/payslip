unit EditLeaveDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;

type
  TEditLeaveForm = class(TForm)
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
    SaveButton: TButton;
    procedure SaveButtonClick(Sender: TObject);
  private
    { Private declarations }
    LeaveInfo: PLeaveInfo;
    fEmployeeList, fLeaveTypeList: TStringList;
  public
    { Public declarations }
  end;

var
  EditLeaveForm: TEditLeaveForm;

Procedure ShowEditLeave(Item: TListItem);

implementation

// Setup and populate fields
//------------------------------------------------------------------------------
Procedure ShowEditLeave(Item: TListItem);
begin
  With TEditLeaveForm.Create(Nil) do try
    // Fetch and load dropdowns
    fEmployeeList := TStringList.Create();
    fLeaveTypeList := TStringList.Create();
    GetEmployees(fEmployeeList);
    GetLeaveTypes(fLeaveTypeList);

    EmployeeSelect.Items.Assign(fEmployeeList);
    LeaveSelect.Items.Assign(fLeaveTypeList); // How to display text instead of enum?

    // Load selected leave info to inputs
    LeaveInfo := PLeaveInfo(Item.Data);
    EmployeeSelect.ItemIndex := EmployeeSelect.Items.IndexOf(LeaveInfo^.EmployeeName);
    LeaveSelect.ItemIndex := LeaveSelect.Items.IndexOf(IntToStr(LeaveInfo^.LeaveType));
    StartDate.DateTime := LeaveInfo^.StartDate;
    EndDate.DateTime := LeaveInfo^.EndDate;
    DurationEdit.Text := IntToStr(LeaveInfo^.DurationDays);

    ShowModal;
  finally
  end;
end;

// Construct payload and update selected leave
//------------------------------------------------------------------------------
procedure TEditLeaveForm.SaveButtonClick(Sender: TObject);
var
  pLeave : PLeaveInfo;
begin
  new(pLeave);
  pLeave^.LeaveID := LeaveInfo^.LeaveID;
  pLeave^.EmployeeID := LeaveInfo^.EmployeeID;
  pLeave^.LeaveType:= StrToInt(LeaveSelect.Items[LeaveSelect.ItemIndex]);
  pLeave^.StartDate := StartDate.DateTime;
  pLeave^.EndDate := EndDate.DateTime;
  pLeave^.DurationDays := StrToInt(DurationEdit.Text);

  UpdateLeave(pLeave);
  Close;
end;

{$R *.dfm}

end.
