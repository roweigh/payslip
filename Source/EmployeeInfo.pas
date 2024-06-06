unit EmployeeInfo;

                                     interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, PaySlipCommon, AdoDBConx, DateUtils,
  EditEmployeeDlg, AddEmployeeDlg, Vcl.WinXCtrls;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type
  TfrmEmployee = class(TForm)
    LVEmpSettings: TListView;
    RelativePanel1: TRelativePanel;
    AddButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LVEmpSettingsDblClick(Sender: TObject);
  private
    { Private declarations }
    eventEnabled : Boolean;
    fEmployeeList, fIRDList: TStringList;
    procedure LoadEmployeesToLV;
  public
    { Public declarations }
  end;

Procedure ShowEmployees;

                              implementation
//--------------------------------------------------------
//
//--------------------------------------------------------

{$R *.dfm}

// Setup
//------------------------------------------------------------------------------
Procedure ShowEmployees;
begin
  With TfrmEmployee.Create(Nil) do try
    fEmployeeList := TStringList.Create();
    fIRDList := TStringList.Create();
    LoadEmployeesToLV;
    ShowModal;
  finally
    Free;
  end;
end;

// Fetch employee list and load to LV
//------------------------------------------------------------------------------
procedure TfrmEmployee.LoadEmployeesToLV;
var i: Integer;
    pEmp : PEmployeeInfo;
    ListItem: TListItem;
begin
  GetEmployees(fEmployeeList);

  // Clear LV before population
  LVEmpSettings.ClearSelection;
  LVEmpSettings.Clear;
  For i := 0 to fEmployeeList.Count - 1 do begin
    pEmp := PEmployeeInfo(fEmployeeList.Objects[i]);
    if pEmp <> Nil then begin
      ListItem := LVEmpSettings.Items.Add;
      ListItem.Data := TObject(pEmp);

      ListItem.Caption := pEmp^.EmployeeName;
      ListItem.SubItems.Add(pEmp^.IRDNum);
      ListItem.SubItems.Add(pEmp^.TaxCode);
      ListItem.SubItems.Add(FormatFloat('#.##', pEmp^.HourRate));
      ListItem.SubItems.Add(pEmp^.Occupation);
      ListItem.SubItems.Add(pEmp^.Credit);
      ListItem.SubItems.Add(FormatFloat('#.##', pEmp^.Hours));
      ListItem.SubItems.Add(BooleanToTF(pEmp^.HasKiwiSaver));
      ListItem.SubItems.Add(BooleanToTF(pEmp^.HasStudentLoan));
    end;
  end;
end;

// Add Employee dialog
//------------------------------------------------------------------------------
procedure TfrmEmployee.AddButtonClick(Sender: TObject);
begin
  ShowAddEmployee(fIRDList);
  LoadEmployeesToLV;
end;

// Update Employee dialog
//------------------------------------------------------------------------------
procedure TfrmEmployee.LVEmpSettingsDblClick(Sender: TObject);
var selItem : TListItem;
begin
  selItem := LVEmpSettings.Selected;
  if selItem = Nil then exit;

  ShowEditEmployee(fIRDList, selItem);
  LoadEmployeesToLV;
end;

//------------------------------------------------------------------------------
procedure TfrmEmployee.FormCreate(Sender: TObject);
begin
  eventEnabled := True;
end;

procedure TfrmEmployee.FormDestroy(Sender: TObject);
var
  I: Integer;
  pEmp: PEmployeeInfo;

begin
  for I := 0 to fEmployeeList.Count -1 do begin
    pEmp := PEmployeeInfo(fEmployeeList.Objects[I]);
    dispose(pEmp);
  end;

  fEmployeeList.Free;
  fIRDList.Free;
end;

end.
