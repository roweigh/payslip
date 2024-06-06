unit LeaveInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdoDBConx, PaySlipCommon, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AddLeaveDlg, EditLeaveDlg;

type
  TfrmLeaves = class(TForm)
    RelativePanel1: TRelativePanel;
    AddButton: TButton;
    LVLeaveList: TListView;
    function GetLeaveTypeName(LeaveType: Integer): String;
    Procedure LoadLeavesToLV;
    procedure LVLeaveListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fLeaveList: TStringList;
    fLeaveTypeList: TStringList;
  public
    { Public declarations }
  end;

Procedure ShowLeaves;

implementation

{$R *.dfm}

// Setup
//------------------------------------------------------------------------------
Procedure ShowLeaves;
begin
  With TfrmLeaves.Create(Nil) do try
    fLeaveList := TStringList.Create();
    fLeaveTypeList := TStringList.Create();
    LoadLeavesToLV;
    ShowModal;
  finally
  end;
end;

// Fetch leave list and load to LV
//------------------------------------------------------------------------------
procedure TfrmLeaves.LoadLeavesToLV;
var i: Integer;
    pLeave : PLeaveInfo;
    LstItem: TListItem;
begin
  // Clear LV before population
  LVLeaveList.ClearSelection;
  LVLeaveList.Clear;

  GetLeaves(fLeaveList);
  GetLeaveTypes(fLeaveTypeList);
  For I := 0 to fLeaveList.Count - 1 do begin
    pLeave := PLeaveInfo(fLeaveList.Objects[i]);
    if pLeave <> Nil then begin
      LstItem := LVLeaveList.Items.Add;
      LstItem.Data := TObject(PLeave);
      LstItem.Caption := IntToStr(PLeave^.LeaveID);
      LstItem.SubItems.Add(DateTimeToStr(PLeave^.StartDate));
      LstItem.SubItems.Add(DateTimeToStr(PLeave^.EndDate));
      LstItem.SubItems.Add(IntToStr(PLeave^.DurationDays));
      LstItem.SubItems.Add(GetLeaveTypeName(PLeave^.LeaveType));
      LstItem.SubItems.Add(IntToStr(PLeave^.EmployeeID));
      LstItem.SubItems.Add(PLeave^.EmployeeName);
    end;
  end;
end;

// Convert index value to description
//------------------------------------------------------------------------------
function TfrmLeaves.GetLeaveTypeName(LeaveType: Integer): String;
var i: Integer;
begin
  i := fLeaveTypeList.IndexOf(LeaveType.ToString());
  result := PLeaveTypeInfo(fLeaveTypeList.Objects[i])^.desc;
end;


// Add
//------------------------------------------------------------------------------
procedure TfrmLeaves.AddButtonClick(Sender: TObject);
begin
  ShowAddLeave;
  LoadLeavesToLV;
end;

// Load leave data to inputs if selected from list view
//------------------------------------------------------------------------------
procedure TfrmLeaves.LVLeaveListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if LVLeaveList.Selected <> Nil then begin
    ShowEditLeave(Item);
  end;

  LoadLeavesToLV;
end;

//------------------------------------------------------------------------------
procedure TfrmLeaves.FormDestroy(Sender: TObject);
begin
  fLeaveList.Free;
end;

end.
