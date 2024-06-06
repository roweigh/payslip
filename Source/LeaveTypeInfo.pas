unit LeaveTypeInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon, AddLeaveTypeDlg, EditLeaveTypeDlg;

type
  TfrmLeaveTypes = class(TForm)
    RelativePanel1: TRelativePanel;
    AddButton: TButton;
    LVLeaveTypeList: TListView;
    Procedure LoadLeaveTypesToLV;
    procedure FormDestroy(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure LVLeaveTypeListDblClick(Sender: TObject);
  private
    { Private declarations }
    fLeaveTypeList: TStringList;
  public
    { Public declarations }
  end;

Procedure ShowLeaveTypes;

var
  frmLeaveTypes: TfrmLeaveTypes;

implementation

{$R *.dfm}

// Setup
//------------------------------------------------------------------------------
Procedure ShowLeaveTypes;
begin
  With TfrmLeaveTypes.Create(Nil) do try
    fLeaveTypeList := TStringList.Create();
    LoadLeaveTypesToLV;
    ShowModal;
  finally
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmLeaveTypes.LoadLeaveTypesToLV;
var i: Integer;
    pLeaveType: PLeaveTypeInfo;
    LstItem: TListItem;
begin
  // Clear LV before population
  LVLeaveTypeList.ClearSelection;
  LVLeaveTypeList.Clear;

  GetLeaveTypes(fLeaveTypeList);
  For I := 0 to fLeaveTypeList.Count - 1 do begin
    pLeaveType := PLeaveTypeInfo(fLeaveTypeList.Objects[i]);
    if pLeaveType <> Nil then begin
      LstItem := LVLeaveTypeList.Items.Add;

      LstItem.Data := TObject(pLeaveType);
      LstItem.Caption := pLeaveType^.desc
//      LstItem.SubItems.Add(pLeaveType^.desc);              W
    end;


  end;
end;

procedure TfrmLeaveTypes.LVLeaveTypeListDblClick(Sender: TObject);
  var selItem : TListItem;
begin
  selItem := LVLeaveTypeList.Selected;
  if selItem = Nil then exit;

  ShowEditLeaveTypes(selItem);
  LoadLeaveTypesToLV;
end;

//------------------------------------------------------------------------------
procedure TfrmLeaveTypes.AddButtonClick(Sender: TObject);
begin
  ShowAddLeaveType;
  LoadLeaveTypesToLV;
end;

procedure TfrmLeaveTypes.FormDestroy(Sender: TObject);
begin
  fLeaveTypeList.Free;
end;

end.
