unit EditLeaveTypeDlg;

interface

uses
//  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
//  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
//  AdoDBConx, PaySlipCommon;
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;


type
  TEditLeaveTypeForm = class(TForm)
    RelativePanel1: TRelativePanel;
    SaveButton: TButton;
    NameEdit: TEdit;
    Label1: TLabel;
    procedure SaveButtonClick(Sender: TObject);
  private
    { Private declarations }
    LeaveType: PLeaveTypeInfo;
    leaveID: integer;
  public
    { Public declarations }
  end;

var
  EditLeaveTypeForm: TEditLeaveTypeForm;

Procedure ShowEditLeaveTypes(Item: TListItem);

implementation

// Load dialog
//------------------------------------------------------------------------------
Procedure ShowEditLeaveTypes(Item: TListItem);
begin
  With TEditLeaveTypeForm.Create(Nil) do try    // Fetch and load dropdowns
    LeaveType := PLeaveTypeInfo(Item.Data);
    NameEdit.Text := LeaveType^.desc;
    ShowModal;
  finally
  end;
end;

{$R *.dfm}

procedure TEditLeaveTypeForm.SaveButtonClick(Sender: TObject);
var pLeaveType: PLeaveTypeInfo;
begin
  new(pLeaveType);
  pLeaveType^.id := LeaveType.id;
  pLeaveType^.desc := NameEdit.Text;
  UpdateLeaveType(pLeaveType);
  Close;
end;

end.
