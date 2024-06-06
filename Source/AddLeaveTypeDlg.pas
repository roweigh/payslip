unit AddLeaveTypeDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  AdoDBConx, PaySlipCommon;

type
  TAddLeaveTypeForm = class(TForm)
    RelativePanel1: TRelativePanel;
    AddButton: TButton;
    NameEdit: TEdit;
    Label1: TLabel;
    procedure AddButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Procedure ShowAddLeaveType;

var
  AddLeaveTypeForm: TAddLeaveTypeForm;

implementation

{$R *.dfm}

// Load dialog
//------------------------------------------------------------------------------
Procedure ShowAddLeaveType;
begin
  With TAddLeaveTypeForm.Create(Nil) do try    // Fetch and load dropdowns
    ShowModal;
  finally
    //
  end;
end;

procedure TAddLeaveTypeForm.AddButtonClick(Sender: TObject);
var pLeaveType: PLeaveTypeInfo;
begin
  new(pLeaveType);
  pLeaveType^.desc := NameEdit.Text;
  AddLeaveType(pLeaveType);
  Close;
end;

end.
