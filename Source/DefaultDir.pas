unit DefaultDir;

                                     interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, Buttons;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type
  TfrmDefDirectory = class(TForm)
    Label1: TLabel;
    edDefDir: TEdit;
    btnOpenfile: TSpeedButton;
    Button1: TButton;
    Button2: TButton;
    procedure btnOpenfileClick(Sender: TObject);
  private
    CurDefDir : String;
  public
    { Public declarations }
  end;

function SetDefDirectory(var DefDir : String) : boolean;

                          implementation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses FileCtrl, PaySlipCommon;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$R *.dfm}



//------------------------------------------------------------------------------
function SetDefDirectory(var DefDir : String) : boolean;
var suc : boolean;
    frmDefDirectory: TfrmDefDirectory;
begin
  frmDefDirectory := TfrmDefDirectory.Create(Nil);
  With frmDefDirectory do try
    edDefDir.Text := DefDir;
    CurDefDir := DefDir;
    suc := ShowModal = mrOK;
    if suc then begin
      WriteDefDirToIniFile(CurDefDir);
      defDir := CurDefDir;
    end;

    result := suc;
  finally
    frmDefDirectory.Free;
  end;

end;


//------------------------------------------------------------------------------
procedure TfrmDefDirectory.btnOpenfileClick(Sender: TObject);
var suc : boolean;
    chosenDir : String;
begin
  suc := SelectDirectory('Select a dirctory', '', chosenDir);
  if not suc then exit;

  CurDefDir := chosenDir;
  edDefDir.Text := chosenDir;
end;

end.
