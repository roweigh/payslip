unit TaxRate;

                                         interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
     ExtCtrls, ComCtrls, StdCtrls, PaySlipCommon;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type
  TfrmTaxRate = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    btnCancel: TButton;
    btnApply: TButton;
    gbIncomeTax: TGroupBox;
    gbStudentLoan: TGroupBox;
    gbOther: TGroupBox;
    Label1: TLabel;
    lblBracket: TLabel;
    lblRate: TLabel;
    edtRate1: TEdit;
    edtBracket1: TEdit;
    edtBracket2: TEdit;
    Label2: TLabel;
    edtRate2: TEdit;
    edtRate3: TEdit;
    edtBracket3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtBracket4: TEdit;
    edtRate4: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label8: TLabel;
    Label5: TLabel;
    edtSLPerc: TEdit;
    Label11: TLabel;
    Label6: TLabel;
    edtACCLevy: TEdit;
    Label12: TLabel;
    Label7: TLabel;
    lblSL: TLabel;
    edtSL: TEdit;
    edtKSPerc: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    edtRate5: TEdit;
    Label15: TLabel;
    EdtBracket5: TEdit;
  private
    procedure GetRatesInfo(var TaxRates: TTaxInfo);
    Procedure DisplayTaxRates(var TaxRatesDet: TTaxInfo);
    { Private declarations }
  public
    { Public declarations }
    myFile: Textfile;
    settingsNameList: TStringList;
    ListItem: TListItem;
  end;

Function ShowTaxRate(var TaxDetails : TTaxInfo) : boolean;

                              implementation
//--------------------------------------------------------
//uses ;
//--------------------------------------------------------

{$R *.dfm}


//------------------------------------------------------------------------------
Function ShowTaxRate(var TaxDetails : TTaxInfo) : boolean;
var Suc : Boolean;
begin
  Result := False;
  With TfrmTaxRate.Create(Nil) do try
    DisplayTaxRates(TaxDetails);
    suc := ShowModal = mrOK;
    if suc then begin
      GetRatesInfo(TaxDetails);
      WriteTaxInfoToIniFile(TaxDetails);
      Result := True;
    end;
  finally
    Free;
  end;

end;


//------------------------------------------------------------------------------
procedure TfrmTaxRate.GetRatesInfo(var TaxRates : TTaxInfo);
begin
  TaxRates.Thresholds[1] := StrToIntDef(edtBracket1.Text);
  TaxRates.Thresholds[2] := StrToIntDef(edtBracket2.Text);
  TaxRates.Thresholds[3] := StrToIntDef(edtBracket3.Text);
  TaxRates.Thresholds[4] := StrToIntDef(edtBracket4.Text);

  TaxRates.IrdTaxRates[0]   := StrToDoubleDef(edtRate1.Text) / 100;
  TaxRates.IrdTaxRates[1]   := StrToDoubleDef(edtRate2.Text) / 100;
  TaxRates.IrdTaxRates[2]   := StrToDoubleDef(edtRate3.Text) / 100;
  TaxRates.IrdTaxRates[3]   := StrToDoubleDef(edtRate4.Text) / 100;
  TaxRates.IrdTaxRates[4]   := StrToDoubleDef(edtRate5.Text) / 100;

  TaxRates.SLoanThreshold := StrToIntDef(edtSL.Text);
  TaxRates.SLoanRate := StrToDoubleDef(edtSLPerc.Text)/ 100;

  TaxRates.KiwiSaverRate := StrToDoubleDef(edtKSPerc.Text)/ 100;
  TaxRates.AccRate := StrToDoubleDef(edtACCLevy.Text)/ 100;
end;

//------------------------------------------------------------------------------
Procedure TfrmTaxRate.DisplayTaxRates(var TaxRatesDet : TTaxInfo);
begin
  edtBracket1.Text := IntToStr(TaxRatesDet.Thresholds[1]);
  edtBracket2.Text := IntToStr(TaxRatesDet.Thresholds[2]);
  edtBracket3.Text := IntToStr(TaxRatesDet.Thresholds[3]);
  edtBracket4.Text := IntToStr(TaxRatesDet.Thresholds[4]);
  EdtBracket5.Text := IntToStr(TaxRatesDet.Thresholds[4]);

  edtRate1.Text := FormatFloat('#.##', TaxRatesDet.IrdTaxRates[0] * 100);
  edtRate2.Text := FormatFloat('#.##', TaxRatesDet.IrdTaxRates[1] * 100);
  edtRate3.Text := FormatFloat('#.##', TaxRatesDet.IrdTaxRates[2] * 100);
  edtRate4.Text := FormatFloat('#.##', TaxRatesDet.IrdTaxRates[3] * 100);
  edtRate5.Text := FormatFloat('#.##', TaxRatesDet.IrdTaxRates[4] * 100);

  edtSL.Text := IntToStr(TaxRatesDet.SLoanThreshold);
  edtSLPerc.Text := FormatFloat('#.##', TaxRatesDet.SLoanRate * 100);

  edtKSPerc.Text := FormatFloat('#.##', TaxRatesDet.KiwiSaverRate * 100);
  edtACCLevy.Text := FormatFloat('#.##', TaxRatesDet.AccRate * 100);
end;


end.
