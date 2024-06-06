object frmTaxRate: TfrmTaxRate
  Left = 565
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Tax Rate Settings'
  ClientHeight = 553
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 553
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 498
      Width = 617
      Height = 55
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object btnCancel: TButton
        Left = 458
        Top = 7
        Width = 75
        Height = 25
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object btnApply: TButton
        Left = 377
        Top = 7
        Width = 75
        Height = 25
        Caption = '&Apply'
        ModalResult = 1
        TabOrder = 1
      end
    end
    object gbIncomeTax: TGroupBox
      Left = 49
      Top = 21
      Width = 521
      Height = 217
      Caption = 'Income Tax'
      TabOrder = 1
      object Label1: TLabel
        Left = 34
        Top = 55
        Width = 131
        Height = 13
        Caption = 'Annual income from 0 up to'
      end
      object lblBracket: TLabel
        Left = 176
        Top = 33
        Width = 68
        Height = 13
        Caption = 'Tax Threshold'
      end
      object lblRate: TLabel
        Left = 336
        Top = 33
        Width = 23
        Height = 13
        Caption = 'Rate'
      end
      object Label2: TLabel
        Left = 140
        Top = 82
        Width = 25
        Height = 13
        Caption = 'up to'
      end
      object Label3: TLabel
        Left = 140
        Top = 106
        Width = 25
        Height = 13
        Caption = 'up to'
      end
      object Label4: TLabel
        Left = 140
        Top = 136
        Width = 25
        Height = 13
        Caption = 'up to'
      end
      object Label9: TLabel
        Left = 420
        Top = 136
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label10: TLabel
        Left = 420
        Top = 110
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label8: TLabel
        Left = 420
        Top = 84
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label5: TLabel
        Left = 420
        Top = 55
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label14: TLabel
        Left = 57
        Top = 163
        Width = 113
        Height = 13
        Alignment = taRightJustify
        Caption = 'Remaining income over '
      end
      object Label15: TLabel
        Left = 420
        Top = 164
        Width = 11
        Height = 13
        Caption = '%'
      end
      object edtRate1: TEdit
        Left = 336
        Top = 52
        Width = 78
        Height = 21
        TabOrder = 0
      end
      object edtBracket1: TEdit
        Left = 176
        Top = 52
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object edtBracket2: TEdit
        Left = 176
        Top = 79
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object edtRate2: TEdit
        Left = 336
        Top = 79
        Width = 78
        Height = 21
        TabOrder = 3
      end
      object edtRate3: TEdit
        Left = 336
        Top = 106
        Width = 78
        Height = 21
        TabOrder = 4
      end
      object edtBracket3: TEdit
        Left = 176
        Top = 106
        Width = 121
        Height = 21
        TabOrder = 5
      end
      object edtBracket4: TEdit
        Left = 176
        Top = 133
        Width = 121
        Height = 21
        TabOrder = 6
      end
      object edtRate4: TEdit
        Left = 336
        Top = 133
        Width = 78
        Height = 21
        TabOrder = 7
      end
      object edtRate5: TEdit
        Left = 336
        Top = 160
        Width = 78
        Height = 21
        TabOrder = 8
      end
      object EdtBracket5: TEdit
        Left = 176
        Top = 160
        Width = 121
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 9
      end
    end
    object gbStudentLoan: TGroupBox
      Left = 49
      Top = 244
      Width = 521
      Height = 74
      Caption = 'Student Loan'
      TabOrder = 2
      object lblSL: TLabel
        Left = 51
        Top = 32
        Width = 114
        Height = 13
        Caption = 'Student Loan Threshold'
      end
      object Label13: TLabel
        Left = 420
        Top = 32
        Width = 11
        Height = 13
        Caption = '%'
      end
      object edtSL: TEdit
        Left = 176
        Top = 28
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtKSPerc: TEdit
        Left = 334
        Top = 28
        Width = 78
        Height = 21
        TabOrder = 1
      end
    end
    object gbOther: TGroupBox
      Left = 49
      Top = 340
      Width = 521
      Height = 125
      Caption = 'ACC and Kiwi Saver'
      TabOrder = 3
      object Label11: TLabel
        Left = 420
        Top = 83
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label6: TLabel
        Left = 287
        Top = 83
        Width = 44
        Height = 13
        Caption = 'ACCLevy'
      end
      object Label12: TLabel
        Left = 420
        Top = 40
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label7: TLabel
        Left = 224
        Top = 40
        Width = 107
        Height = 13
        Caption = 'Kiwi Saver Percentage'
      end
      object edtSLPerc: TEdit
        Left = 337
        Top = 36
        Width = 78
        Height = 21
        TabOrder = 0
      end
      object edtACCLevy: TEdit
        Left = 337
        Top = 79
        Width = 78
        Height = 21
        TabOrder = 1
      end
    end
  end
end
