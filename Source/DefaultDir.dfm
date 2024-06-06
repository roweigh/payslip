object frmDefDirectory: TfrmDefDirectory
  Left = 767
  Top = 223
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Default Directory'
  ClientHeight = 191
  ClientWidth = 482
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
  object Label1: TLabel
    Left = 32
    Top = 32
    Width = 159
    Height = 13
    Caption = 'Payslip PDF file default directory:'
  end
  object btnOpenfile: TSpeedButton
    Left = 390
    Top = 50
    Width = 58
    Height = 23
    Caption = 'Browse...'
    OnClick = btnOpenfileClick
  end
  object edDefDir: TEdit
    Left = 43
    Top = 51
    Width = 337
    Height = 21
    ReadOnly = True
    TabOrder = 2
    Text = 'c:\'
  end
  object Button1: TButton
    Left = 287
    Top = 128
    Width = 74
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 373
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
