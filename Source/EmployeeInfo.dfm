object frmEmployee: TfrmEmployee
  Left = 578
  Top = 125
  BorderStyle = bsDialog
  Caption = 'Employee Settings'
  ClientHeight = 577
  ClientWidth = 863
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LVEmpSettings: TListView
    Left = 0
    Top = 41
    Width = 863
    Height = 536
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'IRD No.'
        Width = 120
      end
      item
        Caption = 'Tax Code'
        Width = 70
      end
      item
        Caption = 'Hour Rate'
        Width = 70
      end
      item
        Caption = 'Occupation'
        Width = 100
      end
      item
        Caption = 'Credit'
        Width = 60
      end
      item
        Caption = 'Fortnightly Hours'
        Width = 100
      end
      item
        Caption = 'Kiwi Saver'
        Width = 70
      end
      item
        Caption = 'Student Loan'
        Width = 90
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = LVEmpSettingsDblClick
  end
  object RelativePanel1: TRelativePanel
    Left = 0
    Top = 0
    Width = 863
    Height = 41
    ControlCollection = <
      item
        Control = AddButton
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end>
    Align = alTop
    TabOrder = 1
    DesignSize = (
      863
      41)
    object AddButton: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Add Employee'
      TabOrder = 0
      OnClick = AddButtonClick
    end
  end
end
