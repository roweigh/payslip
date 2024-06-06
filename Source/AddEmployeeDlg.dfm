object AddEmployeeForm: TAddEmployeeForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Employee'
  ClientHeight = 328
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object RelativePanel1: TRelativePanel
    Left = 0
    Top = 0
    Width = 555
    Height = 328
    ControlCollection = <
      item
        Control = NameLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = IrdLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = TaxCodeLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = HourRateLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = HoursLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = EmployeeNameEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = True
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = True
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = IrdNumberEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = EmployeeNameEdit
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = EmployeeNameEdit
      end
      item
        Control = TaxCodeEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = EmployeeNameEdit
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = IrdNumberEdit
      end
      item
        Control = HourRateEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = TaxCodeEdit
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = TaxCodeEdit
      end
      item
        Control = OccupationEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = HoursEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = HourRateEdit
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = HourRateEdit
      end
      item
        Control = OccupationLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = CreditEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = CreditLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = StartDate
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = StartDateLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = RadioKS
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = RadioSL
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = SaveButton
        AlignBottomWithPanel = True
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end>
    Align = alClient
    Padding.Top = 20
    Padding.Bottom = 20
    TabOrder = 0
    DesignSize = (
      555
      328)
    object NameLbl: TLabel
      AlignWithMargins = True
      Left = 164
      Top = 28
      Width = 27
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Name'
    end
    object IrdLbl: TLabel
      AlignWithMargins = True
      Left = 133
      Top = 55
      Width = 58
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'IRD Number'
    end
    object TaxCodeLbl: TLabel
      AlignWithMargins = True
      Left = 145
      Top = 82
      Width = 46
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Tax Code'
    end
    object HourRateLbl: TLabel
      AlignWithMargins = True
      Left = 142
      Top = 109
      Width = 49
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Hour Rate'
    end
    object HoursLbl: TLabel
      AlignWithMargins = True
      Left = 163
      Top = 136
      Width = 28
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Hours'
    end
    object EmployeeNameEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 24
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 0
    end
    object IrdNumberEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 51
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 1
    end
    object TaxCodeEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 78
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 2
    end
    object HourRateEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 105
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 3
    end
    object OccupationEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 159
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 4
    end
    object HoursEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 132
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 5
    end
    object OccupationLbl: TLabel
      AlignWithMargins = True
      Left = 137
      Top = 163
      Width = 54
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Occupation'
    end
    object CreditEdit: TEdit
      AlignWithMargins = True
      Left = 204
      Top = 186
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 6
    end
    object CreditLbl: TLabel
      AlignWithMargins = True
      Left = 162
      Top = 190
      Width = 29
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Credit'
    end
    object StartDate: TDateTimePicker
      AlignWithMargins = True
      Left = 204
      Top = 213
      Width = 145
      Height = 21
      Anchors = []
      Date = 45214.000000000000000000
      Time = 0.688268935184169100
      TabOrder = 7
    end
    object StartDateLbl: TLabel
      AlignWithMargins = True
      Left = 141
      Top = 217
      Width = 50
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Start Date'
    end
    object RadioKS: TRadioGroup
      AlignWithMargins = True
      Left = 367
      Top = 24
      Width = 121
      Height = 56
      Margins.Left = 20
      Anchors = []
      Caption = 'KiwiSaver'
      Items.Strings = (
        'Yes'
        'No')
      TabOrder = 8
    end
    object RadioSL: TRadioGroup
      AlignWithMargins = True
      Left = 367
      Top = 86
      Width = 121
      Height = 56
      Anchors = []
      Caption = 'Student Loan'
      Items.Strings = (
        'Yes'
        'No')
      TabOrder = 9
    end
    object SaveButton: TButton
      AlignWithMargins = True
      Left = 239
      Top = 279
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Save'
      TabOrder = 10
      OnClick = SaveButtonClick
    end
  end
end
