object AddLeaveForm: TAddLeaveForm
  Left = 0
  Top = 0
  Caption = 'Add Leave'
  ClientHeight = 230
  ClientWidth = 400
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
    Width = 400
    Height = 230
    ControlCollection = <
      item
        Control = EmployeeSelect
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = True
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = True
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = EmployeeLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = EmployeeSelect
        AlignVerticalCenterWithPanel = False
        LeftOf = EmployeeSelect
      end
      item
        Control = LeaveSelect
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = EmployeeSelect
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = EmployeeSelect
      end
      item
        Control = LeaveLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = LeaveSelect
        AlignVerticalCenterWithPanel = False
        LeftOf = LeaveSelect
      end
      item
        Control = StartDate
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = LeaveSelect
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = LeaveSelect
      end
      item
        Control = StartDateLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = StartDate
        AlignVerticalCenterWithPanel = False
        LeftOf = StartDate
      end
      item
        Control = EndDate
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = True
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = StartDate
      end
      item
        Control = EndDateLbl
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = EndDate
        AlignVerticalCenterWithPanel = False
        LeftOf = EndDate
      end
      item
        Control = DurationEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWith = EndDate
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
        Below = EndDate
      end
      item
        Control = Label1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = DurationEdit
        AlignVerticalCenterWithPanel = False
        LeftOf = DurationEdit
      end
      item
        Control = AddButton
        AlignBottomWithPanel = True
        AlignHorizontalCenterWithPanel = True
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
      400
      230)
    object EmployeeSelect: TComboBox
      AlignWithMargins = True
      Left = 126
      Top = 24
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 0
    end
    object EmployeeLbl: TLabel
      AlignWithMargins = True
      Left = 67
      Top = 28
      Width = 46
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Employee'
    end
    object LeaveSelect: TComboBox
      AlignWithMargins = True
      Left = 126
      Top = 51
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 1
    end
    object LeaveLbl: TLabel
      AlignWithMargins = True
      Left = 57
      Top = 55
      Width = 56
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Leave Type'
    end
    object StartDate: TDateTimePicker
      AlignWithMargins = True
      Left = 126
      Top = 78
      Width = 145
      Height = 21
      Anchors = []
      Date = 45214.000000000000000000
      Time = 0.688268935184169100
      TabOrder = 2
    end
    object StartDateLbl: TLabel
      AlignWithMargins = True
      Left = 63
      Top = 82
      Width = 50
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Start Date'
    end
    object EndDate: TDateTimePicker
      AlignWithMargins = True
      Left = 126
      Top = 105
      Width = 145
      Height = 21
      Anchors = []
      Date = 45214.000000000000000000
      Time = 0.689400370371004100
      TabOrder = 3
    end
    object EndDateLbl: TLabel
      AlignWithMargins = True
      Left = 69
      Top = 109
      Width = 44
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'End Date'
    end
    object DurationEdit: TEdit
      AlignWithMargins = True
      Left = 126
      Top = 132
      Width = 145
      Height = 21
      Anchors = []
      TabOrder = 4
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 35
      Top = 136
      Width = 78
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Duration in days'
    end
    object AddButton: TButton
      Left = 161
      Top = 184
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Add'
      TabOrder = 5
      OnClick = AddButtonClick
    end
  end
end
