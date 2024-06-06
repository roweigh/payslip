object frmLeaves: TfrmLeaves
  Left = 0
  Top = 0
  Caption = 'Leave Balances'
  ClientHeight = 445
  ClientWidth = 800
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
    Width = 800
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
    TabOrder = 0
    DesignSize = (
      800
      41)
    object AddButton: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Add Leave'
      TabOrder = 0
      OnClick = AddButtonClick
    end
  end
  object LVLeaveList: TListView
    Left = 0
    Top = 41
    Width = 800
    Height = 404
    Align = alClient
    Columns = <
      item
        Caption = 'LeaveID'
        Width = 55
      end
      item
        Caption = 'Start Date'
        Width = 145
      end
      item
        Caption = 'End Date'
        Width = 145
      end
      item
        Caption = 'Duration (Days)'
        Width = 90
      end
      item
        Caption = 'Leave Type'
        Width = 135
      end
      item
        Caption = 'Employee ID'
        Width = 75
      end
      item
        Caption = 'Employee Name'
        Width = 150
      end>
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = LVLeaveListSelectItem
  end
end
