object frmLeaveTypes: TfrmLeaveTypes
  Left = 0
  Top = 0
  Caption = 'Leave Types'
  ClientHeight = 361
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RelativePanel1: TRelativePanel
    Left = 0
    Top = 0
    Width = 484
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
    ExplicitLeft = -382
    ExplicitWidth = 800
    DesignSize = (
      484
      41)
    object AddButton: TButton
      Left = 16
      Top = 10
      Width = 100
      Height = 25
      Anchors = []
      Caption = 'Add Leave Type'
      TabOrder = 0
      OnClick = AddButtonClick
    end
  end
  object LVLeaveTypeList: TListView
    Left = 0
    Top = 41
    Width = 484
    Height = 320
    Align = alClient
    Columns = <
      item
        Caption = 'Leave Type'
        Width = 480
      end>
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = LVLeaveTypeListDblClick
  end
end
