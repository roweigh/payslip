object AddLeaveTypeForm: TAddLeaveTypeForm
  Left = 0
  Top = 0
  Caption = 'Add Leave Type'
  ClientHeight = 111
  ClientWidth = 400
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
    Width = 400
    Height = 111
    ControlCollection = <
      item
        Control = AddButton
        AlignBottomWithPanel = True
        AlignHorizontalCenterWithPanel = True
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = NameEdit
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = True
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = Label1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWith = NameEdit
        AlignVerticalCenterWithPanel = False
        LeftOf = NameEdit
      end>
    Align = alClient
    Padding.Top = 20
    Padding.Bottom = 20
    TabOrder = 0
    DesignSize = (
      400
      111)
    object AddButton: TButton
      AlignWithMargins = True
      Left = 161
      Top = 62
      Width = 75
      Height = 25
      Margins.Top = 10
      Anchors = []
      Caption = 'Add'
      TabOrder = 0
      OnClick = AddButtonClick
    end
    object NameEdit: TEdit
      Left = 152
      Top = 21
      Width = 121
      Height = 21
      Anchors = []
      TabOrder = 1
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 115
      Top = 25
      Width = 27
      Height = 13
      Margins.Right = 10
      Anchors = []
      Caption = 'Name'
    end
  end
end
