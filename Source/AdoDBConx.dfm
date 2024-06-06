object frmADOCollections: TfrmADOCollections
  Left = 0
  Top = 0
  Caption = 'ADO Collections'
  ClientHeight = 186
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ADOConnection: TADOConnection
    LoginPrompt = False
    Provider = 'SQLOLEDB'
    Left = 48
    Top = 32
  end
  object ADOGetEmployees: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    SQL.Strings = (
      'Select '
      '  e.employeeID,'
      '  e.name,'
      '  e.irdNo,'
      '  e.taxCode,'
      '  e.hourRate,'
      '  e.occupation,'
      '  e.credit,'
      '  e.hours,'
      '  e.kiwiSaver,'
      '  e.studentLoan,'
      '  e.totalLeave,'
      '  e.startDate,'
      '  l.usedLeave'
      'From Employees e'
      'Left Join ('
      '  Select '
      '  employeeID,'
      '  SUM(durationDays) as usedLeave'
      '  From Leaves'
      '  Group By employeeID'
      '  ) l'
      'On e.employeeID = l.employeeID')
    Left = 48
    Top = 80
  end
  object spGenNew_Id: TADOStoredProc
    Connection = ADOConnection
    ProcedureName = 'PayslipGetNextId'
    Parameters = <>
    Left = 88
    Top = 32
  end
  object ADOAddEmployee: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'employeeID'
        Size = -1
        Value = Null
      end
      item
        Name = 'name'
        Size = -1
        Value = Null
      end
      item
        Name = 'irdNo'
        Size = -1
        Value = Null
      end
      item
        Name = 'taxCode'
        Size = -1
        Value = Null
      end
      item
        Name = 'hourRate'
        Size = -1
        Value = Null
      end
      item
        Name = 'occupation'
        Size = -1
        Value = Null
      end
      item
        Name = 'credit'
        Size = -1
        Value = Null
      end
      item
        Name = 'hours'
        Size = -1
        Value = Null
      end
      item
        Name = 'kiwiSaver'
        Size = -1
        Value = Null
      end
      item
        Name = 'studentLoan'
        Size = -1
        Value = Null
      end
      item
        Name = 'startDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'totalLeave'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO Employees'
      '('
      '  employeeID,'
      '  name,'
      '  irdNo,'
      '  taxCode,'
      '  hourRate,'
      '  occupation,'
      '  credit,'
      '  hours,'
      '  kiwiSaver,'
      '  studentLoan,'
      '  startDate,'
      '  totalLeave'
      ')'
      ''
      'VALUES'
      '('
      '  :employeeID,'
      '  :name,'
      '  :irdNo,'
      '  :taxCode,'
      '  :hourRate,'
      '  :occupation,'
      '  :credit,'
      '  :hours,'
      '  :kiwiSaver,'
      '  :studentLoan,'
      '  :startDate,'
      '  :totalLeave'
      ')')
    Left = 88
    Top = 80
  end
  object ADOUpdateEmployee: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'name'
        Size = -1
        Value = Null
      end
      item
        Name = 'irdNo'
        Size = -1
        Value = Null
      end
      item
        Name = 'taxCode'
        Size = -1
        Value = Null
      end
      item
        Name = 'hourRate'
        Size = -1
        Value = Null
      end
      item
        Name = 'occupation'
        Size = -1
        Value = Null
      end
      item
        Name = 'credit'
        Size = -1
        Value = Null
      end
      item
        Name = 'hours'
        Size = -1
        Value = Null
      end
      item
        Name = 'kiwiSaver'
        Size = -1
        Value = Null
      end
      item
        Name = 'studentLoan'
        Size = -1
        Value = Null
      end
      item
        Name = 'startDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'totalLeave'
        Size = -1
        Value = Null
      end
      item
        Name = 'employeeID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'UPDATE Employees'
      'SET  '
      '  name =  :name,'
      '  irdNo = :irdNo,'
      '  taxCode = :taxCode,'
      '  hourRate =  :hourRate,'
      '  occupation = :occupation,'
      '  credit = :credit,'
      '  hours = :hours,'
      '  kiwiSaver = :kiwiSaver,'
      '  studentLoan = :studentLoan,'
      '  startDate = :startDate,'
      '  totalLeave = :totalLeave'
      'Where employeeID  = :employeeID;')
    Left = 128
    Top = 80
  end
  object ADODeleteEmployee: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'employeeID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'DELETE FROM Employees'
      'WHERE employeeID = :employeeID')
    Left = 168
    Top = 80
  end
  object ADOGetLeaves: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    SQL.Strings = (
      'Select '
      '  Leaves.leaveId, '
      '  Leaves.startDate, '
      '  Leaves.endDate, '
      '  Leaves.type, '
      '  Leaves.durationDays, '
      '  Leaves.employeeID,'
      '  name'
      'From Leaves'
      'Left Join Employees'
      'On Leaves.employeeID = Employees.employeeID')
    Left = 48
    Top = 120
  end
  object ADOAddLeave: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'leaveID'
        Size = -1
        Value = Null
      end
      item
        Name = 'startDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'endDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'type'
        Size = -1
        Value = Null
      end
      item
        Name = 'durationDays'
        Size = -1
        Value = Null
      end
      item
        Name = 'employeeID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO Leaves'
      '('
      '  leaveID,'
      '  startDate,'
      '  endDate,'
      '  type,'
      '  durationDays,'
      '  employeeID'
      ')'
      ''
      'VALUES'
      '('
      '  :leaveID,'
      '  :startDate,'
      '  :endDate,'
      '  :type,'
      '  :durationDays,'
      '  :employeeID'
      ')')
    Left = 88
    Top = 120
  end
  object ADOUpdateLeave: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'startDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'endDate'
        Size = -1
        Value = Null
      end
      item
        Name = 'type'
        Size = -1
        Value = Null
      end
      item
        Name = 'durationDays'
        Size = -1
        Value = Null
      end
      item
        Name = 'employeeID'
        Size = -1
        Value = Null
      end
      item
        Name = 'leaveID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'UPDATE Leaves'
      'SET  '
      '  startDate =  :startDate,'
      '  endDate = :endDate ,'
      '  type = :type,'
      '  durationDays =  :durationDays,'
      '  employeeID = :employeeID'
      'Where leaveID  = :leaveID;'
      '')
    Left = 128
    Top = 120
  end
  object ADOGetLeaveTypes: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    SQL.Strings = (
      'Select * '
      'From LeaveTypes;')
    Left = 264
    Top = 80
  end
  object ADOAddLeaveTypes: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'leaveTypeID'
        Size = -1
        Value = Null
      end
      item
        Name = 'description'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO LeaveTypes'
      '('
      '  leaveTypeID,'
      '  description'
      ')'
      ''
      'VALUES'
      '('
      '  :leaveTypeID,'
      '  :description'
      ')')
    Left = 304
    Top = 80
  end
  object ADOUpdateLeaveTypes: TADOQuery
    Connection = ADOConnection
    Parameters = <
      item
        Name = 'description'
        Size = -1
        Value = Null
      end
      item
        Name = 'leaveTypeID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'UPDATE LeaveTypes'
      'SET  '
      '  description =  :description'
      'Where leaveTypeID  = :leaveTypeID;'
      '')
    Left = 344
    Top = 80
  end
end
