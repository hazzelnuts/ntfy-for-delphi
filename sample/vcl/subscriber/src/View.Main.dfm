object ViewMain: TViewMain
  Left = 0
  Top = 0
  Caption = 'Subscriber'
  ClientHeight = 282
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnSubscribe: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Subscribe'
    TabOrder = 0
    OnClick = BtnSubscribeClick
  end
  object BtnUnsubscribe: TButton
    Left = 97
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Unsubscribe'
    Enabled = False
    TabOrder = 1
    OnClick = BtnUnsubscribeClick
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 47
    Width = 577
    Height = 235
    Align = alBottom
    DataSource = DsTable
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 67
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TITLE'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MSG'
        Width = 318
        Visible = True
      end>
  end
  object TableNotification: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 424
    Top = 8
    object TableNotificationID: TStringField
      FieldName = 'ID'
      Size = 30
    end
    object TableNotificationTITLE: TStringField
      FieldName = 'TITLE'
      Size = 50
    end
    object TableNotificationMSG: TStringField
      FieldName = 'MSG'
      Size = 60
    end
  end
  object DsTable: TDataSource
    DataSet = TableNotification
    Left = 472
    Top = 8
  end
end
