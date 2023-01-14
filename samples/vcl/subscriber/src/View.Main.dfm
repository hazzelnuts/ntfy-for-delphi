object ViewMain: TViewMain
  Left = 960
  Top = 256
  Caption = 'Ntfy Subscriber'
  ClientHeight = 527
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    724
    527)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTopic: TLabel
    Left = 445
    Top = 13
    Width = 260
    Height = 13
    Caption = 'Topics - Break a line for each. Don'#39't leave empty lines.'
  end
  object BtnSubscribe: TButton
    Left = 16
    Top = 427
    Width = 132
    Height = 25
    Caption = 'Subscribe'
    TabOrder = 0
    OnClick = BtnSubscribeClick
  end
  object BtnUnsubscribe: TButton
    Left = 16
    Top = 458
    Width = 132
    Height = 25
    Caption = 'Unsubscribe'
    Enabled = False
    TabOrder = 1
    OnClick = BtnUnsubscribeClick
  end
  object DBGrid1: TDBGrid
    Left = 154
    Top = 138
    Width = 551
    Height = 376
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DsTable
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
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
        Width = 50
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TIME'
        Width = 118
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PRIORITY'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TITLE'
        Width = 146
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MSG'
        Width = 700
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TOPIC'
        Width = 300
        Visible = True
      end>
  end
  object CkPoll: TCheckBox
    Left = 16
    Top = 137
    Width = 41
    Height = 17
    Caption = 'Poll'
    TabOrder = 3
  end
  object GbSince: TRadioGroup
    Left = 16
    Top = 17
    Width = 89
    Height = 71
    Caption = 'Since'
    Enabled = False
    ItemIndex = 2
    Items.Strings = (
      'Message Id'
      'Duration'
      'Unix Time')
    TabOrder = 4
    OnClick = GbSinceClick
  end
  object CkScheduled: TCheckBox
    Left = 16
    Top = 155
    Width = 132
    Height = 17
    Caption = 'Fectch scheduled only'
    TabOrder = 5
  end
  object CkSince: TCheckBox
    Left = 16
    Top = 173
    Width = 132
    Height = 17
    Caption = 'Fetch cached messages'
    TabOrder = 6
    OnClick = CkSinceClick
  end
  object GbFilters: TGroupBox
    Left = 16
    Top = 196
    Width = 132
    Height = 225
    Caption = 'Filters'
    TabOrder = 7
    object lbPriority: TLabel
      Left = 15
      Top = 177
      Width = 34
      Height = 13
      Caption = 'Priority'
    end
    object lbeIdFilter: TLabeledEdit
      Left = 15
      Top = 31
      Width = 104
      Height = 21
      EditLabel.Width = 53
      EditLabel.Height = 13
      EditLabel.Caption = 'Message id'
      TabOrder = 0
      TextHint = 'Id'
    end
    object lbeFilterTitle: TLabeledEdit
      Left = 15
      Top = 71
      Width = 104
      Height = 21
      EditLabel.Width = 65
      EditLabel.Height = 13
      EditLabel.Caption = 'Message Title'
      TabOrder = 1
      TextHint = 'Title'
    end
    object lbeFilterMessage: TLabeledEdit
      Left = 15
      Top = 111
      Width = 104
      Height = 21
      EditLabel.Width = 67
      EditLabel.Height = 13
      EditLabel.Caption = 'Message Text'
      TabOrder = 2
      TextHint = 'Message'
    end
    object CbFilterPriority: TComboBox
      Left = 15
      Top = 196
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Items.Strings = (
        ''
        'Min'
        'Low'
        'Default'
        'High'
        'Maximum')
    end
    object lbeFilterTags: TLabeledEdit
      Left = 15
      Top = 150
      Width = 104
      Height = 21
      EditLabel.Width = 68
      EditLabel.Height = 13
      EditLabel.Caption = 'Message Tags'
      TabOrder = 4
      TextHint = 'Tags'
    end
  end
  object BtnClearTable: TButton
    Left = 680
    Top = 107
    Width = 25
    Height = 25
    Caption = #9851
    TabOrder = 8
    OnClick = BtnClearTableClick
  end
  object LbeBaseURL: TLabeledEdit
    Left = 16
    Top = 111
    Width = 249
    Height = 21
    EditLabel.Width = 45
    EditLabel.Height = 13
    EditLabel.Caption = 'Base URL'
    TabOrder = 9
    Text = 'https://ntfy.sh'
    TextHint = 'Tags'
  end
  object MemTopics: TMemo
    Left = 462
    Top = 32
    Width = 243
    Height = 52
    BorderStyle = bsNone
    Lines.Strings = (
      'your-very-secret-topic'
      'notify-delphi-integration-8jh27d')
    TabOrder = 10
  end
  object BtnHide: TButton
    Left = 16
    Top = 489
    Width = 132
    Height = 25
    Caption = 'Hide'
    TabOrder = 11
    OnClick = BtnHideClick
  end
  object LbeUsername: TLabeledEdit
    Left = 271
    Top = 111
    Width = 132
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'Username'
    TabOrder = 12
    TextHint = '(optional)'
  end
  object LbePassword: TLabeledEdit
    Left = 409
    Top = 111
    Width = 132
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    PasswordChar = '*'
    TabOrder = 13
    TextHint = '(optional)'
  end
  object DtSince: TDateTimePicker
    Left = 111
    Top = 63
    Width = 137
    Height = 21
    Date = 44911.000000000000000000
    Time = 0.858430219908768800
    TabOrder = 14
  end
  object EdtSince: TEdit
    Left = 111
    Top = 36
    Width = 137
    Height = 21
    TabOrder = 15
    TextHint = 'Message id or Unix time'
  end
  object TableNotification: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 568
    Top = 336
    object TableNotificationID: TStringField
      DisplayLabel = 'Id'
      FieldName = 'ID'
      Size = 30
    end
    object TableNotificationTIME: TStringField
      DisplayLabel = 'Time'
      FieldName = 'TIME'
    end
    object TableNotificationPRIORITY: TStringField
      DisplayLabel = 'Priority'
      FieldName = 'PRIORITY'
      Size = 10
    end
    object TableNotificationTITLE: TStringField
      DisplayLabel = 'Title'
      FieldName = 'TITLE'
      Size = 50
    end
    object TableNotificationMSG: TStringField
      DisplayLabel = 'Message'
      FieldName = 'MSG'
      Size = 500
    end
    object TableNotificationTOPIC: TStringField
      DisplayLabel = 'Topic'
      FieldName = 'TOPIC'
      Size = 100
    end
  end
  object DsTable: TDataSource
    DataSet = TableNotification
    Left = 640
    Top = 336
  end
  object Tray: TTrayIcon
    PopupMenu = Pop
    Left = 392
    Top = 344
  end
  object Pop: TPopupMenu
    Left = 432
    Top = 344
    object PopShow: TMenuItem
      Caption = 'Show'
      OnClick = PopShowClick
    end
    object PopSubscribe: TMenuItem
      Caption = 'Subscribe'
      OnClick = PopSubscribeClick
    end
    object PopUnsubscribe: TMenuItem
      Caption = 'Unsubscribe'
      Enabled = False
      OnClick = PopUnsubscribeClick
    end
    object PopQuit: TMenuItem
      Caption = 'Quit'
      OnClick = PopQuitClick
    end
  end
end
