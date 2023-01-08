object ViewMain: TViewMain
  Left = 0
  Top = 0
  Caption = 'VCL Publisher'
  ClientHeight = 671
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gpInfo: TGroupBox
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 569
    Height = 671
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    Caption = 'Notification'
    Padding.Left = 3
    Padding.Top = 3
    Padding.Right = 3
    Padding.Bottom = 3
    TabOrder = 0
    object lbPriority: TLabel
      Left = 12
      Top = 488
      Width = 34
      Height = 13
      Caption = 'Priority'
    end
    object Label1: TLabel
      Left = 296
      Top = 574
      Width = 23
      Height = 13
      Caption = 'Tags'
    end
    object lblTopic: TLabel
      Left = 12
      Top = 64
      Width = 25
      Height = 13
      Caption = 'Topic'
    end
    object LbMessage: TLabel
      Left = 14
      Top = 152
      Width = 42
      Height = 13
      Caption = 'Message'
    end
    object CbPriority: TComboBox
      Left = 12
      Top = 507
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 2
      Text = 'High'
      Items.Strings = (
        'Min'
        'Low'
        'Default'
        'High'
        'Maximum')
    end
    object lbeFileAttachment: TLabeledEdit
      Left = 12
      Top = 583
      Width = 246
      Height = 21
      EditLabel.Width = 16
      EditLabel.Height = 13
      EditLabel.Caption = 'File'
      TabOrder = 5
      TextHint = 'Attach file...'
    end
    object btnFileAttachment: TButton
      Left = 264
      Top = 581
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 6
      OnClick = btnFileAttachmentClick
    end
    object lbeURLAttachment: TLabeledEdit
      Left = 296
      Top = 507
      Width = 254
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = 'URL Attachment'
      TabOrder = 7
      TextHint = 'Url to an attachment...'
    end
    object lbeIconAttachment: TLabeledEdit
      Left = 12
      Top = 546
      Width = 278
      Height = 21
      EditLabel.Width = 80
      EditLabel.Height = 13
      EditLabel.Caption = 'Icon Attachment'
      TabOrder = 4
      TextHint = 'Attach icon url...'
    end
    object lbeEmail: TLabeledEdit
      Left = 122
      Top = 507
      Width = 168
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Email'
      TabOrder = 3
      TextHint = 'someone@mail.com'
    end
    object lbeTitle: TLabeledEdit
      Left = 14
      Top = 125
      Width = 169
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Title'
      TabOrder = 1
      Text = 'Ntfy for Delphi'
    end
    object CbTopic: TComboBox
      Left = 14
      Top = 83
      Width = 278
      Height = 21
      TabOrder = 0
      Text = 'notify-delphi-integration-8jh27d'
      Items.Strings = (
        'notify-delphi-integration-8jh27d'
        'your-very-secret-topic')
    end
    object lbeDelay: TLabeledEdit
      Left = 296
      Top = 547
      Width = 254
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Delay'
      TabOrder = 8
      TextHint = 'Delay (1h, 10min, 15s)'
    end
    object btnPublish: TButton
      Left = 296
      Top = 201
      Width = 254
      Height = 33
      Caption = #55357#56596' Notify'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = btnPublishClick
    end
    object LbeBaseURL: TLabeledEdit
      Left = 14
      Top = 37
      Width = 169
      Height = 21
      EditLabel.Width = 45
      EditLabel.Height = 13
      EditLabel.Caption = 'Base URL'
      TabOrder = 10
      Text = 'https://ntfy.sh'
    end
    object MemMessage: TMemo
      Left = 14
      Top = 171
      Width = 278
      Height = 63
      Lines.Strings = (
        'Hi, this is a very long message. '
        'I would like to inform that a new library for ntfy is '
        'available for cross-platform applications. '
        'It was made in Delphi. '
        'I'#39'm looking forward to other contribuitors who can help '
        'me out with this project.'
        'New ideas will be far wellcome.'
        'I think togheter we can finish development and support '
        'for other platforms such as linux, android and macos.')
      TabOrder = 11
    end
    object LbeUsername: TLabeledEdit
      Left = 189
      Top = 37
      Width = 169
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = 'Username'
      TabOrder = 12
      TextHint = 'Only for protected topics/servers'
    end
    object LbePassword: TLabeledEdit
      Left = 364
      Top = 37
      Width = 169
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      TabOrder = 13
      TextHint = '(Optional)'
    end
    object CkDisableFirebase: TCheckBox
      Left = 12
      Top = 613
      Width = 118
      Height = 17
      Caption = 'Disable Firebase'
      TabOrder = 14
    end
    object CkCache: TCheckBox
      Left = 12
      Top = 636
      Width = 55
      Height = 17
      Caption = 'Cache'
      Checked = True
      State = cbChecked
      TabOrder = 15
    end
    object MemTags: TMemo
      Left = 296
      Top = 593
      Width = 254
      Height = 63
      Lines.Strings = (
        'smiley'
        'tada'
        'white_check_mark'
        'lock')
      TabOrder = 16
    end
  end
  object gbActions: TGroupBox
    Left = 12
    Top = 240
    Width = 538
    Height = 239
    Caption = 'Actions (up to three actions only)'
    TabOrder = 1
    object lblActionType: TLabel
      Left = 14
      Top = 17
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object lbActionMethod: TLabel
      Left = 124
      Top = 17
      Width = 36
      Height = 13
      Caption = 'Method'
    end
    object lblActionBody: TLabel
      Left = 266
      Top = 17
      Width = 24
      Height = 13
      Caption = 'Body'
      Enabled = False
    end
    object CbActionType: TComboBox
      Left = 14
      Top = 36
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'view'
      OnChange = CbActionTypeChange
      Items.Strings = (
        'view'
        'broadcast'
        'http')
    end
    object CkActionClear: TCheckBox
      Left = 216
      Top = 81
      Width = 55
      Height = 17
      Caption = 'Clear'
      TabOrder = 2
    end
    object CbActionMethod: TComboBox
      Left = 124
      Top = 36
      Width = 104
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 1
      TabOrder = 1
      Text = 'POST'
      Items.Strings = (
        'GET'
        'POST'
        'PUT'
        'PATCH'
        'DELETE')
    end
    object MemActionBody: TMemo
      Left = 266
      Top = 36
      Width = 268
      Height = 66
      Enabled = False
      Lines.Strings = (
        '{ "message":"a json"}')
      TabOrder = 3
    end
    object DBGrid1: TDBGrid
      Left = 2
      Top = 143
      Width = 534
      Height = 94
      Align = alBottom
      BorderStyle = bsNone
      DataSource = DsTableAction
      TabOrder = 6
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'TYPE'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'METHOD'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'CLEAR'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'BODY'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'LABEL'
          Width = 136
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'URL'
          Width = 152
          Visible = True
        end>
    end
    object btnAddAction: TButton
      Left = 290
      Top = 108
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 4
      OnClick = btnAddActionClick
    end
    object btnDeleteAction: TButton
      Left = 371
      Top = 108
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 5
      OnClick = btnDeleteActionClick
    end
    object lbeActionLabel: TLabeledEdit
      Left = 14
      Top = 79
      Width = 187
      Height = 21
      EditLabel.Width = 25
      EditLabel.Height = 13
      EditLabel.Caption = 'Label'
      TabOrder = 7
      Text = 'Check it out'
      TextHint = 'Labels are actions button'#39's captions'
    end
    object lbeActionURL: TLabeledEdit
      Left = 14
      Top = 116
      Width = 268
      Height = 21
      EditLabel.Width = 13
      EditLabel.Height = 13
      EditLabel.Caption = 'Url'
      TabOrder = 8
      Text = 'https://github.com/hazzelnuts/ntfy-for-delphi'
      TextHint = 'Actions might have rest calls'
    end
  end
  object FileDialog: TOpenDialog
    Left = 226
    Top = 287
  end
  object TableActions: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 520
    Top = 592
    object TableActionsTYPE: TIntegerField
      FieldName = 'TYPE'
    end
    object TableActionsMETHOD: TStringField
      FieldName = 'METHOD'
      Size = 10
    end
    object TableActionsCLEAR: TBooleanField
      FieldName = 'CLEAR'
    end
    object TableActionsBODY: TMemoField
      FieldName = 'BODY'
      BlobType = ftMemo
    end
    object TableActionsLABEL: TStringField
      FieldName = 'LABEL'
      Size = 50
    end
    object TableActionsURL: TStringField
      FieldName = 'URL'
      Size = 50
    end
  end
  object DsTableAction: TDataSource
    DataSet = TableActions
    Left = 472
    Top = 592
  end
end
