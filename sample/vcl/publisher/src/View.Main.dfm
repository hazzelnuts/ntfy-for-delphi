object ViewMain: TViewMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Publisher'
  ClientHeight = 418
  ClientWidth = 612
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
    Width = 612
    Height = 418
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
    object lblTopic: TLabel
      Left = 17
      Top = 22
      Width = 25
      Height = 13
      Caption = 'Topic'
    end
    object lbPriority: TLabel
      Left = 17
      Top = 154
      Width = 34
      Height = 13
      Caption = 'Priority'
    end
    object Label1: TLabel
      Left = 17
      Top = 333
      Width = 23
      Height = 13
      Caption = 'Tags'
    end
    object lbeMessage: TLabeledEdit
      Left = 17
      Top = 128
      Width = 278
      Height = 21
      EditLabel.Width = 42
      EditLabel.Height = 13
      EditLabel.Caption = 'Message'
      TabOrder = 0
      Text = 'Cannot forget the gift!'
    end
    object lbeTitle: TLabeledEdit
      Left = 17
      Top = 83
      Width = 169
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Title'
      TabOrder = 1
      Text = #55357#56518' My wife'#39's gift'
    end
    object CbPriority: TComboBox
      Left = 17
      Top = 173
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Min'
      Items.Strings = (
        'Min'
        'Low'
        'Default'
        'High'
        'Maximum')
    end
    object ckTags: TCheckListBox
      Left = 17
      Top = 352
      Width = 152
      Height = 57
      BorderStyle = bsNone
      ItemHeight = 13
      Items.Strings = (
        'grinning'
        'smiley'
        'laughing'
        'slightly_smiling_face')
      ParentColor = True
      TabOrder = 3
    end
    object lbeFileAttachment: TLabeledEdit
      Left = 17
      Top = 264
      Width = 246
      Height = 21
      EditLabel.Width = 16
      EditLabel.Height = 13
      EditLabel.Caption = 'File'
      TabOrder = 4
      TextHint = 'Attach file...'
    end
    object btnFileAttachment: TButton
      Left = 269
      Top = 262
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 5
      OnClick = btnFileAttachmentClick
    end
    object lbeURLAttachment: TLabeledEdit
      Left = 17
      Top = 309
      Width = 278
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = 'URL Attachment'
      TabOrder = 6
      TextHint = 'Url to an attachment...'
    end
    object lbeIconAttachment: TLabeledEdit
      Left = 17
      Top = 219
      Width = 278
      Height = 21
      EditLabel.Width = 80
      EditLabel.Height = 13
      EditLabel.Caption = 'Icon Attachment'
      TabOrder = 7
      TextHint = 'Attach icon url...'
    end
    object CbTopic: TComboBox
      Left = 17
      Top = 41
      Width = 278
      Height = 21
      ItemIndex = 0
      TabOrder = 8
      Text = 'notify-delphi-integration-8jh27d'
      Items.Strings = (
        'notify-delphi-integration-8jh27d')
    end
    object lbeEmail: TLabeledEdit
      Left = 127
      Top = 173
      Width = 168
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Email'
      TabOrder = 9
      TextHint = 'someone@mail.com'
    end
  end
  object gbActions: TGroupBox
    Left = 308
    Top = 46
    Width = 291
    Height = 300
    Caption = 'Actions'
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
      Left = 14
      Top = 63
      Width = 24
      Height = 13
      Caption = 'Body'
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
      Items.Strings = (
        'view'
        'broadcast'
        'http')
    end
    object CkActionClear: TCheckBox
      Left = 240
      Top = 38
      Width = 55
      Height = 17
      Caption = 'Clear'
      TabOrder = 1
    end
    object CbActionMethod: TComboBox
      Left = 124
      Top = 36
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 2
      Text = 'POST'
      Items.Strings = (
        'GET'
        'POST'
        'PUT'
        'PATCH'
        'DELETE')
    end
    object MemActionBody: TMemo
      Left = 14
      Top = 82
      Width = 268
      Height = 50
      TabOrder = 3
    end
    object DBGrid1: TDBGrid
      Left = 2
      Top = 169
      Width = 287
      Height = 129
      Align = alBottom
      BorderStyle = bsNone
      DataSource = DsTableAction
      TabOrder = 4
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
          Width = 108
          Visible = True
        end>
    end
    object btnAddAction: TButton
      Left = 14
      Top = 138
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 5
    end
    object btnDeleteAction: TButton
      Left = 95
      Top = 138
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 6
    end
    object btnRemoveAction: TButton
      Left = 176
      Top = 138
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 7
    end
  end
  object btnPublish: TButton
    Left = 462
    Top = 359
    Width = 137
    Height = 45
    Caption = 'Publish Notification'
    TabOrder = 2
    OnClick = btnPublishClick
  end
  object FileDialog: TOpenDialog
    Left = 258
    Top = 362
  end
  object TableActions: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 328
    Top = 360
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
  end
  object DsTableAction: TDataSource
    DataSet = TableActions
    Left = 400
    Top = 360
  end
end
