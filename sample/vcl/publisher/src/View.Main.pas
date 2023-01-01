unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Notify, Vcl.CheckLst, Data.DB, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TViewMain = class(TForm)
    gpInfo: TGroupBox;
    gbActions: TGroupBox;
    CbPriority: TComboBox;
    lbPriority: TLabel;
    CkTags: TCheckListBox;
    Label1: TLabel;
    lbeFileAttachment: TLabeledEdit;
    btnFileAttachment: TButton;
    lbeURLAttachment: TLabeledEdit;
    lbeIconAttachment: TLabeledEdit;
    FileDialog: TOpenDialog;
    CbActionType: TComboBox;
    lblActionType: TLabel;
    CkActionClear: TCheckBox;
    CbActionMethod: TComboBox;
    lbActionMethod: TLabel;
    MemActionBody: TMemo;
    lblActionBody: TLabel;
    btnPublish: TButton;
    DBGrid1: TDBGrid;
    btnAddAction: TButton;
    btnDeleteAction: TButton;
    btnRemoveAction: TButton;
    TableActions: TFDMemTable;
    TableActionsTYPE: TIntegerField;
    DsTableAction: TDataSource;
    TableActionsMETHOD: TStringField;
    TableActionsCLEAR: TBooleanField;
    TableActionsBODY: TMemoField;
    lbeEmail: TLabeledEdit;
    lblTopic: TLabel;
    lbeTitle: TLabeledEdit;
    CbTopic: TComboBox;
    lbeMessage: TLabeledEdit;
    lbeActionLabel: TLabeledEdit;
    lbeActionURL: TLabeledEdit;
    TableActionsLABEL: TStringField;
    TableActionsURL: TStringField;
    Image1: TImage;
    lbeDelay: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnFileAttachmentClick(Sender: TObject);
    procedure CbActionTypeChange(Sender: TObject);
    procedure btnRemoveActionClick(Sender: TObject);
    procedure btnAddActionClick(Sender: TObject);
    procedure btnDeleteActionClick(Sender: TObject);
    published
      procedure btnPublishClick(Sender: TObject);
    private
      FNotification: INotifyNotification;
      procedure SendNotification;
  end;

var
  ViewMain: TViewMain;

implementation

uses
  System.Threading;

{$R *.dfm}

procedure TViewMain.btnAddActionClick(Sender: TObject);
begin

  if TableActions.RecordCount >= 3 then
    Exit;

  TableActions.Open;

  TableActions.AppendRecord([
    Ord(TNotifyActionType(CbActionType.ItemIndex)),
    CbActionMethod.Text,
    CkActionClear.Checked,
    MemActionBody.Lines,
    lbeActionLabel.Text,
    lbeActionURL.Text
  ])
end;

procedure TViewMain.btnDeleteActionClick(Sender: TObject);
begin
  if not TableActions.IsEmpty then
    TableActions.Delete;
end;

procedure TViewMain.btnFileAttachmentClick(Sender: TObject);
begin
  FileDialog.InitialDir := ExtractFileDir(Application.ExeName);
  if FileDialog.Execute then
  begin
    lbeFileAttachment.Text := FileDialog.FileName;
  end;
end;

procedure TViewMain.btnPublishClick(Sender: TObject);
var
  LTask: ITask;
begin

  if lbeTitle.Text = '' then
    Exit;

  LTask := TTask.Create(procedure begin
    try
      btnPublish.Enabled := False;
      btnPublish.Caption := '⌛ Notify';
      SendNotification;
    finally
      TThread.Queue(nil, procedure begin
        btnPublish.Caption := '✔ Notify';
        btnPublish.Enabled := True;
      end)
    end;
  end);

  LTask.Start;
end;

procedure TViewMain.btnRemoveActionClick(Sender: TObject);
begin
  TableActions.EmptyDataSet;
end;

procedure TViewMain.CbActionTypeChange(Sender: TObject);
begin

  if CbActionType.Text = 'view' then
    CbActionMethod.ItemIndex := 1;

  CbActionMethod.Enabled := (CbActionType.Text <> 'view');
  MemActionBody.Enabled := (CbActionType.Text <> 'view');
  lblActionBody.Enabled := (CbActionType.Text <> 'view');

end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  FNotification := New.Notification;
end;

procedure TViewMain.SendNotification;
var
  LTags: TArray<string>;
  LTag: String;
begin

  FNotification := New.Notification;
  FNotification.Topic(CbTopic.Text);
  FNotification.Title(lbeTitle.Text);
  FNotification.MessageContent(lbeMessage.Text);
  FNotification.Priority(TNotifyPriority(CbPriority.ItemIndex + 1));
  FNotification.Icon(lbeIconAttachment.Text);
  FNotification.AttachFile(lbeFileAttachment.Text);
  FNotification.AttachURL(lbeURLAttachment.Text);
  FNotification.Email(lbeEmail.Text);
  FNotification.Delay(lbeDelay.Text);
  //FNotification.Tags(CkTags.Items.ToStringArray);

  if not TableActions.IsEmpty then
  begin
    TableActions.First;
    while not TableActions.Eof do
    begin
      FNotification.Action(New.Action
        .&Label(TableActionsLABEL.AsString)
        .&Url(TableActionsURL.AsString)
        .Method(TableActionsMETHOD.AsString)
        .Clear(TableActionsCLEAR.AsBoolean)
        .&Type(TNotifyActionType(Ord(TableActionsTYPE.AsInteger)))
        .Body(TableActionsBODY.GetAsString)
      );
      TableActions.Next;
    end;
  end;

  Ntfy.Topic(CbTopic.Text);
  Ntfy.Notification(FNotification).Publish;

end;
end.
