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
    lblTopic: TLabel;
    lbeMessage: TLabeledEdit;
    lbeTitle: TLabeledEdit;
    gbActions: TGroupBox;
    CbPriority: TComboBox;
    lbPriority: TLabel;
    ckTags: TCheckListBox;
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
    CbTopic: TComboBox;
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
    procedure FormCreate(Sender: TObject);
    procedure btnFileAttachmentClick(Sender: TObject);
    published
      procedure btnPublishClick(Sender: TObject);
    private
      FNotification: INotifyNotification;
  end;

var
  ViewMain: TViewMain;

implementation

{$R *.dfm}

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
  LTags: TArray<string>;
begin

  FNotification.Topic(CbTopic.Text);
  FNotification.Title(lbeTitle.Text);
  FNotification.MessageContent(lbeMessage.Text);
  FNotification.Priority(TNotifyPriority(CbPriority.ItemIndex + 1));
  FNotification.Icon(lbeIconAttachment.Text);
  FNotification.FilePath(lbeFileAttachment.Text);
  FNotification.Attach(lbeURLAttachment.Text);
  FNotification.Email(lbeEmail.Text);

  Ntfy.Notification(FNotification);
  Ntfy.Publish;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  FNotification := New.Notification;
end;

end.
