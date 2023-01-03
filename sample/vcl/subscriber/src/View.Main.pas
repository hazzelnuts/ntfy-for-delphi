unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Notify,
  System.Notification, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids,
  Vcl.DBGrids, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.CheckLst;

type
  TViewMain = class(TForm)
    BtnSubscribe: TButton;
    BtnUnsubscribe: TButton;
    DBGrid1: TDBGrid;
    TableNotification: TFDMemTable;
    TableNotificationID: TStringField;
    TableNotificationTITLE: TStringField;
    TableNotificationMSG: TStringField;
    DsTable: TDataSource;
    CkPoll: TCheckBox;
    GbSince: TRadioGroup;
    CkScheduled: TCheckBox;
    CkSince: TCheckBox;
    GbFilters: TGroupBox;
    lbeIdFilter: TLabeledEdit;
    lbeFilterTitle: TLabeledEdit;
    lbeFilterMessage: TLabeledEdit;
    CbFilterPriority: TComboBox;
    lbPriority: TLabel;
    lbeFilterTags: TLabeledEdit;
    lblTopic: TLabel;
    DtSince: TDateTimePicker;
    EdtSince: TEdit;
    CkTopic1: TCheckBox;
    CkTopic2: TCheckBox;
    CkFilters: TCheckBox;
    BtnClearTable: TButton;
    Image1: TImage;
    LbeBaseURL: TLabeledEdit;
    procedure BtnSubscribeClick(Sender: TObject);
    procedure BtnUnsubscribeClick(Sender: TObject);
    procedure GbSinceClick(Sender: TObject);
    procedure CkSinceClick(Sender: TObject);
    procedure BtnClearTableClick(Sender: TObject);
    procedure CkFiltersExit(Sender: TObject);
    procedure CkFiltersClick(Sender: TObject);
    private
      procedure CheckButtons;
      procedure YourCallBackProcedure(AEvent: INotifyEvent);

  end;

var
  ViewMain: TViewMain;

implementation

{$R *.dfm}

uses
  System.DateUtils,
  Example.Push.Notifications;

procedure TViewMain.BtnClearTableClick(Sender: TObject);
begin
  TableNotification.EmptyDataSet;
end;

procedure TViewMain.BtnSubscribeClick(Sender: TObject);
var
  LSince, LTopics: String;
begin

  if CkSince.Checked then
  begin
    case GbSince.ItemIndex of
      0, 1: LSince := EdtSince.Text;
      2: LSince := DateTimeToUnix(DtSince.DateTime).ToString;
    end;
  end;

  if not CkPoll.Checked and (CkTopic1.Checked or CkTopic2.Checked) then
    CheckButtons;

  Ntfy
    .BaseURL(LbeBaseURL.Text)
    .Poll(CkPoll.Checked)
    .Since(LSince)
    .Scheduled(CkScheduled.Checked);

  Ntfy.ClearFilters;
  
  if CkFilters.Checked then
  begin
    
    if lbeIdFilter.Text <> '' then
      Ntfy.Filter(TNotifyFilter.ID, lbeIdFilter.Text);
    
    if lbeFilterTitle.Text <> '' then
      Ntfy.Filter(TNotifyFilter.TITLE, lbeFilterTitle.Text);
    
    if lbeFilterMessage.Text <> '' then
      Ntfy.Filter(TNotifyFilter.MESSAGECONTENT, lbeFilterMessage.Text);
    
    if lbeFilterTags.Text <> '' then
      Ntfy.Filter(TNotifyFilter.TAGS, lbeFilterTags.Text);
    
    if CbFilterPriority.ItemIndex <> 5 then
      Ntfy.Filter(TNotifyFilter.PRIORITY, IntToStr(CbFilterPriority.ItemIndex + 1));      
  end;  

  if CkTopic1.Checked then
    LTopics := CkTopic1.Caption;

  if CkTopic2.Checked then
    LTopics := Format('%s,%s', [CkTopic1.Caption, CkTopic2.Caption]);

  if (CkTopic1.Checked) or (CkTopic2.Checked) then
    Ntfy.Subscribe(LTopics, YourCallBackProcedure);

end;

procedure TViewMain.BtnUnsubscribeClick(Sender: TObject);
begin
  Ntfy.Unsubscribe;
  CheckButtons;
end;

procedure TViewMain.CheckButtons;
begin
  BtnSubscribe.Enabled := not BtnSubscribe.Enabled;
  BtnUnsubscribe.Enabled := not BtnUnsubscribe.Enabled;
end;

procedure TViewMain.CkFiltersClick(Sender: TObject);
begin
  if CkFilters.Checked then
    GbFilters.Enabled := not GbFilters.Enabled;
end;

procedure TViewMain.CkFiltersExit(Sender: TObject);
begin
  if CkScheduled.Checked then
    CkPoll.Checked := True;
end;

procedure TViewMain.CkSinceClick(Sender: TObject);
begin
  GbSince.Enabled := not GbSince.Enabled;
end;

procedure TViewMain.GbSinceClick(Sender: TObject);
begin
  case GbSince.ItemIndex of
    0, 1: begin
      EdtSince.Enabled := True;
      DtSince.Enabled := False;
    end;
    else
    begin
      EdtSince.Enabled := False;
      DtSince.Enabled := True;
    end;
  end;
end;

procedure TViewMain.YourCallBackProcedure(AEvent: INotifyEvent);
begin
  TableNotification.Open;
  TableNotification.AppendRecord([
    AEvent.Id,
    AEvent.Title,
    AEvent.MessageContent
  ]);
  PushWindowsNotification(AEvent);
end;

end.
