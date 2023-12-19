unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Notify,
  System.Notification, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids,
  Vcl.DBGrids, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.CheckLst,
  Vcl.Menus;

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
    BtnClearTable: TButton;
    LbeBaseURL: TLabeledEdit;
    MemTopics: TMemo;
    Tray: TTrayIcon;
    Pop: TPopupMenu;
    PopShow: TMenuItem;
    PopSubscribe: TMenuItem;
    PopUnsubscribe: TMenuItem;
    BtnHide: TButton;
    LbeUsername: TLabeledEdit;
    LbePassword: TLabeledEdit;
    PopQuit: TMenuItem;
    DtSince: TDateTimePicker;
    EdtSince: TEdit;
    TableNotificationTIME: TStringField;
    TableNotificationTOPIC: TStringField;
    TableNotificationPRIORITY: TStringField;
    CkShowNotification: TCheckBox;
    procedure BtnSubscribeClick(Sender: TObject);
    procedure BtnUnsubscribeClick(Sender: TObject);
    procedure GbSinceClick(Sender: TObject);
    procedure CkSinceClick(Sender: TObject);
    procedure BtnClearTableClick(Sender: TObject);
    procedure BtnHideClick(Sender: TObject);
    procedure CkFiltersExit(Sender: TObject);
    procedure PopQuitClick(Sender: TObject);
    procedure PopShowClick(Sender: TObject);
    procedure PopSubscribeClick(Sender: TObject);
    procedure PopUnsubscribeClick(Sender: TObject);
    private
      FTopics: String;
      FSince: String;
      procedure CheckTopics;
      procedure CheckSince;
      procedure CheckButtons;
      procedure CheckPops;
      function CheckPriority: String;
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

procedure TViewMain.BtnHideClick(Sender: TObject);
begin
  Hide;
  Tray.Visible := True;
end;

procedure TViewMain.BtnSubscribeClick(Sender: TObject);
begin

  CheckTopics;
  CheckSince;
  CheckButtons;

  Ntfy
    .UserName(LbeUsername.Text)
    .Password(LbePassword.Text)
    .BaseURL(LbeBaseURL.Text)
    .Poll(CkPoll.Checked)
    .Since(FSince)
    .Scheduled(CkScheduled.Checked);

  Ntfy
    .ClearFilters
    .Filter(TNotifyFilter.ID, lbeIdFilter.Text)
    .Filter(TNotifyFilter.TITLE, lbeFilterTitle.Text)
    .Filter(TNotifyFilter.MESSAGECONTENT, lbeFilterMessage.Text)
    .Filter(TNotifyFilter.TAGS, lbeFilterTags.Text)
    .Filter(TNotifyFilter.PRIORITY, CheckPriority)
    .Subscribe(FTopics, YourCallBackProcedure);

  CheckPops;

end;

procedure TViewMain.BtnUnsubscribeClick(Sender: TObject);
begin
  Ntfy.Unsubscribe;
  CheckButtons;
  CheckPops;
end;

procedure TViewMain.CheckButtons;
begin
  if (not CkPoll.Checked)  then
  begin
    BtnSubscribe.Enabled := not BtnSubscribe.Enabled;
    BtnUnsubscribe.Enabled := not BtnUnsubscribe.Enabled;
  end;
end;

procedure TViewMain.CheckPops;
begin
  PopSubscribe.Enabled := not PopSubscribe.Enabled;
  PopUnsubscribe.Enabled := not PopUnsubscribe.Enabled;
end;

function TViewMain.CheckPriority: String;
begin
  Result := '';
  if CbFilterPriority.ItemIndex > 0 then
    Result := IntToStr(Ord(TNotifyPriority(CbFilterPriority.ItemIndex)));
end;

procedure TViewMain.CheckSince;
begin
  FSince := '';
  if CkSince.Checked then
    case GbSince.ItemIndex of
      0, 1: FSince := EdtSince.Text;
      2: FSince := DateTimeToUnix(DtSince.DateTime).ToString;
    end;
end;

procedure TViewMain.CheckTopics;
var
  LTopic: String;
begin
  FTopics := '';
  for LTopic in MemTopics.Lines do
    if FTopics = '' then
      FTopics := LTopic
    else
      FTopics := Format('%s,%s', [FTopics, LTopic]);
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

procedure TViewMain.PopQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TViewMain.PopShowClick(Sender: TObject);
begin
  Show;
end;

procedure TViewMain.PopSubscribeClick(Sender: TObject);
begin
  BtnSubscribe.Click;
end;

procedure TViewMain.PopUnsubscribeClick(Sender: TObject);
begin
  BtnUnsubscribe.Click;
end;

procedure TViewMain.YourCallBackProcedure(AEvent: INotifyEvent);
begin
  TableNotification.Open;
  TableNotification.AppendRecord([
    AEvent.Id,
    FormatDateTime('dd/MM/yyyy hh:mm:ss', UnixToDateTime(AEvent.Time)),
    IntToStr(Ord(AEvent.Priority)),
    AEvent.Title,
    AEvent.MessageContent,
    AEvent.Topic
  ]);

  if CkShowNotification.Checked then
    PushWindowsNotification(AEvent);
end;

end.
