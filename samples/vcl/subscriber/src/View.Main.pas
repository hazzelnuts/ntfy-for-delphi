unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Notify,
  System.Notification, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids,
  Vcl.DBGrids, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.CheckLst,
  Vcl.Menus, Vcl.Mask, System.IniFiles;

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
    CkAutoSaveSubs: TCheckBox;
    CkAutoSubscribe: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
      FTopics: String;
      FSince: String;
      FIniFileName: String;
      procedure CheckTopics;
      procedure CheckSince;
      procedure CheckButtons;
      procedure CheckPops;
      function CheckPriority: String;
      procedure YourCallBackProcedure(AEvent: INotifyEvent);
      procedure LoadSettingsFromIni;
      procedure SaveSettingsToIni;
      procedure AutoSubscribe;
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

procedure TViewMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettingsToIni;
  Ntfy.Unsubscribe;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  FIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  LoadSettingsFromIni;
  
  if CkAutoSubscribe.Checked then
    AutoSubscribe;
end;

procedure TViewMain.LoadSettingsFromIni;
var
  Ini: TIniFile;
  TopicsList: TStringList;
  i: Integer;
begin
  if not FileExists(FIniFileName) then
    Exit;
    
  Ini := TIniFile.Create(FIniFileName);
  try
    // Load general settings
    LbeBaseURL.Text := Ini.ReadString('Settings', 'BaseURL', 'https://ntfy.sh');
    LbeUsername.Text := Ini.ReadString('Settings', 'Username', '');
    LbePassword.Text := Ini.ReadString('Settings', 'Password', '');
    CkPoll.Checked := Ini.ReadBool('Settings', 'Poll', True);
    CkScheduled.Checked := Ini.ReadBool('Settings', 'Scheduled', False);
    CkShowNotification.Checked := Ini.ReadBool('Settings', 'ShowNotification', True);
    CkAutoSubscribe.Checked := Ini.ReadBool('Settings', 'AutoSubscribe', False);
    CkAutoSaveSubs.Checked := Ini.ReadBool('Settings', 'AutoSaveSubs', False);
    
    // Load filters
    lbeIdFilter.Text := Ini.ReadString('Filters', 'ID', '');
    lbeFilterTitle.Text := Ini.ReadString('Filters', 'Title', '');
    lbeFilterMessage.Text := Ini.ReadString('Filters', 'Message', '');
    lbeFilterTags.Text := Ini.ReadString('Filters', 'Tags', '');
    CbFilterPriority.ItemIndex := Ini.ReadInteger('Filters', 'Priority', 0);
    
    // Load Since settings
    CkSince.Checked := Ini.ReadBool('Since', 'Enabled', False);
    GbSince.ItemIndex := Ini.ReadInteger('Since', 'Type', 0);
    EdtSince.Text := Ini.ReadString('Since', 'Value', '');
    DtSince.DateTime := StrToDateTimeDef(Ini.ReadString('Since', 'DateTime', ''), Now);
    
    // Load topics
    MemTopics.Clear;
    TopicsList := TStringList.Create;
    try
      Ini.ReadSection('Topics', TopicsList);
      for i := 0 to TopicsList.Count - 1 do
        MemTopics.Lines.Add(Ini.ReadString('Topics', TopicsList[i], ''));
    finally
      TopicsList.Free;
    end;
    
    // Update UI based on loaded settings
    GbSince.Enabled := CkSince.Checked;
    GbSinceClick(nil);
    
  finally
    Ini.Free;
  end;
end;

procedure TViewMain.SaveSettingsToIni;
var
  Ini: TIniFile;
  i: Integer;
begin
  Ini := TIniFile.Create(FIniFileName);
  try
    // Save general settings
    Ini.WriteString('Settings', 'BaseURL', LbeBaseURL.Text);
    Ini.WriteString('Settings', 'Username', LbeUsername.Text);
    Ini.WriteString('Settings', 'Password', LbePassword.Text);
    Ini.WriteBool('Settings', 'Poll', CkPoll.Checked);
    Ini.WriteBool('Settings', 'Scheduled', CkScheduled.Checked);
    Ini.WriteBool('Settings', 'ShowNotification', CkShowNotification.Checked);
    Ini.WriteBool('Settings', 'AutoSubscribe', CkAutoSubscribe.Checked);
    Ini.WriteBool('Settings', 'AutoSaveSubs', CkAutoSaveSubs.Checked);
    
    // Save filters
    Ini.WriteString('Filters', 'ID', lbeIdFilter.Text);
    Ini.WriteString('Filters', 'Title', lbeFilterTitle.Text);
    Ini.WriteString('Filters', 'Message', lbeFilterMessage.Text);
    Ini.WriteString('Filters', 'Tags', lbeFilterTags.Text);
    Ini.WriteInteger('Filters', 'Priority', CbFilterPriority.ItemIndex);
    
    // Save Since settings
    Ini.WriteBool('Since', 'Enabled', CkSince.Checked);
    Ini.WriteInteger('Since', 'Type', GbSince.ItemIndex);
    Ini.WriteString('Since', 'Value', EdtSince.Text);
    Ini.WriteString('Since', 'DateTime', DateTimeToStr(DtSince.DateTime));
    
    // Save topics
    Ini.EraseSection('Topics');
    for i := 0 to MemTopics.Lines.Count - 1 do
      if MemTopics.Lines[i] <> '' then
        Ini.WriteString('Topics', 'Topic' + IntToStr(i), MemTopics.Lines[i]);
        
  finally
    Ini.Free;
  end;
end;

procedure TViewMain.AutoSubscribe;
begin
  if MemTopics.Lines.Count > 0 then
    BtnSubscribe.Click;
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
