unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Notify,
  System.Notification, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids,
  Vcl.DBGrids;

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
    procedure BtnSubscribeClick(Sender: TObject);
    procedure BtnUnsubscribeClick(Sender: TObject);
    private
      procedure YourCallBackProcedure(AEvent: INotifyEvent);

  end;

var
  ViewMain: TViewMain;

implementation

{$R *.dfm}

procedure TViewMain.BtnSubscribeClick(Sender: TObject);
begin
  BtnSubscribe.Enabled := not BtnSubscribe.Enabled;
  BtnUnsubscribe.Enabled := not BtnUnsubscribe.Enabled;
  Ntfy.Subscribe('notify-delphi-integration-8jh27d', YourCallBackProcedure);
end;

procedure TViewMain.BtnUnsubscribeClick(Sender: TObject);
begin
  BtnSubscribe.Enabled := not BtnSubscribe.Enabled;
  BtnUnsubscribe.Enabled := not BtnUnsubscribe.Enabled;
  Ntfy.Unsubscribe;
end;

procedure TViewMain.YourCallBackProcedure(AEvent: INotifyEvent);
begin
  TableNotification.Open;
  TableNotification.AppendRecord([
    AEvent.Id,
    AEvent.Title,
    AEvent.MessageContent
  ])
end;

end.
