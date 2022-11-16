unit Notify.Provider.Net;

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TNotifyProviderNet = class(TDataModule)
    NetHTTPClient: TNetHTTPClient;
    NetHTTPRequest: TNetHTTPRequest;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NotifyProviderNet: TNotifyProviderNet;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
