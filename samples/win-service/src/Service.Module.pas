unit Service.Module;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Notify;

type
  TNtfyModule = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  NtfyModule: TNtfyModule;

implementation

uses
  System.Win.Registry;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  NtfyModule.Controller(CtrlCode);
end;
function TNtfyModule.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TNtfyModule.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      Reg.WriteString('Description', 'Ntfy Client Service');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TNtfyModule.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // You can log your messages with a service. I could not find a solution
  // for pushing windows notifications on win services.
  Ntfy.Subscribe('your-very-secret-topic',procedure (AEvent: INotifyEvent)
  var LPath: TStringStream;
  begin
    LPath := TStringStream.Create(AEvent.MessageContent);
    try
      LPath.SaveToFile(ExtractFilePath(ParamStr(0)) + 'ntfy-logs.txt');
    finally
      LPath.Free;
    end;
  end);
end;

procedure TNtfyModule.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Ntfy.Unsubscribe;
end;

end.
