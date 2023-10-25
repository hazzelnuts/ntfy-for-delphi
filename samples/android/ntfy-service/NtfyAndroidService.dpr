program NtfyAndroidService;

uses
  System.Android.ServiceApplication,
  Service.Module.Ntfy in 'Service.Module.Ntfy.pas' {NtfyAndroidServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNtfyAndroidServiceModule, NtfyAndroidServiceModule);
  Application.Run;
end.
