program NtfyAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'src\View.Main.pas' {Form1},
  Service.Module.Ntfy in 'ntfy-service\Service.Module.Ntfy.pas' {NtfyAndroidServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
