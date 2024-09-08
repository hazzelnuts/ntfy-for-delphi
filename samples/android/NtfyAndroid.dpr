program NtfyAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'src\View.Main.pas' {Form1},
  Intent.Service.Helper in 'src\Intent.Service.Helper.pas',
  Service.Local.Ntfy in 'services\ntfy-service\src\Service.Local.Ntfy.pas',
  FMX.Skia {DM: TAndroidService};

{$R *.res}

begin
  GlobalUseSkia := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
