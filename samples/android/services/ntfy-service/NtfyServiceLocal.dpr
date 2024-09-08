program NtfyServiceLocal;

uses
  System.Android.ServiceApplication,
  Service.Local.Ntfy in 'src\Service.Local.Ntfy.pas' {DM: TAndroidService},
  Intent.Service.Helper in '..\..\src\Intent.Service.Helper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
