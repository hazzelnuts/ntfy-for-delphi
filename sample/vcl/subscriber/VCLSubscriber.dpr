program VCLSubscriber;

uses
  Vcl.Forms,
  View.Main in 'src\View.Main.pas' {ViewMain},
  Example.Push.Notifications in 'src\Example.Push.Notifications.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glow');
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
