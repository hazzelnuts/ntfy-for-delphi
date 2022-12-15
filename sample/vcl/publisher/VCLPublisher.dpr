program VCLPublisher;

uses
  Vcl.Forms,
  View.Main in 'src\View.Main.pas' {ViewMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
