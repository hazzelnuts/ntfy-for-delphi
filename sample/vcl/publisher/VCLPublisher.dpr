program VCLPublisher;

uses
   madExcept,
  madListHardware,
  madListProcesses,
  madListModules,
 Vcl.Forms,
  View.Main in 'src\View.Main.pas' {ViewMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glow');
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
