program AndroidPublisher;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'src\View.Main.pas' {ViewMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
