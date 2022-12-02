unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TViewMain = class(TForm)
    btnPublish: TButton;
    lbeTitle: TLabeledEdit;
    lbeMessage: TLabeledEdit;
    Image: TImage;
    procedure btnPublishClick(Sender: TObject);

  end;

var
  ViewMain: TViewMain;


implementation

uses
  NotifyDelphi;


{$R *.dfm}

procedure TViewMain.btnPublishClick(Sender: TObject);
begin
    Ntfy.Notification(
      New.Notification
        .Topic('notify-delphi-integration-8jh27d')
        .Title(lbeTitle.Text)
        .MessageContent(lbeMessage.Text)
    );

    Ntfy.Publish;
end;

end.
