unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Edit,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, REST.Backend.PushTypes, System.JSON,
  System.PushNotification, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Backend.BindSource, REST.Backend.PushDevice;

type
  TViewMain = class(TForm)
    Style: TStyleBook;
    Layout1: TLayout;
    Text2: TText;
    Layout4: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Image1: TImage;
    Text1: TText;
    Layout5: TLayout;
    edtBaseURL: TEdit;
    Layout6: TLayout;
    Layout7: TLayout;
    edtTitle: TEdit;
    edtMessage: TMemo;
    Layout8: TLayout;
    edtTopic: TEdit;
    btnPublish: TCornerButton;
    PushEvents1: TPushEvents;
    procedure btnPublishClick(Sender: TObject);
  private
    procedure SendNotification;
  end;

var
  ViewMain: TViewMain;

implementation

uses
  System.Threading,
  Notify;

{$R *.fmx}

procedure TViewMain.btnPublishClick(Sender: TObject);
var
  LTask: ITask;
begin

  LTask := TTask.Create(procedure begin
    try
      btnPublish.Enabled := False;
      btnPublish.Text := '⌛ Notify';
      SendNotification;
    finally
      TThread.Queue(nil, procedure begin
        btnPublish.Text := '✔ Notify';
        btnPublish.Enabled := True;
      end)
    end;
  end);

  LTask.Start;
end;

procedure TViewMain.SendNotification;
begin

  Ntfy.Notification(
    New.Notification
      .Topic(edtTopic.Text)
      .Title(edtTitle.Text)
      .MessageContent(edtMessage.Lines.Text)
  );

  Ntfy
    .BaseURL(edtBaseURL.Text)
    .Publish;

end;

end.
