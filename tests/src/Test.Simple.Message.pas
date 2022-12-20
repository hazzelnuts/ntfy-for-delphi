unit Test.Simple.Message;

interface

uses
  Notify, TestFramework;

type
  TTestSimpleMessage = class(TTestCase)
  private
    FTitle: String;
    FMessageContent: String;
    procedure CallBack(AEvent: INotifyEvent);
    const Title = '⚡ Ntfy for Delphi';
    const MessageContent = 'A friendly Delphi library to ntfy.sh';
    const Topic = 'notify-delphi-integration-8jh27d';
    const FailureMessage = 'Response status differs from 200';
    const TitleDifferent = 'Published title differs from received one';
    const MessageDiffent = 'Message content differs from received one';
  public
    procedure SetUp; override;
  published
    procedure SendSimpleMessage;
  end;

implementation

uses
  Winapi.Windows, System.Classes, System.SysUtils;

{ TTestSimpleMessage }

procedure TTestSimpleMessage.CallBack(AEvent: INotifyEvent);
begin
  FTitle := AEvent.Title;
  FMessageContent := AEvent.MessageContent;
end;

procedure TTestSimpleMessage.SendSimpleMessage;
begin

  WriteLn('Simple message test...');

  Ntfy.Notification(
    New.Notification
      .Topic(Topic)
      .Title(Title)
      .MessageContent(MessageContent)
  );

  Ntfy.Publish;
  Sleep(2000);

  Ntfy
    .SaveLog(True)
    .Poll(True)
    .Since(Ntfy.Response.Notification.Time.ToString)
    .Subscribe(Topic, CallBack);

  CheckEquals(Title, FTitle, TitleDifferent);
  CheckEquals(MessageContent, FMessageContent, MessageDiffent);
  Ntfy.Unsubscribe;

end;

procedure TTestSimpleMessage.SetUp;
begin
  inherited;
end;

initialization
  RegisterTest('Sending simple message', TTestSimpleMessage.Suite);

end.
