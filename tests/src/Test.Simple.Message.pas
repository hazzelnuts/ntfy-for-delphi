unit Test.Simple.Message;

interface

uses
  Notify, TestFramework;

type
  TTestSimpleMessage = class(TTestCase)
  private
    FTitle: String;
    FMessage: String;
    FPriority: TNotifyPriority;
    procedure CallBack(AEvent: INotifyEvent);
  public
    procedure SetUp; override;
  published
    procedure SendSimpleMessage;
  end;

implementation

uses
  Test.Constants, Winapi.Windows, System.SysUtils;

{ TTestSimpleMessage }

procedure TTestSimpleMessage.CallBack(AEvent: INotifyEvent);
begin
  FTitle := AEvent.Title;
  FMessage := AEvent.MessageContent;
  FPriority := AEvent.Priority;
end;

procedure TTestSimpleMessage.SendSimpleMessage;
begin
  WriteLn('Simple message test...');


  try
    Ntfy.Publish;
    Sleep(1000);
  finally
    CheckEquals(
      Ord(TStatusCode.OK),
      Ntfy.Response.StatusCode,
      MSG_WRONG_STATUS_CODE
    );
  end;

  try
    try
      Ntfy.Since(Ntfy.Response.Notification.Time.ToString);
      Ntfy.Subscribe(Topic, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(TITLE, FTitle, MSG_WRONG_TITLE);
    CheckEquals(MESSAGECONTENT, FMessage, MSG_WRONG_MESSAGE);
    CheckEquals(Ord(PRIORITY), Ord(FPriority), MSG_WRONG_PRIORITY);
  end;
end;

procedure TTestSimpleMessage.SetUp;
begin
  inherited;
  Ntfy.SaveLog(True);
  Ntfy.Poll(True);
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Title(TITLE)
      .MessageContent(MESSAGECONTENT)
      .Priority(TNotifyPriority.MAX)
  );
end;

initialization
  //RegisterTest('Sending simple message', TTestSimpleMessage.Suite);

end.
