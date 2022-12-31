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
    procedure Publish;
    procedure Subscribe;
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

procedure TTestSimpleMessage.Publish;
begin
  try
    try
      Sleep(TIME_DELAY);
      Ntfy.ClearFilters;
      Ntfy.Publish;
    except on E: Exception do
      Writeln(E.Message)
    end;
  finally
    CheckEquals(
      Ord(TStatusCode.OK),
      Ntfy.Response.StatusCode,
      MSG_REQUEST_FAILED
    );
  end;
end;

procedure TTestSimpleMessage.SetUp;
begin
  inherited;
  WriteLn('Simple message test');
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Title(TITLE)
      .MessageContent(MESSAGECONTENT)
      .Priority(TNotifyPriority.MAX)
  );
end;

procedure TTestSimpleMessage.Subscribe;
begin
  try
    try
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Ntfy.Response.Data.Id);
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

initialization
  RegisterTest('Sending simple message', TTestSimpleMessage.Suite);

end.
