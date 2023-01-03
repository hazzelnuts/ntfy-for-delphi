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

var
  Id, Time: String;

{ TTestSimpleMessage }

procedure TTestSimpleMessage.CallBack(AEvent: INotifyEvent);
begin
  FTitle := AEvent.Title;
  FMessage := AEvent.MessageContent;
  FPriority := AEvent.Priority;
end;

procedure TTestSimpleMessage.Publish;
begin

  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Title(TITLE)
      .MessageContent(MESSAGECONTENT)
      .Priority(TNotifyPriority.MAX)
  );

  try
    try
      WriteLn('Simple message test: Publish');
      Sleep(TIME_DELAY);
      Ntfy.BaseURL('http://localhost:80');
      Ntfy.ClearFilters;
      Ntfy.Publish;
      Id := Ntfy.Response.Data.Id;
      Time := Ntfy.Response.Data.Time.ToString;
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

end;

procedure TTestSimpleMessage.Subscribe;
begin
  try
    try
      WriteLn('Simple message test: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.BaseURL('http://localhost:80');
      Ntfy.Poll(True);
      Ntfy.Since(Time);
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

end.
