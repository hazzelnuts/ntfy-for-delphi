unit Test.Email;

interface

uses
  TestFramework, Notify;

type
  TTestEmail = class(TTestCase)
  private
    FPriority: TNotifyPriority;
    FTitle: String;
    FMessageContent: String;
    procedure CallBack(AEvent: INotifyEvent);
  public
    procedure SetUp; override;
  published
    procedure Publish;
    procedure Subscribe;
  end;

implementation

uses
  System.SysUtils, Test.Constants;

var
  Id, Time: String;

{ TTestEmail }

procedure TTestEmail.CallBack(AEvent: INotifyEvent);
begin
  FTitle := AEvent.Title;
  FMessageContent := AEvent.MessageContent;
  FPriority := AEvent.Priority;
end;

procedure TTestEmail.Publish;
begin
  try
    Ntfy := New.Notify;
    Ntfy.Notification(
      New.Notification
        .Topic(TOPIC)
        .Title(TITLE)
        .MessageContent(MESSAGECONTENT)
        .Priority(TNotifyPriority.HIGH)
        .Email('hazzelnuts.contact@gmail.com')
    );

    try
      WriteLn('Send email: Publish');
      Sleep(TIME_DELAY);
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

procedure TTestEmail.SetUp;
begin
  inherited;

end;

procedure TTestEmail.Subscribe;
begin
  try
    try
      WriteLn('Send email: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Time);
      Ntfy.Subscribe(Topic, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(TITLE, FTitle, MSG_WRONG_TITLE);
    CheckEquals(MESSAGECONTENT, FMessageContent, MSG_WRONG_MESSAGE);
    CheckEquals(Ord(TNotifyPriority.HIGH), Ord(FPriority), MSG_WRONG_PRIORITY);
  end;
end;

end.
