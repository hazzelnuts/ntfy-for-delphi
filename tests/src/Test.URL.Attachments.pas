unit Test.URL.Attachments;

interface

uses
  TestFramework, Notify;

type
  TTestURLAttachments = class(TTestCase)
  private
    FURLAttachment: String;
    const URLAttachment =
      'https://images.unsplash.com/photo-1670531910262-'+
      '5ddb5ad666ea?crop=entropy&cs=tinysrgb&fit=crop&f'+
      'm=jpg&h=150&ixid=MnwxfDB8MXxyYW5kb218MHx8fHx8fHx'+
      '8MTY3MTE0OTI2NA&ixlib=rb-4.0.3&q=80&w=500';
    procedure CallBack(AEvent: INotifyEvent);
  published
    procedure Publish;
    procedure Subscribe;
  end;


implementation

uses
  System.SysUtils, Test.Constants;

var
  Id, Time: String;

{ TTestURLAttachments }

procedure TTestURLAttachments.CallBack(AEvent: INotifyEvent);
begin
  FURLAttachment := AEvent.Attachment.Url;
end;

procedure TTestURLAttachments.Publish;
begin
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .AttachURL(URLAttachment)
  );

  try
    try
      WriteLn('Send url attachments: Publish');
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

procedure TTestURLAttachments.Subscribe;
begin
  try
    try
      WriteLn('Send url attachments: Subscribe');
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
    CheckEquals(FURLAttachment, URLAttachment, MSG_WRONG_URL_ATTACHMENT);
  end;
end;

end.
