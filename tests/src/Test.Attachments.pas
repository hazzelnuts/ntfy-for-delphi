unit Test.Attachments;

interface

uses
  TestFramework, Notify;

type
  TTestAttachments = class(TTestCase)
  private
    FFileName: String;
    FFilePath: String;
    procedure CallBack(AEvent: INotifyEvent);
  public
    procedure SetUp; override;
  published
    procedure Publish;
    procedure Subscribe;
  end;

implementation

uses
  Test.Constants, System.SysUtils;

var
  Id, Time: String;

{ TTestAttachments }

procedure TTestAttachments.CallBack(AEvent: INotifyEvent);
begin
  CheckEquals(FFileName, AEvent.Attachment.Name, MSG_WRONG_FILENAME);
end;

procedure TTestAttachments.Publish;
begin
  WriteLn('Send Attachment: Publish');
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .AttachFile(FFilePath)
  );

  try
    try
      Sleep(TIME_DELAY);
      Ntfy.ClearFilters;
      Ntfy.Publish;
      Id := Ntfy.Response.Data.Id;
      Time := Ntfy.Response.Data.Time.ToString;
    except on E: Exception do
      WriteLn(E.Message);
    end;
  finally
    CheckEquals(
      Ord(TStatusCode.OK),
      Ntfy.Response.StatusCode,
      MSG_REQUEST_FAILED
    );
  end;
end;

procedure TTestAttachments.SetUp;
begin
  inherited;
  FFilePath := '..\..\img\delphi-notify.png';
  FFileName := ExtractFileName(FFilePath);
end;

procedure TTestAttachments.Subscribe;
begin
  WriteLn('Send Attachment: Subscribe');
  try
    Sleep(TIME_DELAY);
    Ntfy := New.Notify;
    Ntfy.Poll(True);
    Ntfy.Since(Time);
    Ntfy.Subscribe(TOPIC, CallBack);
  finally
    Ntfy.Unsubscribe;
  end;
end;

end.
