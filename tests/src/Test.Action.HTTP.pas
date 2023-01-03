unit Test.Action.HTTP;

interface

uses
  TestFramework, Notify;

type
  TTestActionHTTP = class(TTestCase)
  private
    FActionType: TNotifyActionType;
    FLabel: String;
    FUrl: String;
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

{ TTestActionHTTP }

procedure TTestActionHTTP.CallBack(AEvent: INotifyEvent);
begin
  FLabel := AEvent.Actions.Items[ACTION_LABEL].&Label;
  FUrl := AEvent.Actions.Items[ACTION_LABEL].Url;
  FActionType := AEvent.Actions.Items[ACTION_LABEL].&Type;
end;

procedure TTestActionHTTP.Publish;
begin
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Action(New.Action
        .&Type(TNotifyActionType.HTTP)
        .&Label(ACTION_LABEL)
        .Url(ACTION_URL)
  ));

  try
    try
      WriteLn('Action http test: Publish');
      Sleep(TIME_DELAY);
      Ntfy.ClearFilters;
      Ntfy.BaseURL('http://localhost:80');
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

procedure TTestActionHTTP.SetUp;
begin

end;

procedure TTestActionHTTP.Subscribe;
begin
  try
    try
      WriteLn('Action http test: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Time);
      Ntfy.BaseURL('http://localhost:80');
      Ntfy.Subscribe(TOPIC, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(Ord(FActionType), Ord(TNotifyActionType.HTTP), MSG_WRONG_HEADERS);
    CheckEquals(FLabel, ACTION_LABEL, MSG_WRONG_LABELS);
    CheckEquals(FUrl, ACTION_URL, MSG_WRONG_URLS);
  end;
end;

end.
