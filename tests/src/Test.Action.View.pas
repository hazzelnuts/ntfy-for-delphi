unit Test.Action.View;

interface

uses
  TestFrameWork, Notify;

type
  TTestActionView = class(TTestCase)
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
  System.SysUtils, Test.Constants;

var
  Id, Time: String;

{ TTestActionView }

procedure TTestActionView.CallBack(AEvent: INotifyEvent);
begin
  FLabel := AEvent.Actions.Items[ACTION_LABEL].&Label;
  FUrl := AEvent.Actions.Items[ACTION_LABEL].Url;
  FActionType := AEvent.Actions.Items[ACTION_LABEL].&Type;
end;

procedure TTestActionView.Publish;
begin
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Action(New.Action
        .&Type(TNotifyActionType.VIEW)
        .&Label(ACTION_LABEL)
        .Url(ACTION_URL)
  ));

  try
    try
      WriteLn('Send action view: Publish');
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

procedure TTestActionView.SetUp;
begin
  inherited;
end;

procedure TTestActionView.Subscribe;
begin
  try
    try
      WriteLn('Send action view: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Time);
      Ntfy.Subscribe(TOPIC, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(Ord(FActionType), Ord(TNotifyActionType.VIEW), MSG_WRONG_HEADERS);
    CheckEquals(FLabel, ACTION_LABEL, MSG_WRONG_LABELS);
    CheckEquals(FUrl, ACTION_URL, MSG_WRONG_URLS);
  end;
end;

end.
