unit Test.Action.Header;

interface

uses
  TestFrameWork, Notify.Custom.Types, Notify;

type

  TTestActionHeader = class(TTestCase)
  private
    FHeaders: TNotifyActionHeaders;
    FEventHeaders: TNotifyActionHeaders;
    procedure CallBack(AEvent: INotifyEvent);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Publish;
    procedure Subscribe;
  end;

implementation

uses
  Test.Constants, System.SysUtils;

var
  Id, Time: String;

{ TestActionHeader }

procedure TTestActionHeader.CallBack(AEvent: INotifyEvent);
begin
  FEventHeaders := AEvent.Actions.Items[ACTION_LABEL].EventHeaders;
end;

procedure TTestActionHeader.Publish;
begin

  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Action(New.Action
        .&Type(TNotifyActionType.VIEW)
        .&Label(ACTION_LABEL)
        .Url(ACTION_URL)
        .Headers(FHeaders)
  ));

  try
    try
      WriteLn('Action headers test: Publish');
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

procedure TTestActionHeader.SetUp;
begin
  inherited;
  FHeaders := TNotifyActionHeaders.Create;
  FHeaders.Cmd := 'systeminfo';
  FHeaders.Parameter := '/FO LIST';
  FHeaders.SystemDate := 'date';
end;

procedure TTestActionHeader.Subscribe;
begin
  try
    try
      WriteLn('Action headers test: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Time);
      Ntfy.Subscribe(TOPIC, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(FHeaders.Cmd, FEventHeaders.Cmd, MSG_WRONG_HEADERS);
    CheckEquals(FHeaders.Parameter, FEventHeaders.Parameter, MSG_WRONG_HEADERS);
    CheckEquals(FHeaders.SystemDate, FEventHeaders.SystemDate, MSG_WRONG_HEADERS);
  end;
end;

procedure TTestActionHeader.TearDown;
begin
  inherited;
  FHeaders.Free;
end;

end.
