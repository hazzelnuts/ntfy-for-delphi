unit Test.Action.Header;

interface

uses
  TestFrameWork, REST.Json.Types, Notify.Custom.Types, Notify;

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
    procedure SendMessageWithActionHeaders;
  end;

implementation

uses
  Test.Constants, System.SysUtils;

{ TestActionHeader }

procedure TTestActionHeader.CallBack(AEvent: INotifyEvent);
begin
  FEventHeaders := AEvent.Action.EventHeaders;
end;

procedure TTestActionHeader.SendMessageWithActionHeaders;
begin

  WriteLn('Action headers test');
  FHeaders.Cmd := 'systeminfo';
  FHeaders.Parameter := '/FO LIST';
  FHeaders.SystemDate := 'date';

  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Action(New.Action
        .&Type(TNotifyActionType.VIEW)
        .&Label('Test Action Header')
        .Url(ACTION_URL)
        .Headers(FHeaders)
  ));

  try
    try
      Sleep(1000);
      Ntfy.ClearFilters;
      Ntfy.Publish;
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

  try
    try
      Sleep(1000);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Ntfy.Response.Data.Id);
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

procedure TTestActionHeader.SetUp;
begin
  inherited;
  FHeaders := TNotifyActionHeaders.Create;
  FEventHeaders := TNotifyActionHeaders.Create;
end;

procedure TTestActionHeader.TearDown;
begin
  inherited;
  FHeaders.Free;
  FEventHeaders.Free;
end;

initialization
  RegisterTest('Sending Action Headers', TTestActionHeader.Suite);

end.
