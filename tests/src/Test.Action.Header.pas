unit Test.Action.Header;

interface

uses
  Notify, TestFrameWork, REST.Json.Types;

type

  TActionHeaders = class(TJsonDTO)
  private
    [JSONName('cmd')]
    FCmd: String;
    [JSONName('parameter')]
    FParameter: String;
  published
    property Cmd: String read FCmd write FCmd;
    property Parameter: String read FParameter write FParameter;
  end;

  TTestActionHeader = class(TTestCase)
  private
    FHeaders: TActionHeaders;
    FEventHeaders: TActionHeaders;
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
  FEventHeaders.AsJson := AEvent.Action.EventHeaders.AsJson;
end;

procedure TTestActionHeader.SendMessageWithActionHeaders;
begin

  FHeaders.Cmd := 'systeminfo';
  FHeaders.Parameter := ' C:\';

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
      Ntfy.SaveLog(True);
      Ntfy.Poll(True);
      Ntfy.Since(Ntfy.Response.Data.Time.ToString);
      Ntfy.Subscribe(TOPIC, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(FHeaders.Cmd, FEventHeaders.Cmd, MSG_WRONG_HEADERS);
    CheckEquals(FHeaders.Parameter, FEventHeaders.Parameter, MSG_WRONG_HEADERS);
  end;
end;

procedure TTestActionHeader.SetUp;
begin
  inherited;
  FHeaders := TActionHeaders.Create;
  FEventHeaders := TActionHeaders.Create;
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
