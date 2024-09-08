unit Test.Subscription;

interface

uses
  TestFramework, Notify, System.SyncObjs;

type

  TMessageStatus = record
    received: Boolean;
    msg: string;
    constructor Create(AReceived: Boolean; AMsg: String);
  end;

  TMsgs = class
  public
    class var
      msg1m: TMessageStatus;
      msg2m: TMessageStatus;
      msg5m: TMessageStatus;
      msg10m: TMessageStatus;
      msg15m: TMessageStatus;
      msg30m: TMessageStatus;
      msg1h: TMessageStatus;
      msg3h: TMessageStatus;
      msg6h: TMessageStatus;
      msg12h: TMessageStatus;
      msg24h: TMessageStatus;
    class procedure Initialize; static;
  end;

  TTestSubscription = class(TTestCase)
  private
    Event: TEvent;
    FMsg: TMsgs;
    procedure CallBack(AEvent: INotifyEvent);
  published
    procedure Publish;
    procedure Subscribe;
  public
    constructor Create;
    destructor Destroy;
  end;

implementation

uses
  System.SysUtils, Test.Constants, System.Classes;

{ TTestSubscription }

var
  Id, Time: String;


procedure TTestSubscription.CallBack(AEvent: INotifyEvent);
begin
  WriteLn(AEvent.Title);
end;

constructor TTestSubscription.Create;
begin
  FMsg := TMsgs.Create;
end;

destructor TTestSubscription.Destroy;
begin
  FMsg.Free;
end;

procedure TTestSubscription.Publish;
var
  Id: String;
  Time: String;
begin

  try
    try

      WriteLn('Long subscription, publishing messages for 1m, 2m, 5m, 10m, 15m, 30m, 1h, 3h, 6h, 12h and 24h');
      Sleep(TIME_DELAY);

      Ntfy := New.Notify;
      Ntfy.BaseURL('http://localhost:80');
      FMsg.Initialize;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg1m.msg)
          .Title(FMsg.msg1m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg2m.msg)
          .Title(FMsg.msg2m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg5m.msg)
          .Title(FMsg.msg5m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg10m.msg)
          .Title(FMsg.msg10m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg15m.msg)
          .Title(FMsg.msg15m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg30m.msg)
          .Title(FMsg.msg30m.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg1h.msg)
          .Title(FMsg.msg1h.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg3h.msg)
          .Title(FMsg.msg3h.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg6h.msg)
          .Title(FMsg.msg6h.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg12h.msg)
          .Title(FMsg.msg12h.msg)
      ).Publish;

      Ntfy.ClearFilters.Notification(
        New.Notification
          .Topic(TOPIC)
          .Delay(FMsg.msg24h.msg)
          .Title(FMsg.msg24h.msg)
      ).Publish;


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

procedure TTestSubscription.Subscribe;
begin
  try
    WriteLn('Long subscription, waiting for scheduled notifications...');
    Sleep(TIME_DELAY);
    Ntfy := New.Notify;
    Ntfy.BaseURL('http://localhost:80').Subscribe(Topic, CallBack);
  finally
    Ntfy.Unsubscribe;
  end;
end;

{ TMsgs }

class procedure TMsgs.Initialize;
begin
  msg1m := TMessageStatus.Create(False, '1m');
  msg2m := TMessageStatus.Create(False, '2m');
  msg5m := TMessageStatus.Create(False, '5m');
  msg10m := TMessageStatus.Create(False, '10m');
  msg15m := TMessageStatus.Create(False, '15m');
  msg30m := TMessageStatus.Create(False, '30m');
  msg1h := TMessageStatus.Create(False, '1h');
  msg3h := TMessageStatus.Create(False, '3h');
  msg6h := TMessageStatus.Create(False, '6h');
  msg12h := TMessageStatus.Create(False, '12h');
  msg24h := TMessageStatus.Create(False, '24h');
end;

{ TMessageStatus }

constructor TMessageStatus.Create(AReceived: Boolean; AMsg: String);
begin
  msg := AMsg;
  received := AReceived;
end;

end.
