unit Test.Emojis;

interface

uses
  TestFrameWork, Notify;

type
  TTestEmojis = class(TTestCase)
  private
    FEmojis: TArray<String>;
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

{ TTestEmojis }

procedure TTestEmojis.CallBack(AEvent: INotifyEvent);
var
  Idx: Integer;
begin
  for Idx := 0 to Length(AEvent.Tags) - 1 do
    CheckEquals(FEmojis[Idx], AEvent.Tags[Idx], MSG_WRONG_EMOJI);
end;

procedure TTestEmojis.Publish;
begin
  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Tags(FEmojis)
  );

  try
    try
      WriteLn('Send tags: Publish');
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

procedure TTestEmojis.SetUp;
begin
  inherited;
  FEmojis := TArray<String>.Create('warning', 'cd', 'skull');
end;

procedure TTestEmojis.Subscribe;
begin
  try
    WriteLn('Send tags: Subscribe');
    Sleep(TIME_DELAY);
    Ntfy := New.Notify;
    Ntfy.BaseURL('http://localhost:80');
    Ntfy.Poll(True);
    Ntfy.Since(Time);
    Ntfy.Subscribe(Topic, CallBack);
  finally
    Ntfy.Unsubscribe;
  end;
end;

end.
