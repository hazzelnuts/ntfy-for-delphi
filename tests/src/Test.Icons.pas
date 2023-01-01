unit Test.Icons;

interface

uses
  TestFramework, Notify;

type
  TTestIcons = class(TTestCase)
  private
    FIconURL: String;
    const IconURL = 'https://styles.redditmedia.com/t5_32uhe/styles/communityIcon_xnt6chtnr2j21.png';
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

{ TTestIcons }

procedure TTestIcons.CallBack(AEvent: INotifyEvent);
begin
  FIconURL := AEvent.Icon;
end;

procedure TTestIcons.Publish;
begin

  Ntfy := New.Notify;
  Ntfy.Notification(
    New.Notification
      .Topic(TOPIC)
      .Icon(IconURL)
  );

  try
    try
      WriteLn('Send icons: Publish');
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

procedure TTestIcons.SetUp;
begin
  inherited;
end;

procedure TTestIcons.Subscribe;
begin
  try
    try
      WriteLn('Send icons: Subscribe');
      Sleep(TIME_DELAY);
      Ntfy := New.Notify;
      Ntfy.Poll(True);
      Ntfy.Since(Time);
      Ntfy.Subscribe(Topic, CallBack);
    finally
      Ntfy.Unsubscribe;
    end;
  finally
    CheckEquals(FIconURL, IconURL, MSG_WRONG_ICON);
  end;
end;

end.
