unit TestCases;

interface

uses
   TestFramework, Notify;

type
  TTestNotifyPublisher = class(TTestCase)
  public
    procedure SetUp; override;
  published
    procedure SendSimpleMessage;
    procedure SendWithPriorities;
  end;

implementation

 { TestNotifyPublisher }

procedure TTestNotifyPublisher.SendSimpleMessage;
var
  LResult: Boolean;
begin
  LResult := True;
  try
    Ntfy.Notification(
      New.Notification
        .Topic('notify-delphi-integration-8jh27d')
        .Title('Ntfy for Delphi')
        .MessageContent('A friendly library to ntfy.sh for Delphi'));
    Ntfy.Publish;
  except
    LResult := False;
  end;
  CheckEquals(True, LResult, 'Publishing simple messages failed');
end;

procedure TTestNotifyPublisher.SendWithPriorities;
var
  LResult: Boolean;
begin
  LResult := True;
  try
    Ntfy.Notification(
      New.Notification
        .Topic('notify-delphi-integration-8jh27d')
        .Title('With Priorities')
        .Priority(TNotifyPriority.HIGH));
    Ntfy.Publish;
  except
    LResult := False;
  end;
  CheckEquals(True, LResult, 'Publishing messages with priorities failed');
end;

procedure TTestNotifyPublisher.SetUp;
begin
  inherited;
end;

initialization
   RegisterTest('Publishing simples messages', TTestNotifyPublisher.Suite);
 end.
