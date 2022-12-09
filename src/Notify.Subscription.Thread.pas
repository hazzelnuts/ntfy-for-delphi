unit Notify.Subscription.Thread;

interface

uses
  System.Classes, System.SysUtils;

type

  TNotifySubcriptionThread = class(TThread)
  private
    FCallBack: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACallBack: TThreadMethod);
    destructor Destroy;
  end;

implementation

{ TNotifySubcriptionThread }

constructor TNotifySubcriptionThread.Create(const ACallBack: TThreadMethod);
begin
  inherited Create(True);
  FCallBack := ACallBack;
end;

destructor TNotifySubcriptionThread.Destroy;
begin
  Writeln('Destroying');
  inherited;
end;

procedure TNotifySubcriptionThread.Execute;
begin
  inherited;
  FCallBack;
end;

end.
