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
  end;

implementation

{ TNotifySubcriptionThread }

constructor TNotifySubcriptionThread.Create(const ACallBack: TThreadMethod);
begin
  inherited Create(True);
  FCallBack := ACallBack;
end;

procedure TNotifySubcriptionThread.Execute;
begin
  inherited;
  FCallBack;
end;

end.
