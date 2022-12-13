unit Notify.Attachment.Factory;

interface

uses
  Notify.Attachment.Contract;

type
  TNotifyAttachmentFactory = class(TInterfacedObject, INotifyAttachmentFactory)
  public
    class function New: INotifyAttachmentFactory;
    function Attachment: INotifyAttachment;
  end;

implementation

uses
  Notify.Attachment;

{ TNotifyAttachmentFactory }

function TNotifyAttachmentFactory.Attachment: INotifyAttachment;
begin
  Result := TNotifyAttachment.New;
end;

class function TNotifyAttachmentFactory.New: INotifyAttachmentFactory;
begin
  Result := Self.Create;
end;

end.
