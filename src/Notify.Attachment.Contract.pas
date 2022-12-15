unit Notify.Attachment.Contract;

interface

type
  INotifyAttachment = interface
    ['{2F96DFC0-08A4-4096-BF48-578E4B6EB327}']
    function Name: String; overload;
    function Name(const AValue: String): INotifyAttachment; overload;
    function Url: String; overload;
    function Url(const AValue:String): INotifyAttachment; overload;
    function MimeType: String; overload;
    function MimeType(const AValue: String): INotifyAttachment; overload;
    function Size: Integer; overload;
    function Size(const AValue: Integer): INotifyAttachment; overload;
    function Expires: Integer; overload;
    function Expires(const AValue: Integer): INotifyAttachment; overload;
  end;

  INotifyAttachmentFactory = interface
    ['{64C7D33A-56D6-46D5-A016-405D9F712BDC}']
    function Attachment: INotifyAttachment;
  end;

implementation

end.
