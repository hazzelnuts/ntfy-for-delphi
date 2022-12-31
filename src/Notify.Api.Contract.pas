unit Notify.Api.Contract;

interface

uses
  System.Classes,
  Notify.Config.Contract,
  Notify.Api.Response;

type
  INotifyApi = interface
    ['{4A4C86DB-6176-404E-A317-BA789ED4848B}']
    function Response: TNotifyApiResponse;
    function Get: INotifyApi;
    function Post: INotifyApi;
    function Put: INotifyApi;
    function ClearHeaders: INotifyApi;
    function ClearBody: INotifyApi;
    function ClearURLParameters: INotifyApi;
    function ClearEndPoint: INotifyApi;
    function AddHeader(const PName: String; AValue: String): INotifyApi; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyApi; overload;
    function AddBody(const AValue: String): INotifyApi; overload;
    function AddBody(const AValue: TFileStream): INotifyApi; overload;
    function AddEndPoint(const AValue: String): INotifyApi; overload;
    function AddURLParameter(const AName: String; AValue: String): INotifyApi;
    function Config(const AValue: INotifyConfig): INotifyApi;
    function AbortStream: INotifyApi;
  end;

implementation

end.
