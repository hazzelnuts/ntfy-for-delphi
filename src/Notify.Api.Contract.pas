unit Notify.Api.Contract;

interface

uses
  Notify.Config.Contract,
  System.Classes;

type
  INotifyApi = interface
    ['{4A4C86DB-6176-404E-A317-BA789ED4848B}']
    function Get: INotifyApi;
    function Post: INotifyApi;
    function Put: INotifyApi;
    function AddHeader(const PName: String; AValue: String): INotifyApi; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyApi; overload;
    function AddBody(const AValue: String): INotifyApi; overload;
    function AddBody(const AValue: TFileStream): INotifyApi; overload;
    function AddURLSegment(const AValue: String): INotifyApi; overload;
    function Config(const AValue: INotifyConfig): INotifyApi;
  end;

  INotifyApiFactory = interface
    ['{E337A427-2614-448E-ABF5-82BE0769E016}']
    function Api: INotifyApi;
  end;

implementation

end.
