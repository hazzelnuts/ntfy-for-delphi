unit Notify.Provider.Contract;

interface

uses
  System.Classes;

type
  INotifyProvider = interface
    ['{4A4C86DB-6176-404E-A317-BA789ED4848B}']
    function Get: INotifyProvider;
    function Post: INotifyProvider;
    function Put: INotifyProvider;
    function AddHeader(const PName: String; AValue: String): INotifyProvider; overload;
    function AddHeader(const AName: String; AValues: array of String): INotifyProvider; overload;
    function AddBody(const AValue: String): INotifyProvider; overload;
    function AddBody(const AValue: TFileStream): INotifyProvider; overload;
    function AddURLSegment(const AValue: String): INotifyProvider; overload;
  end;

  INotifyProviderFactory = interface
    ['{E337A427-2614-448E-ABF5-82BE0769E016}']
    function Provider: INotifyProvider;
  end;

implementation

end.
