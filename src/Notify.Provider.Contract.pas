unit Notify.Provider.Contract;

interface

type
  INotifyProvider = interface
    ['{4A4C86DB-6176-404E-A317-BA789ED4848B}']
    function Get: INotifyProvider;
  end;

  INotifyProviderFactory = interface
    ['{E337A427-2614-448E-ABF5-82BE0769E016}']
    function Provider: INotifyProvider;
  end;

implementation

end.
