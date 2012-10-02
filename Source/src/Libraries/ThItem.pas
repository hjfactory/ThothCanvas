unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types;

type
  IThItem = interface

  end;

  TThItem = class(TControl, IThItem)

  end;

  TThItemClass = class of TThItem;

implementation

end.
