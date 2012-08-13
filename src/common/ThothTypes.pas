unit ThothTypes;

interface

uses
  System.UITypes, System.Classes;

type
  IThObserver = interface;

  IThCommand = interface
  end;

  IThSubject = interface
    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

  IThShape = interface
  end;

  IThCanvas = interface
  end;

implementation

end.
