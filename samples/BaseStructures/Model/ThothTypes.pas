unit ThothTypes;

interface

uses
  System.UITypes, System.Classes;

type
  IThObserver = interface;

  IThCommand = interface
  end;

//  TThObserver = class;

  IThSubject = interface
    procedure Subject(ASource: IThObserver; ACommand: IThCommand);
    procedure RegistObserver(AObserver: IThObserver);
    procedure UnregistObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;
//  TThObserver = class(TThInterfacedObject, IThObserver)
//    procedure Notifycation(ACommand: IThCommand); virtual; abstract;
//    procedure SetSubject(ASubject: IThSubject); virtual; abstract;
//  end;

  IThShape = interface
  end;

  IThCanvas = interface
//    property Selected read
//    procedure SetSelection(AShape: IThShape);    // Unselect & select
//    procedure AddSelection(AShape: IThShape); // Multi select
  end;

implementation

end.
