unit ThothTypes;

interface

uses
  System.UITypes, System.Classes;

type
  IThCommand = interface
  end;

  IThObserver = interface;
//  TThObserver = class;

  IThSubject = interface
    procedure Report(ACommand: IThCommand);
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  end;

implementation

end.
