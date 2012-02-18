unit ThothTypes;

interface

uses
  System.UITypes, System.Classes;

type
  IThCommand = interface
  end;

  IThObserver = interface;
  IThSubject = interface
    procedure Report(ACommand: IThCommand);
    procedure AddObserver(AObserver: IThObserver);
  end;

  IThObserver = interface
    procedure Notifycation(ACommand: IThCommand);
  end;

  IThShape = interface

  end;

  IThCanvas = interface
    procedure DrawBegin(Shape: IThShape);
    procedure DrawEnd(Shape: IThShape);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  end;

implementation

end.
