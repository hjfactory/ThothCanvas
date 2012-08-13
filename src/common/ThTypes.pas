unit ThTypes;

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

  procedure Debug(const Value: string; const Args: array of const); overload;
  procedure Debug(Value: string); overload;

implementation

uses
  WinAPI.Windows, System.SysUtils;

procedure Debug(Value: string);
begin
  OutputDebugString(PChar(Value));
end;

procedure Debug(const Value: string; const Args: array of const);
begin
  Debug(Format(Value, Args));
end;

end.
