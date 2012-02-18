unit ThothCommands;

interface

uses
  ThothTypes;

type
///////////////////////////////////////////////////////
// Command
  TThCommand = class;
  TThCommand = class(TInterfacedObject, IThCommand)
  protected
    procedure Execute; virtual; abstract;
    procedure Rollback; virtual; abstract;
  end;

  TThShapeCommand = class(TThCommand)
  public
//    procedure Execute; override;
//    procedure Rollback; override;
  end;

  TThInsertShapeCommand = class(TThShapeCommand)

  end;

  TThDeleteShapeCommand = class(TThShapeCommand)

  end;

  TThMoveShapeCommand = class(TThShapeCommand)

  end;

  TThResizeShapeCommand = class(TThShapeCommand)

  end;

  TThStyleCommand = class(TThCommand)

  end;


implementation

end.
