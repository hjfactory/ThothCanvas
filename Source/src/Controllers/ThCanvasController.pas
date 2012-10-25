unit ThCanvasController;

interface

uses
  ThTypes, ThItem, ThCanvas, ThCanvasEditor;

type
  TThCanvasEditorController = class(TInterfacedObject, IThObserver, IThCanvasController)
  private
    FCanvas: TThCanvasEditor;
    FSubject: IThSubject;

    procedure ItemAdded(Item: TThItem);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetThCanvas(ThCanvas: IThCanvas);

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

implementation

uses
  ThItemCommand;

{ TThCanvasController }

constructor TThCanvasEditorController.Create;
begin

end;

destructor TThCanvasEditorController.Destroy;
begin
//  FSubject := nil;
//  FCanvas := nil;

  inherited;
end;

procedure TThCanvasEditorController.Notifycation(ACommand: IThCommand);
begin

end;

procedure TThCanvasEditorController.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;

//  ASubject.RegistObserver(Self);
end;

procedure TThCanvasEditorController.SetThCanvas(ThCanvas: IThCanvas);
begin
  FCanvas := TThCanvasEditor(ThCanvas);
  FCanvas.OnItemAdded := ItemAdded;
end;

procedure TThCanvasEditorController.ItemAdded(Item: TThItem);
var
  Command: IThCommand;
begin
//  FSubject.Subject(Self, TThCommandItemAdd.Create(FCanvas, Item));
end;

end.
