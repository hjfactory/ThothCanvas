unit ThCanvasController;

interface

uses
  ThTypes, ThClasses, ThItem, ThCanvas, ThCanvasEditor, System.Types;

type
  TThCanvasEditorController = class(TThInterfacedObject, IThObserver, IThCanvasController)
  private
    FCanvas: TThCanvasEditor;
    FSubject: IThSubject;

    procedure ItemAdded(Item: TThItem);
    procedure ItemDelete(Items: TThItems);
    procedure ItemMove(Items: TThItems; Distance: TPointF);
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

  ASubject.RegistObserver(Self);
end;

procedure TThCanvasEditorController.SetThCanvas(ThCanvas: IThCanvas);
begin
  FCanvas := TThCanvasEditor(ThCanvas);
  FCanvas.OnItemAdded := ItemAdded;
  FCanvas.OnItemDelete := ItemDelete;
  FCanvas.OnItemMove := ItemMove;
end;

procedure TThCanvasEditorController.ItemAdded(Item: TThItem);
begin
  FSubject.Subject(Self, TThCommandItemAdd.Create(FCanvas, Item));
end;

procedure TThCanvasEditorController.ItemDelete(Items: TThItems);
begin
  FSubject.Subject(Self, TThCommandItemDelete.Create(FCanvas, Items));
end;

procedure TThCanvasEditorController.ItemMove(Items: TThItems;
  Distance: TPointF);
begin
  FSubject.Subject(Self, TThCommandItemMove.Create(Items, Distance));
end;

end.
