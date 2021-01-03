unit ThCanvasController;

interface

uses
  ThTypes, ThClasses, ThItem, ThCanvas, ThCanvasEditor, System.Types;

type
  TThCanvasController = class(TThInterfacedObject, IThObserver, IThCanvasController)
  private
    procedure CanvasZoom(Sender: TObject);
  protected
    FCanvas: TThCanvasEditor;
    FSubject: IThSubject;
  public
    constructor Create(ThCanvas: IThCanvas); reintroduce; virtual;
    destructor Destroy; override;

    procedure Notifycation(ACommand: IThCommand);
    procedure SetSubject(ASubject: IThSubject);
  end;

  TThCanvasEditorController = class(TThCanvasController)
  private
    procedure ItemAdded(Item: TThItem);
    procedure ItemDelete(Items: TThItems);
    procedure ItemMove(Items: TThItems; Distance: TPointF);
    procedure ItemResize(Item: TThItem; BeforeRect: TRectF);
  public
    constructor Create(ThCanvas: IThCanvas); override;
  end;

implementation

uses
  ThCanvasCommand, ThItemCommand;

{ TThCanvasController }

procedure TThCanvasController.CanvasZoom(Sender: TObject);
begin
  FSubject.Subject(Self, TThCommandCanvasZoom.Create);
end;

constructor TThCanvasController.Create(ThCanvas: IThCanvas);
begin
  FCanvas := TThCanvasEditor(ThCanvas);

  FCanvas.OnZoom := CanvasZoom;
end;

destructor TThCanvasController.Destroy;
begin
  FSubject.UnregistObserver(Self);

  inherited;
end;

procedure TThCanvasController.Notifycation(ACommand: IThCommand);
begin
//  FCanvas.DoGrouping(ACommand.;
end;

procedure TThCanvasController.SetSubject(ASubject: IThSubject);
begin
  FSubject := ASubject;

  ASubject.RegistObserver(Self);
end;

{ TThCanvasController }

constructor TThCanvasEditorController.Create(ThCanvas: IThCanvas);
begin
  inherited;

  FCanvas.OnItemAdded := ItemAdded;
  FCanvas.OnItemDelete := ItemDelete;
  FCanvas.OnItemMove := ItemMove;
  FCanvas.OnItemResize := ItemResize;
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

procedure TThCanvasEditorController.ItemResize(Item: TThItem;
  BeforeRect: TRectF);
begin
  FSubject.Subject(Self, TThCommandItemResize.Create(Item, BeforeRect));
end;

end.

