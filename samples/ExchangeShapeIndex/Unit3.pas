unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts;

type
  TSB = class(TScrollBox)
  public
    procedure InsertObject(Index: Integer; AObject: TFmxObject); override;
  end;

  TForm3 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    R4: TRectangle;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    SB: TSB;
    R1, R2, R3: TRectangle;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
begin
  SB := TSB.Create(Self);
  SB.Align := TAlignLayout.alClient;
  SB.Parent := Panel1;

  R1 := TRectangle.Create(SB);
  R1.Width := 200;
  R1.Height := 200;
  R1.Position.Point := PointF(20, 20);
  R1.Parent := SB;

  R2 := TRectangle.Create(SB);
  R2.Width := 200;
  R2.Height := 200;
  R2.Position.Point := PointF(120, 120);
  R2.Parent := SB;

  R3 := TRectangle.Create(SB);
  R3.Width := 100;
  R3.Height := 100;
  R3.Position.Point := PointF(80, 150);
  R3.Parent := SB;

  R4.Parent := nil;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  R4.Position.Point := PointF(100, 100);

  R4.Parent := SB;
//  SB.InsertObject(2, R4);

  SB.Repaint;
end;

// 안되면 감추기만(ChildCount가 늘어나 성능에 영향)
procedure TForm3.Button2Click(Sender: TObject);
begin
  R2.Visible := not R2.Visible;

  SB.Repaint;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  R4.Position.Point := PointF(100, 100);

  SB.AddObject(R4);
//  R4.Parent := SB;

  SB.Repaint;
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  R4.Position.Point := PointF(100, 100);

  SB.InsertObject(1, R4);
//  R4.Parent := SB;

  SB.Repaint;
end;

{ TSB }

procedure TSB.InsertObject(Index: Integer; AObject: TFmxObject);
begin
  if (FContent <> nil) and (AObject <> FContent) and (AObject <> FResourceLink) and
    not (AObject is TEffect) and not (AObject is TAnimation) then
  begin
    FContent.InsertObject(Index, AObject);
  end
  else
    inherited;
end;

end.
