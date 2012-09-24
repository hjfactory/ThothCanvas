unit FMX.TestLib.Mouse;

interface

uses
  System.Types, System.UITypes, FMX.Types, FMX.Forms, FMX.Platform.Win;

procedure MouseDown(const P: TPointF);
procedure MouseUp(const P: TPointF);
procedure MouseClick(const P: TPointF);

implementation

function GetObjInPoint(const P: TPointF): IControl;
begin
//  Result := IControl(ObjectAtPoint(ClientToScreen(P)));
  if Assigned(Application.MainForm) then
  begin

  end;

end;


procedure MouseDown(const P: TPointF);
var
  Pt: TPointF;
  Obj: IControl;
begin
  Obj := GetObjInPoint(P);
  if Assigned(Obj) then
  begin
    Pt := Obj.LocalToScreen(P);
    Pt := Obj.ScreenToLocal(Pt);
    Obj.MouseDown(TMouseButton.mbLeft, [], P.X, P.Y);
    Obj.MouseUp(TMouseButton.mbLeft, [], P.X, P.Y);
  end;
end;

procedure MouseUp(const P: TPointF);
begin

end;

procedure MouseClick(const P: TPointF);
begin

end;

end.
