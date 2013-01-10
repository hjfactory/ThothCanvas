unit TestThItemGroupping;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #19 캔버스에 선을 추가한다.
  TestTThItemGroupping = class(TBaseTestUnit)
  published
    // #235 C1이 P1의 영역으로 이동 시 그룹핑 된다.
    procedure TestC1MoveInnerP1;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThShapeItem, ThItemFactory, ThConsts, System.Math, DebugUtils;

{ TestTThItemGroupping }

procedure TestTThItemGroupping.TestC1MoveInnerP1;
begin
  ShowForm;

  DrawRectangle(10, 10, 50, 50);   // C1
  DrawRectangle(100, 100, 200, 200); // P1

//  TestLib.RunMouseClick(30, 30);
  MousePath.New
  .Add(30, 30)
  .Add(180, 180)
  .Add(150, 150);
  TestLib.RunMousePath(MousePath.Path);

  Check(FCanvas.ItemCount = 1, Format('ItemCount = %d', [FCanvas.ItemCount]));
end;

initialization
  RegisterTest(TestTThItemGroupping.Suite);

end.
