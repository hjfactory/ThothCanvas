unit TestThItemText;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms, System.Classes, FMX.Platform,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #18 캔버스에 사각형을 추가한다.
  TestTThText = class(TThCanvasBaseTestUnit)
  published
    procedure TestItemFactory;
  end;

implementation

uses
  DebugUtils,
  FMX.TestLib, ThItem, ThImageItem, ThItemFactory, ThConsts, ThTypes, FMX.Dialogs;

{ TestTThText }

procedure TestTThText.TestItemFactory;
var
  Item: TThItem;
begin
  Item := ItemFactory.Get(ItemFactoryIDText);
  try
    Check(Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;
end;

initialization
  RegisterTest(TestTThText.Suite);

end.
