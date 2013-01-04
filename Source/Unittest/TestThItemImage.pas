unit TestThItemImage;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #18 캔버스에 사각형을 추가한다.
  TestTThImage = class(TBaseTestUnit)
  published
    procedure TestItemFactory;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThImageItem, ThItemFactory, ThConsts;

{ TestTThImage }

procedure TestTThImage.TestItemFactory;
var
  Item: TThItem;
begin
  // Not assigned number 0
  Item := ItemFactory.Get(0);
  try
    Check(not Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;

  // 2000 is Image
  Item := ItemFactory.Get(2000);
  try
    Check(Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;
end;

initialization
  RegisterTest(TestTThImage.Suite);

end.
