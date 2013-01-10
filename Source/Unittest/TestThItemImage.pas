unit TestThItemImage;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms, System.Classes,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #18 캔버스에 사각형을 추가한다.
  TestTThImage = class(TBaseTestUnit)
  published
    procedure SetUpTestImage;
    procedure TestItemFactory;

    procedure TestAddImage;
  end;

implementation

uses
  FMX.TestLib, ThItem, ThImageItem, ThItemFactory, ThConsts, ThTypes;

var
  TestImagePath: string;


{ TestTThImage }

procedure TestTThImage.SetUpTestImage;
var
  Bitmap: TBitmap;
  Stream: TResourceStream;
begin
  TestImagePath := ExtractFilePath(ParamStr(0)) + 'TEST_IMAGE.PNG';

  // Create Image file from resource
  if not FileExists(TestImagePath) then
  begin
    Bitmap := TBitmap.Create(0, 0);
    Stream := TResourceStream.Create(HInstance, 'TEST_IMAGE', RT_RCDATA);
    try
      Bitmap.LoadFromStream(Stream);
      Bitmap.SaveToFile(TestImagePath);
    finally
      Bitmap.Free;
      Stream.Free;
    end;
  end;

end;

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
  Item := ItemFactory.Get(ItemFactoryIDImageFile, TThFileItemData.Create(TestImagePath));
  try
    Check(Assigned(Item));
  finally
    if Assigned(Item) then
      Item.Free;
  end;
end;

procedure TestTThImage.TestAddImage;
begin
//  ShowForm;

  Check(FCanvas.ItemCount = 0);
  FCanvas.AppendFileItem(ItemFactoryIDImageFile, TestImagePath);

  Check(FCanvas.ItemCount = 1);
end;

initialization
  RegisterTest(TestTThImage.Suite);

end.
