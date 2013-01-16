unit TestThItemImage;

interface

uses
  TestFramework, BaseTestUnit, FMX.Types, FMX.Forms, System.Classes, FMX.Platform,
  System.UITypes, System.Types, System.SysUtils, FMX.Controls, System.UIConsts;

type
  // #18 캔버스에 사각형을 추가한다.
  TestTThImage = class(TThCanvasBaseTestUnit)
  protected
    procedure CreateObject; override;
  published
    procedure TestItemFactory;

    // #222 로컬의 이미지를 선택하여 로드한다.
    procedure TestAddImage;
    procedure TestAddSelectImage;

    // #223 이미지 추가 시 경로 미선택 시 추가가 취소되어야 한다.
    procedure TestAddSelectImageCancel;

    // #224 이미지 추가 시 중앙에 위치해야 한다.
    procedure TestAddImageCenterPosition;
  end;

implementation

uses
  DebugUtils,
  FMX.TestLib, ThItem, ThImageItem, ThItemFactory, ThConsts, ThTypes, FMX.Dialogs;

var
  TestImagePath: string;

{ TestTThImage }

procedure TestTThImage.CreateObject;
begin
  inherited;

  if TestImagePath <> '' then
    Exit;
  TestImagePath := GetImagePath;
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
  Check(FCanvas.ItemCount = 0);
  FCanvas.AppendFileItem(ItemFactoryIDImageFile, TestImagePath);

  Check(FCanvas.ItemCount = 1);
end;

procedure TestTThImage.TestAddSelectImage;
var
  ServiceIntf: IInterface;
  thread: TThread;
begin
  // Clipboard에 추가
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ServiceIntf) then
    (ServiceIntf as IFMXClipboardService).SetClipboard(TestImagePath);

  thread := TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to 20 do
      begin
        Sleep(1000);
        TestLib.RunKeyPress([vkControl, Ord('V')]);
        TestLib.RunKeyPress([vkReturn]);

        if FCanvas.ItemCount > 0 then
          Break;
      end;
    end
  );
  thread.FreeOnTerminate := False;
  thread.Start;
  FCanvas.AppendFileItem(ItemFactoryIDImageFile);

  thread.WaitFor;
  thread.Terminate;
  thread.Free;

  Check(FCanvas.ItemCount = 1);
  TestLib.RunMouseClick(150, 150);

  CheckNotNull(FCanvas.SelectedItem);
  Check(TThImageItem(FCanvas.SelectedItem).Bitmap.Width > 0);
end;

procedure TestTThImage.TestAddSelectImageCancel;
var
  ServiceIntf: IInterface;
  thread: TThread;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ServiceIntf) then
    (ServiceIntf as IFMXClipboardService).SetClipboard(TestImagePath);
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to 20 do
      begin
        Sleep(100);
        TestLib.RunKeyPress([vkEscape]);
      end;
    end
  );
  thread.FreeOnTerminate := False;
  thread.Start;
  FCanvas.AppendFileItem(ItemFactoryIDImageFile);

  thread.WaitFor;
  thread.Terminate;
  thread.Free;

  Application.ProcessMessages;

  Check(FCanvas.ItemCount = 0);
end;

procedure TestTThImage.TestAddImageCenterPosition;
begin
  FCanvas.AppendFileItem(ItemFactoryIDImageFile, TestImagePath);

  TestLib.RunMouseClick(10, 10);
  CheckNull(FCanvas.SelectedItem);

  TestLib.RunMouseClick(150, 150);
  CheckNotNull(FCanvas.SelectedItem);
end;

initialization
  RegisterTest(TestTThImage.Suite);

end.
