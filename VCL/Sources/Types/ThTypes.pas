unit ThTypes;

interface

uses
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  GR32;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;

type
  TThDrawMode       = (dmSelect, dmDraw, dmPen, dmEraser);
  TThShapeDragState = (dsNone, dsItemAdd, dsItemMove, dsItemResize, dsMultSelect{Drag for select});

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThItemSelection = interface;
  IThItemConnection = interface;

  IThCanvas = interface
  end;

  IThDrawStyle = interface
  end;

  IThItem = interface
  ['{2A361DAA-4178-4D6E-868B-B6ADD9CB21D9}']
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    function PtInItem(APt: TFloatPoint): Boolean;
    function GetBounds: TFloatRect;
    function GetPolyPoly: TThPolyPoly;

    property Bounds: TFloatRect read GetBounds;
    property PolyPoly: TThPolyPoly read GetPolyPoly;
  end;

  IThDrawItem = interface(IThItem)
  end;

  IThShapeItem = interface(IThItem)
    procedure ResizeItem(AFromPoint, AToPoint: TFloatPoint);
    procedure DrawPoints(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint;
      AFromPoint, AToPoint: TFloatPoint);
  end;

  IThSelectableItem = interface(IThShapeItem)
  ['{796D1837-E123-4612-9307-53512AD52FDC}']

    procedure MoveItem(APoint: TFloatPoint);

    procedure MouseDown(APoint: TFloatPoint);
    procedure MouseMove(APoint: TFloatPoint);
    procedure MouseUp(APoint: TFloatPoint);
    procedure MouseEnter(APoint: TFloatPoint);
    procedure MouseLeave(APoint: TFloatPoint);

    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetSelection: IThItemSelection;

    property Selected: Boolean read GetSelected write SetSelected;
    property Selection: IThItemSelection read GetSelection;
  end;

  // 연결할 수 있는(도형)
  IThConnectableItem = interface(IThSelectableItem)
  ['{6ECF9DA8-3440-42B9-80DE-C33B296CC4D5}']
    procedure ShowConnection;
    procedure HideConnection;

    function GetConnection: IThItemConnection;

    property Connection: IThItemConnection read GetConnection;
    // Visible
  end;

  // 연결자(선)
  IThConnectorItem = interface
  ['{B08D51EF-045C-4C7C-B694-DDD4B4C1625A}']

  end;

  // 그리기 객체
  IThDrawObject = interface
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    function GetItemInstance: IThItem;
    property ItemInstance: IThItem read GetItemInstance;
  end;

  IThItemHandle = interface
    function GetCursor: TCursor;
    property Cursor: TCursor read GetCursor;
  end;

  IThItemHandles = interface
    procedure DrawHandles(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure RealignHandles;

    procedure MouseDown(const APoint: TFloatPoint);
    procedure MouseMove(const APoint: TFloatPoint);   // MouseDown & move
    procedure MouseUp(const APoint: TFloatPoint);

    function PtInHandles(APoint: TFloatPoint): Boolean;

    function GetHotHandle: IThItemHandle;
    procedure ReleaseHotHandle;
    property HotHandle: IThItemHandle read GetHotHandle;
  end;

  // 선택 시 크기변경 핸들 관리
  IThItemSelection = interface(IThItemHandles)
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure ResizeItem(const APoint: TFloatPoint);
  end;

  IThItemConnection = interface(IThItemHandles)
  ['{A4ED614A-0CB3-419A-85B2-0C42176B6C53}']
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
  end;

implementation

end.
