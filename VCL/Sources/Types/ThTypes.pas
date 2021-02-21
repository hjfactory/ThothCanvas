unit ThTypes;

interface

uses
  System.Classes,
  System.Generics.Collections,
  GR32;

const
  TH_SCALE_MIN = 0.2;
  TH_SCALE_MAX = 4;

type
  TThDrawMode       = (dmSelect, dmDraw, dmPen, dmEraser);

  TThPercent = 0..100;

  TThPath = TArray<TFloatPoint>;

  TThPoly = TArrayOfFloatPoint;
  TThPolyPoly = TArrayOfArrayOfFloatPoint;

type
  IThCanvas = interface
  end;

  IThDrawStyle = interface
  end;

  IThDrawItem = interface
  end;

  IThShapeItem = interface(IThDrawItem)
    function MakePolyPoly(ARect: TFloatRect): TThPolyPoly;
  end;

  // 그리기 객체
  IThDrawObject = interface
    procedure MouseDown(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseMove(const APoint: TFloatPoint; AShift: TShiftState);
    procedure MouseUp(const APoint: TFloatPoint; AShift: TShiftState);

    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);

    function GetDrawItem: IThDrawItem;
    property DrawItem: IThDrawItem read GetDrawItem;
  end;

  IThItemHandles = interface
    procedure DrawHandles(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
    procedure RealignHandles;

    procedure MouseDown(const APoint: TFloatPoint);
    procedure MouseMove(const APoint: TFloatPoint);   // MouseDown & move
    procedure MouseUp(const APoint: TFloatPoint);
    procedure MouseOver(const APoint: TFloatPoint);   // Not mouse downed & move

    function PtInHandles(APoint: TFloatPoint): Boolean;
    function IsOverHandle: Boolean;
  end;

  // 선택 시 크기변경 핸들 관리
  IThItemSelection = interface(IThItemHandles)
    procedure Draw(Bitmap: TBitmap32; AScale, AOffset: TFloatPoint);
  end;

  IThItemConnection = interface(IThItemHandles)

  end;

implementation

end.
