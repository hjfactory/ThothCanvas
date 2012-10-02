unit ThItem;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types;

type
  IThItem = interface

  end;

  TThItem = class(TControl, IThItem)
  protected
    FSelected: Boolean;

    FOnSelected: TNotifyEvent;
    FOnTrack: TNotifyEvent;

    procedure SetSelected(const Value: Boolean);
  public
    property Selected: Boolean read FSelected write SetSelected;

    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;

  end;

  TThItemClass = class of TThItem;

implementation

{ TThItem }

procedure TThItem.SetSelected(const Value: Boolean);
begin
  if FSelected = Value then
    Exit;

  FSelected := Value;

  if FSelected and Assigned(FOnSelected) then
    FOnSelected(Self);
end;

end.
