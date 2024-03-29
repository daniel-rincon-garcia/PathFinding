unit Cloud;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Position;

type
  TCloud = class
    private
      Fname: String;
      Fposition: TPosition;
      function getPosition: TPosition;
      procedure setPosition(const position: TPosition);
    public
      constructor Create(const name: String; const positionX: Integer;
                         const positionY: Integer; const PositionZ: Integer); overload;
      constructor Create(const name: String; const position: TPosition); overload;
      // Properties
      property position: TPosition
           read getPosition write setPosition;
  end;

implementation

constructor TCloud.Create(const name: String; const positionX: Integer; const positionY: Integer; const PositionZ: Integer);
begin
  Fname := name;
  Fposition := TPosition.Create(positionX, positionY, PositionZ);
end;

constructor TCloud.Create(const name: String; const position: TPosition);
begin
  Fname := name;
  Fposition := position;
end;

function TCloud.getPosition: TPosition;
begin
  Result := Fposition;
end;

procedure TCloud.setPosition(const position: TPosition);
begin
  Fposition := position;
end;

end.

