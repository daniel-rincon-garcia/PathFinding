unit Position;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TPosition = class
    private
      FX: Integer;
      FY: Integer;
      FZ: Integer;
      function getX: Integer;
      procedure setX(const X: Integer);
      function getY: Integer;
      procedure setY(const Y: Integer);
      function getZ: Integer;
      procedure setZ(const Z: Integer);
    public
      constructor Create(const positionX: Integer; const positionY: Integer;
                         const PositionZ: Integer);
      // Properties
      property X: Integer
           read getX write setX;
      property Y: Integer
           read getY write setY;
      property Z: Integer
           read getZ write setZ;
  end;

implementation

constructor TPosition.Create(const positionX: Integer; const positionY: Integer;
                             const PositionZ: Integer);
begin
  FX := positionX;
  FY := positionY;
  FZ := positionZ;
end;

function TPosition.getX: Integer;
begin
  Result := FX;
end;

procedure TPosition.setX(const X: Integer);
begin
  FX := X;
end;

function TPosition.getY: Integer;
begin
  Result := FY;
end;

procedure TPosition.setY(const Y: Integer);
begin
  FY := Y;
end;

function TPosition.getZ: Integer;
begin
  Result := FZ;
end;

procedure TPosition.setZ(const Z: Integer);
begin
  FZ := Z;
end;

end.

