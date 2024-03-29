unit Tile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Position;

type
  TTile = class
    private
      Fid: String;
      Fname: String;
      Fposition: TPosition;
      Fwall: Boolean;
      function getId: String;
      procedure setId(Id: String);
      function getName: String;
      procedure setName(const name: String);
      function getPosition: TPosition;
      procedure setPosition(const position: TPosition);
      function getWall: Boolean;
      procedure setWall(const wall: Boolean);
    public
      constructor Create(const id: String; const position: TPosition;
                         const isWall: boolean); overload;
      constructor Create(const id: String; const positionX: Integer;
                         const positionY: Integer; const positionZ: Integer;
                         const isWall: boolean); overload;
      // Properties
      property id: String
           read getId write setId;
      property name: String
           read getName write setName;
      property position: TPosition
           read getPosition write setPosition;
      property isWall: Boolean
           read getWall write setWall;
  end;

implementation

constructor TTile.Create(const id: String; const position: TPosition;
                         const isWall: boolean);
begin
  Fid := id;
  Fname := name;
  Fposition := position;
  Fwall := isWall;
end;

constructor TTile.Create(const id: String; const positionX: Integer;
                         const positionY: Integer; const positionZ: Integer;
                         const isWall: boolean);
begin
  Fid := id;
  Fname := 'Tile' + id;
  Fposition := TPosition.Create(positionX, positionY, positionZ);
  Fwall := isWall;
end;

function TTile.getId: String;
begin
  Result := Fid;
end;

procedure TTile.setId(Id: String);
begin
  Fid := id;
end;

function TTile.getName: String;
begin
  Result := Fname;
end;

procedure TTile.setName(const name: String);
begin
  Fname := name;
end;

function TTile.getPosition: TPosition;
begin
  Result := Fposition;
end;

procedure TTile.setPosition(const position: TPosition);
begin
  Fposition := position;
end;

function TTile.getWall: Boolean;
begin
  Result := Fwall;
end;

procedure TTile.setWall(const wall: Boolean);
begin
  Fwall := wall;
end;

end.

