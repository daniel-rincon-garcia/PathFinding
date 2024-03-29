unit Island;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TIsland = class
    private
      Fname:String;
      Frows: Integer;
      Fcolumns: Integer;
      Ftiles: TObjectList;
      function getName: String;
      procedure setName(name: String);
      function getRows: Integer;
      procedure setRows(rows: Integer);
      function getColumns: Integer;
      procedure setColumns(columns: Integer);
      function getTiles: TObjectList;
      procedure setTiles(tiles: TObjectList);
    public
      constructor Create(const name: String; const columns: Integer;
                         const rows: Integer);
      procedure addTile(const tileId: String; const positionX: Integer;
                        const positionY: Integer; const positionZ: Integer;
                        const isWall: Boolean);
      // Properties
      property name: String
           read getName write setName;
      property rows: Integer
           read getRows write setRows;
      property columns: Integer
           read getColumns write setColumns;
      property tiles: TObjectList
           read getTiles write setTiles;
  end;

implementation

uses Tile;

constructor TIsland.Create(const name: String; const columns: Integer;
                           const rows: Integer);
begin
 Fname := name;
 Fcolumns := columns;
 Frows := rows;
 Ftiles := TObjectList.create();
end;

function TIsland.getName: String;
begin
  Result := Fname;
end;

procedure TIsland.setName(name: String);
begin
  Fname := name;
end;

function TIsland.getRows: Integer;
begin
  Result := Frows;
end;

procedure TIsland.setRows(rows: Integer);
begin
  Frows := rows;
end;

function TIsland.getColumns: Integer;
begin
  Result := Fcolumns;
end;

procedure TIsland.setColumns(columns: Integer);
begin
  Fcolumns := columns;
end;

function TIsland.getTiles: TObjectList;
begin
  Result := FTiles;
end;

procedure TIsland.setTiles(tiles: TObjectList);
begin
  Ftiles := tiles;
end;

procedure TIsland.addTile(const tileId: String; const positionX: Integer;
                          const positionY: Integer; const positionZ: Integer;
                          const isWall: Boolean);
var
  tile: TTile;
begin
  tile := TTile.Create(tileId, positionX, positionY, positionZ, isWall);
  Ftiles.Add(tile);
end;

end.

