unit DataModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TDataModel = class
    private
      Fclouds: TObjectList;
      Fislands: TObjectList;
      constructor Create;
      function getClouds: TObjectList;
      procedure setClouds(const clouds: TObjectList);
      function getIslands: TObjectList;
      procedure setIslands(const islands: TObjectList);
    public
      Fname: String;
      function GetInstance: TDataModel;
      procedure addCloud(const name: String; const positionX: Integer;
                         const positionY: Integer;
                         const PositionZ: Integer);
      procedure addIsland(const name: String; const columns: Integer;
                          const rows: Integer);
      procedure addTileToIsland(const islandName: String; const tileId: String;
                                const positionX: Integer;
                                const positionY: Integer;
                                const positionZ: Integer;
                                const isWall: Boolean);
      // Properties
      property clouds: TObjectList
           read getClouds write setClouds;
      property islands: TObjectList
           read getIslands write setClouds;
  end;

implementation

uses Cloud, Island;

var
  DataModelInstance: TDataModel = nil;

constructor TDataModel.Create;
begin
  Fclouds := TObjectList.create();
  Fislands := TObjectList.create();
  inherited Create;
end;

function TDataModel.GetInstance: TDataModel;
begin
  if DataModelInstance = nil then
    raise Exception.Create('DataModel not created during initialization.');
  Result := DataModelInstance;
end;

function TDataModel.getClouds: TObjectList;
begin
  Result := Fclouds;
end;

procedure TDataModel.setClouds(const clouds: TObjectList);
begin
  Fclouds := clouds;
end;

function TDataModel.getIslands: TObjectList;
begin
  Result := Fislands;
end;

procedure TDataModel.setIslands(const islands: TObjectList);
begin
  Fislands := islands;
end;

procedure TDataModel.addCloud(const name: String; const positionX: Integer;
                              const positionY: Integer;
                              const PositionZ: Integer);
var
  Cloud: TCloud;
begin
  Cloud := TCloud.Create(name, positionX, positionY, positionZ);
  Fclouds.add(Cloud);
end;

procedure TDataModel.addIsland(const name: String; const columns: Integer;
                               const rows: Integer);
var
  Island: TIsland;
begin
  Island := TIsland.Create(name, columns, rows);
  Fislands.add(Island);
end;

procedure TDataModel.addTileToIsland(const islandName: String;
                                     const tileId: String;
                                     const positionX: Integer;
                                     const positionY: Integer;
                                     const positionZ: Integer;
                                     const isWall: Boolean);
var
  i: Integer;
  island: TIsland;
begin
  for i := 0 to islands.Count -1 do
    begin
      Island := islands.Items[i] as TIsland;
      if island.name = islandName then
        begin
          island.addTile(tileId, positionX, positionY, positionZ, isWall);
          break;
        end;
    end;
end;

initialization
  DataModelInstance := TDataModel.Create;

end.


