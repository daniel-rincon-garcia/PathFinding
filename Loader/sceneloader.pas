unit SceneLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLScene, PathFindingManager;

type
  TSceneLoader = class
    private
      FGLScene: TGLScene;
      function getGLScene: TGLScene;
      procedure setGLScene(const GLScene: TGLScene);
    public
      constructor Create(const GLScene: TGLScene);
      procedure loadScene(var pathFindingManagerInstance: cPathFindingManager);
      procedure loadClouds();
      procedure loadIslands(var pathFindingManagerInstance: cPathFindingManager);
      // Properties
      property GLScene: TGLScene
           read getGLScene write setGLScene;
  end;

implementation

uses DataModel, PathFindingNode, Cloud, Island, Tile, Dialogs;

constructor TSceneLoader.Create(const GLScene: TGLScene);
begin
  FGLScene := GLScene;
  inherited Create;
end;

function TSceneLoader.getGLScene: TGLScene;
begin
  Result := FGLScene;
end;

procedure TSceneLoader.setGLScene(const GLScene: TGLScene);
begin
  FGLScene := GLScene;
end;

procedure TSceneLoader.loadScene(var pathFindingManagerInstance: cPathFindingManager);
begin
  loadClouds();
  loadIslands(pathFindingManagerInstance);
end;

procedure TSceneLoader.loadClouds();
var
  i: Integer;
  cloud: cPathFindingNode;
  cloudModel: TCloud;
  DtaModel: TDataModel;
begin
  for i := 0 to DtaModel.GetInstance.clouds.Count - 1 do
    begin
      cloudModel := DtaModel.GetInstance.clouds.Items[i] as TCloud;
      cloud := cPathFindingNode(FGLScene.Objects.AddNewChild(cPathFindingNode));
      cloud.Position.X := cloudModel.position.X;
      cloud.Position.Y := cloudModel.position.Y;
      cloud.Position.Z := cloudModel.position.Z;
    end;
end;

procedure TSceneLoader.loadIslands(var pathFindingManagerInstance: cPathFindingManager);
var
  i, j: Integer;
  tile: cPathFindingNode;
  islandModel: TIsland;
  tileModel: TTile;
  DtaModel: TDataModel;
begin
  for i := 0 to DtaModel.GetInstance.islands.Count - 1 do
    begin
      islandModel := DtaModel.GetInstance.islands.Items[i] as TIsland;
      pathFindingManagerInstance := cPathFindingManager.create(islandModel.columns, islandModel.rows);
      for j := 0 to islandModel.tiles.Count - 1 do
        begin
          tileModel := islandModel.tiles.Items[j] as TTile;
          tile := cPathFindingNode(GLScene.Objects.AddNewChild(cPathFindingNode));
          tile.id := tileModel.id;
          tile.Name := tileModel.name;
          tile.Position.X  := tileModel.position.X;
          tile.Position.Y := tileModel.position.Y;
          tile.Position.Z  := tileModel.position.Z;
          if not tileModel.isWall then
            tile.nodeType := MidleNode
          else
            begin
              tile.nodeType := WallNode;
              tile.Pickable := False;
            end;
          pathFindingManagerInstance.AddNode(tile);
        end;
    end;
end;

end.

