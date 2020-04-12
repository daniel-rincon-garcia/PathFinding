unit SceneParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type
  cSceneParser = class
    private
      procedure parseCloud(const Hijo: TDOMNode);
      procedure parseIsland(const Hijo: TDOMNode);
      procedure parseTileNodes(const Hijo: TDOMNode; const wallList: TStringList);
      function fillWallList(const Hijo: TDOMNode): TStringList;
    public
      constructor create();
      procedure parseScene();
  end;

implementation

uses XMLRead, DataModel, Dialogs;

constructor cSceneParser.create();
begin

end;

procedure cSceneParser.parseScene();
var
  Documento: TXMLDocument;
  Hijo: TDOMNode;
begin
  try
    ReadXMLFile(Documento, 'scene.xml');
    // usando las propiedades FirstChild y NextSibling
    Hijo := Documento.DocumentElement.FirstChild;
    while Assigned(Hijo) do
      begin
        if 'cloud' = Hijo.NodeName then
          parseCloud(Hijo)
        else
          if ('island' = Hijo.NodeName) and Hijo.HasChildNodes then
            parseIsland(Hijo);
        Hijo := Hijo.NextSibling;
      end;
  finally
    Documento.Free;
  end;
end;

procedure cSceneParser.parseCloud(const Hijo: TDOMNode);
var
  DtaModel: TDataModel;
begin
  with Hijo.Attributes do
    DtaModel.GetInstance.addCloud(GetNamedItem('name').NodeValue,
                                  StrToInt(GetNamedItem('X').NodeValue),
                                  StrToInt(GetNamedItem('Y').NodeValue),
                                  StrToInt(GetNamedItem('Z').NodeValue));
end;

function cSceneParser.fillWallList(const Hijo: TDOMNode): TStringList;
var
  wallList: TStringList;
  i: Integer;
begin
  wallList := TStringList.Create;
  with Hijo.FirstChild.ChildNodes do
  try
    for i := 0 to (Count - 1) do
      begin
        wallList.Add(item[i].Attributes.GetNamedItem('row').NodeValue + '_' + item[i].Attributes.GetNamedItem('column').NodeValue);
      end;
  finally
    Free;
  end;
  Result := wallList;
end;

procedure cSceneParser.parseTileNodes(const Hijo: TDOMNode;
                                      const wallList: TStringList);
var
  column, row: Integer;
  positionY: Integer;
  isWall: boolean;
  tileId: String;
  DtaModel: TDataModel;
begin
  with Hijo.Attributes do
    for column:= 0 to StrToInt(GetNamedItem('columns').NodeValue) do
      for row:= 0 to StrToInt(GetNamedItem('rows').NodeValue) do
        begin
          tileId := IntToStr(row) + '_' + IntToStr(column);
          positionY := 0;
          if wallList.IndexOf(tileId) = -1 then
            positionY := StrToInt(GetNamedItem('Y').NodeValue) - 70
          else
            positionY := StrToInt(GetNamedItem('Y').NodeValue) - 69;
          isWall := false;
          if wallList.IndexOf(tileId) <> -1 then
            isWall := true;
          DtaModel.GetInstance.addTileToIsland(GetNamedItem('name').NodeValue, tileId,
                                               StrToInt(GetNamedItem('X').NodeValue) + column,
                                               positionY,
                                               StrToInt(GetNamedItem('Z').NodeValue) + row,
                                               isWall);
        end;
end;

procedure cSceneParser.parseIsland(const Hijo: TDOMNode);
var
  wall: TDOMNode;
  wallList: TStringList;
  DtaModel: TDataModel;
begin
  with Hijo do
  begin
    DtaModel.GetInstance.addIsland(Attributes.GetNamedItem('name').NodeValue,
                                   StrToInt(Attributes.GetNamedItem('columns').NodeValue),
                                   StrToInt(Attributes.GetNamedItem('rows').NodeValue));
    with ChildNodes do
    try
      wallList := fillWallList(Hijo);
      parseTileNodes(Hijo, wallList);
    finally
      Free;
    end;
  end;
end;

end.

