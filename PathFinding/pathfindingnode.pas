unit PathFindingNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLObjects;

type
  TNodeType = (StartNode, MidleNode, EndNode, WallNode);

  cPathFindingNode = class(TGLCube)
    private
      // Class attributes
      m_id: String;
      m_g: integer;
      m_h: integer;
      m_parent: cPathFindingNode;
      m_type: TNodeType;

      // Get methods
      function getId: String;
      function getG: integer;
      function getH: integer;
      function getParentNode: cPathFindingNode;
      function getType: TNodeType;
      function getF: integer;

      // Set methods
      procedure setId(Id: String);
      procedure setG(G: integer);
      procedure setH(H: integer);
      procedure setType(nodeType: TNodeType);
      procedure setParentNode(ParentNode: cPathFindingNode);
    public
      constructor Create(id: String; g: integer; h: integer; parentNode: cPathFindingNode; nodeType: TNodeType);
      function texto: string;

      // Properties
      property id: String
           read getId write setId;
      property G: integer
           read getG write setG;
      property H: integer
           read getH write setH;
      property F: integer
           read getF;
      property nodeType: TNodeType
           read getType write setType;
      property ParentNode: cPathFindingNode
           read getParentNode write setParentNode;
    end;

implementation

constructor cPathFindingNode.Create(id: String; g: integer; h: integer; parentNode: cPathFindingNode; nodeType: TNodeType);
begin
  m_id     := id;
  m_g      := g;
  m_h      := h;
  m_parent := parentNode;
  m_type := nodeType;
end;

function cPathFindingNode.getId: String;
begin
  Result := m_id;
end;

function cPathFindingNode.getG: integer;
begin
  Result := m_g;
end;

function cPathFindingNode.getH: integer;
begin
  Result := m_h;
end;

function cPathFindingNode.getType: TNodeType;
begin
  Result := m_type;
end;

function cPathFindingNode.getF: integer;
begin
  Result := m_g + m_h;
end;

function cPathFindingNode.getParentNode: cPathFindingNode;
begin
  Result := m_parent;
end;

procedure cPathFindingNode.setId(Id: String);
begin
  m_id := Id;
end;

procedure cPathFindingNode.setG(G: integer);
begin
  m_g := G;
end;

procedure cPathFindingNode.setH(H: integer);
begin
  m_h := H;
end;

procedure cPathFindingNode.setType(nodeType: TNodeType);
begin
  m_type := nodeType;
end;

procedure cPathFindingNode.setParentNode(ParentNode: cPathFindingNode);
begin
  m_parent := ParentNode;
end;

function cPathFindingNode.texto: string;
var
  returnText: string;
begin
  returntext := 'id: ' + self.id;
  //returnText := 'id: ' + self.id + ' g: ' + inttostr(self.G) + ' h: ' + inttostr(self.H) + ' parent id: ' + self.ParentNode.id;
  result := returnText;
end;

end.

