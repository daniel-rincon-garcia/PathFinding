unit PathFindingManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PathFindingNode, Contnrs, GLGeomObjects;

type
  cPathFindingManager = class
    private
      // Class attributes
      numberOfColumns: integer;
      numberOfRows: integer;

      openList: TObjectList;
      closeList: TObjectList;
      nodeList: TObjectList;
      currentNode: cPathFindingNode;

      // Class methods
      function getEndNode: cPathFindingNode;

      procedure fillNeighbourList(var neighbourList: TObjectList);
      procedure getRowAndColumFromId(id: String; var row: integer; var column: integer);

      function northWestNode(centralNode: cPathFindingNode): cPathFindingNode;
      function northNode(centralNode: cPathFindingNode): cPathFindingNode;
      function northEastNode(centralNode: cPathFindingNode): cPathFindingNode;
      function westNode(centralNode: cPathFindingNode): cPathFindingNode;
      function eastNode(centralNode: cPathFindingNode): cPathFindingNode;
      function southWestNode(centralNode: cPathFindingNode): cPathFindingNode;
      function southNode(centralNode: cPathFindingNode): cPathFindingNode;
      function southEastNode(centralNode: cPathFindingNode): cPathFindingNode;

      function getCurrentNode: cPathFindingNode;

      function getNode(row: integer; column: integer): cPathFindingNode;

      function existsInOpenList(node :cPathFindingNode): boolean;
      function existsInCloseList(node :cPathFindingNode): boolean;

      function computeG(adyacentNode :cPathFindingNode): integer;
      function computeH(adyacentNode :cPathFindingNode): integer;

      function movementDiagonal(adyacentNode :cPathFindingNode): boolean;
      function isBetterPath(node :cPathFindingNode; var g: integer; var h: integer): boolean;

    public
      constructor create(numOfColumns: integer; numOfRows: integer);

      procedure AddNode(Cube: cPathFindingNode);

      procedure calculatesPath(Player: TGLArrowLine); // Cambiar por Seeker: Character
      procedure resetPath;

      function getStartNode: cPathFindingNode;

      function startNodeIsSelected: boolean;
      function endNodeIsSelected: boolean;
      function texto: string;
  end;

implementation

uses GLColor, GLMovement, VectorGeometry, Math, Dialogs;

constructor cPathFindingManager.create(numOfColumns: integer; numOfRows: integer);
begin
  numberOfColumns := numOfColumns;
  numberOfRows := numOfRows;

  nodeList := TObjectList.create;
end;

procedure cPathFindingManager.AddNode(Cube: cPathFindingNode);
begin
  nodeList.Add(Cube);
end;

procedure cPathFindingManager.calculatesPath(Player: TGLArrowLine);
var
  i, j, m, g, h: integer;
  neighbourList: TObjectList;
  node: cPathFindingNode;
  pathNodeColor: TGLColor;
  pathList: TObjectList;

  Movement: TGLMovement;
  Path:     TGLMovementPath;
  PathNode:     TGLPathNode;

  upVector: {TVector}TAffineVector;
  turnAngle: Float;

  pathNodeColorCreated: boolean;
begin
  pathNodeColorCreated := false;
  {pathNodeColor := TGLColor.Create(Self);
  pathNodeColor.Red := 0;
  pathNodeColor.Green := 0;
  pathNodeColor.Blue := 255;
  pathNodeColor.Alpha := 255;}
  if (not Assigned(openList)) then
    begin
      openList := TObjectList.create;
      //ShowMessage('Lista abierta creada');
    end
  else
    begin
      //ShowMessage('La lista abierta está creada de antes y su número de elementos es: ' + IntToStr(openList.Count));
      for i:= 0 to openList.Count - 1 do
        openList.Extract(openList.Items[0]);
      //ShowMessage('La lista abierta se ha quedado con: ' + IntToStr(openList.Count) + ' elementos.');
    end;
  if (not Assigned(closeList)) then
    begin
      closeList := TObjectList.create;
      //ShowMessage('Lista cerrada creada');
    end
  else
    begin
      //ShowMessage('La lista cerrada está creada de antes y su número de elementos es: ' + IntToStr(closeList.Count));
      for i:= 0 to closeList.Count - 1 do
        closeList.Extract(closeList.Items[0]);
      //ShowMessage('La lista cerrada se ha quedado con: ' + IntToStr(closeList.Count) + ' elementos.');
    end;
  // Añade el vértice inicial a la lista Abierta
  openList.Add(getStartNode);
  neighbourList := TObjectList.create();
  // Repite lo siguiente mientras la lista Abierta no sea vacía
  // o el vértice actual sea distinto del vértice destino.
  repeat
    currentNode := getCurrentNode;
    //ShowMessage('currentNode.id: '+ currentNode.id);
    if (currentNode <> getEndNode) then
      begin
        if not pathNodeColorCreated then
          begin
            pathNodeColor := TGLColor.Create(currentNode);
            pathNodeColor.Red := 0;
            pathNodeColor.Green := 0;
            pathNodeColor.Blue := 255;
            pathNodeColor.Alpha := 255;
            pathNodeColorCreated := true;
          end;
        currentNode.Material.FrontProperties.Emission := pathNodeColor;
        // Tras comprobar que el vértice actual es distinto del vértice de destino, tomamos todos los vértices
        // adyacentes al actual
        fillNeighbourList(neighbourList);
        //ShowMessage('neighbourList.Count: '+ IntToStr(neighbourList.Count));
        for j := 0 to neighbourList.Count - 1 do
          // Por cada vértice adyacente comprobamos que dicho adyacente o bien no se
          // encuentre en la lista Abierta y Cerrada o por el contrario que se encuentre en la lista Abierta.
          begin
            //ShowMessage('vértice adyacente '+(neighbourList.Items[j] as cPathFindingNode).id);
            if (existsInOpenList(neighbourList.Items[j] as cPathFindingNode) = false) AND
               (existsInCloseList(neighbourList.Items[j] as cPathFindingNode) = false) then
              begin
                // Si el vértice adyacente a tratar no figura en la lista Abierta, ni en la lista Cerrada,
                // asignamos a dicho vértice los costes F, G y H. Hacemos que el vértice adyacente apunte a su
                // vértice padre, es decir, al vértice actual y finalmente añadimos el vértice adyacente a la
                // lista Abierta.
                (neighbourList.Items[j] as cPathFindingNode).G := computeG(neighbourList.Items[j] as cPathFindingNode);
                (neighbourList.Items[j] as cPathFindingNode).H := computeH(neighbourList.Items[j] as cPathFindingNode);
                (neighbourList.Items[j] as cPathFindingNode).ParentNode := currentNode;
                openList.Add(neighbourList.Items[j]);

                {(neighbourList.Items[j] as cPathFindingNode).Material.FrontProperties.Emission := pathNodeColor;}
              end
            else
              begin
                if existsInOpenList(neighbourList.Items[j] as cPathFindingNode) = true then
                  // Si por el contrario nos encontramos en la situación de que el vértice adyacente ya figuraba
                  // en la lista Abierta, hemos de comprobar si el camino para este vértice es mejor usando el
                  // coste G como baremo. Un coste G menor significa que este será un mejor camino. Si es así,
                  // cambiamos el padre del vértice adyacente (al estar en la lista Abierta es de suponer que ya
                  // tendría asignado un padre) tomando como padre el vértice actual y recalculamos los
                  // costes F, G y H.
                  begin
                    if isBetterPath(neighbourList.Items[j] as cPathFindingNode, g, h) then
                      begin
                        (neighbourList.Items[j] as cPathFindingNode).ParentNode := currentNode;
                        // recalcular costes F, G y H.
                        (neighbourList.Items[j] as cPathFindingNode).G := g;
                        (neighbourList.Items[j] as cPathFindingNode).H := h;

                        {(neighbourList.Items[j] as cPathFindingNode).Material.FrontProperties.Emission := pathNodeColor;}
                      end;
                  end;
              end;
          end;
      end;
  until ((openList.Count = 0) OR (currentNode = getEndNode));
  // Por tanto, y recordando lo ya dicho, nuestro algoritmo finalizará cuando sea añadido a la lista Abierta el
  // vértice destino en cuyo caso el camino habrá sido encontrado, o por el contrario se falle en encontrar el
  // vértice destino y la lista Abierta esté vacía. En este caso no hay camino.
  pathNodeColor.Red := 255;
  pathNodeColor.Green := 0;
  pathNodeColor.Blue := 0;
  pathNodeColor.Alpha := 255;

  pathList := TObjectList.create();

  //if (openList.Count > 0) then
    begin
      //ShowMessage('Ruta encontrada!');
      node := getEndNode;
      while (node <> nil) do
        begin
          //ShowMessage(node.id);
          //node.Position.Y := -41;

          node.Material.FrontProperties.Emission := pathNodeColor;
          pathList.Insert(0, node);
          node := node.ParentNode;
        end;
    end;


    // Create a movement, a path and the first node of the path.
    //if (not Assigned(Player)) then
    //  Player := {TGLCube}TGLArrowLine(GLScene1.Objects.AddNewChild({TGLCube}TGLArrowLine));
    Player.Position := (pathList.Items[0] as cPathFindingNode).Position;
    Player.Position.Y := -69;
    Movement   := GetOrCreateMovement(Player);
    Movement.ClearPaths;
    //Movement.OnPathTravelStop := PathTravelStop;
    //Movement.OnAllPathTravelledOver := PathAllTravelledOver;
    Path := Movement.AddPath;
    Path.ShowPath := True;
    //Path.PathSplineMode := lsmLines;

    // Path.StartTime := 2;
    // Path.Looped := True;

    PathNode       := Path.AddNodeFromObject(Player);
    PathNode.Speed := 1.0;
    PathNode.RotationAsVector := VectorMake(0, 0, 0);

    turnAngle := 0;
    for i:= 0 to pathList.count - 1 do
      begin
        // Add a node.
        PathNode       := Path.AddNode;
        PathNode.Speed := 1.0;

        //PathNode.PositionAsVector := VectorMake(-10, 0, 0, 1);
        PathNode.PositionAsVector := VectorMake((pathList.Items[i] as cPathFindingNode).Position.X, (pathList.Items[i] as cPathFindingNode).Position.Y + 1, (pathList.Items[i] as cPathFindingNode).Position.Z);
        if (i <> pathList.count - 1) then
          begin
            //PathNode.RotationAsVector := VectorMake((pathList.Items[i + 1] as cPathFindingNode).Position.X - (pathList.Items[i] as cPathFindingNode).Position.X,
            //                                        (pathList.Items[i + 1] as cPathFindingNode).Position.Y - (pathList.Items[i] as cPathFindingNode).Position.Y,
            //                                        0);
            //upVector[0] := (pathList.Items[i + 1] as cPathFindingNode).Position.X - (pathList.Items[i] as cPathFindingNode).Position.X;
            //upVector[1] := (pathList.Items[i + 1] as cPathFindingNode).Position.Y - (pathList.Items[i] as cPathFindingNode).Position.Y;
            //upVector[2] := 0;
            //ShowMessage(FloatToStr((pathList.Items[i] as cPathFindingNode).Position.X) + '_' + FloatToStr((pathList.Items[i] as cPathFindingNode).Position.Y));
            //Player.Direction.AsAffineVector := upVector;
            //PathNode.DirectionX := i;//(pathList.Items[i + 1] as cPathFindingNode).Position.X - (pathList.Items[i] as cPathFindingNode).Position.X;
            //PathNode.DirectionY := 4;//(pathList.Items[i + 1] as cPathFindingNode).Position.Y - (pathList.Items[i] as cPathFindingNode).Position.Y;
            //PathNode.DirectionZ := 3;//(pathList.Items[i + 1] as cPathFindingNode).Position.Z - (pathList.Items[i] as cPathFindingNode).Position.Z;
            //PathNode.UpAsVector := VectorMake((pathList.Items[i + 1] as cPathFindingNode).Position.X - (pathList.Items[i] as cPathFindingNode).Position.X,
            //                                         (pathList.Items[i + 1] as cPathFindingNode).Position.Y - (pathList.Items[i] as cPathFindingNode).Position.Y,
            //                                         (pathList.Items[i + 1] as cPathFindingNode).Position.Z - (pathList.Items[i] as cPathFindingNode).Position.Z);

            //PathNode.RotationAsVector := VectorMake(0, 45*i, 0);
            //ShowMessage(PathNode.ToString);



            if (southNode(pathList.Items[i] as cPathFindingNode) <> nil) and
               ((pathList.Items[i + 1] as cPathFindingNode).id = southNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(0 - PathNode.RotationAsVector[1]) < abs(-360 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 0, 0) // 0 o -360
                else
                  PathNode.RotationAsVector := VectorMake(0, -360, 0); // 0 o -360
              end
            else if (southEastNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = southEastNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(45 - PathNode.RotationAsVector[1]) < abs(-315 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 45, 0) // 45 o -315
                else
                  PathNode.RotationAsVector := VectorMake(0, -315, 0); // 45 o -315
              end
            else if (eastNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = eastNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                //ShowMessage(FloatToStr(PathNode.RotationAsVector[2]));
                if (abs(90 - PathNode.RotationAsVector[1]) < abs(-270 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 90, 0) // 90 o -270
                else
                  PathNode.RotationAsVector := VectorMake(0, -270, 0); // 90 o -270
              end
            else if (northEastNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = northEastNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(135 - PathNode.RotationAsVector[1]) < abs(-225 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 135, 0) // 135 o -225
                else
                  PathNode.RotationAsVector := VectorMake(0, -225, 0); // 135 o -225
              end
            else if (northNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = northNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(180 - PathNode.RotationAsVector[1]) < abs(-180 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 180, 0) // 180 o -180
                else
                  PathNode.RotationAsVector := VectorMake(0, -180, 0); // 180 o -180
              end
            else if (northWestNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = northWestNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(225 - PathNode.RotationAsVector[1]) < abs(-135 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 225, 0) // 225 o -135
                else
                  PathNode.RotationAsVector := VectorMake(0, -135, 0); // 225 o -135
              end
            else if (westNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = westNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(270 - PathNode.RotationAsVector[1]) < abs(-90 - PathNode.RotationAsVector[1])) then
                   PathNode.RotationAsVector := VectorMake(0, 270, 0) // 270 o -90;  270 - 0 = 270; -90 - 0 = -90;
                else
                   PathNode.RotationAsVector := VectorMake(0, -90, 0); // 270 o -90;  270 - 0 = 270; -90 - 0 = -90;
              end
            else if (southWestNode(pathList.Items[i] as cPathFindingNode) <> nil) and
                ((pathList.Items[i + 1] as cPathFindingNode).id = southWestNode(pathList.Items[i] as cPathFindingNode).id) then
              begin
                if (abs(315 - PathNode.RotationAsVector[1]) < abs(-45 - PathNode.RotationAsVector[1])) then
                  PathNode.RotationAsVector := VectorMake(0, 315, 0) // 315 o -45
                else
                  PathNode.RotationAsVector := VectorMake(0, -45, 0); // 315 o -45
              end;
          end;
      end;

    // Activatived the current path.
    Movement.ActivePathIndex := 0;

    // Start persecution!
    Movement.StartPathTravel;

  pathNodeColor.Destroy;

  for i:= 0 to neighbourList.Count - 1 do
    neighbourList.Extract(neighbourList.Items[0]);
  neighbourList.Destroy;

  for i:= 0 to pathList.Count - 1 do
    pathList.Extract(pathList.Items[0]);
  pathList.Destroy;
  //else
  //  ShowMessage('Oops no hay camino amigo...');
end;

procedure cPathFindingManager.resetPath;
var
  i: integer;
begin
  for i := 0 to nodeList.Count - 1 do
   begin
     if ((nodeList.Items[i] as cPathFindingNode).nodeType = StartNode) or
        ((nodeList.Items[i] as cPathFindingNode).nodeType = EndNode) then
       begin
         (nodeList.Items[i] as cPathFindingNode).nodeType := MidleNode;
       end;
     (nodeList.Items[i] as cPathFindingNode).Material.FrontProperties.Emission.Color := clrBlack;
     (nodeList.Items[i] as cPathFindingNode).G := 0;
     (nodeList.Items[i] as cPathFindingNode).H := 0;
     (nodeList.Items[i] as cPathFindingNode).ParentNode := nil;
   end;
end;

function cPathFindingManager.getStartNode: cPathFindingNode;
var
  i: integer;
begin
  for i := 0 to nodeList.Count - 1 do
   begin
     if (nodeList.Items[i] as cPathFindingNode).nodeType = StartNode then
       begin
         Result := (nodeList.Items[i] as cPathFindingNode);
         break;
       end;
   end;
end;

function cPathFindingManager.getEndNode: cPathFindingNode;
var
  i: integer;
begin
  //ShowMessage('nodeList.Count: '+IntToStr(nodeList.Count));
  for i := 0 to nodeList.Count - 1 do
   begin
     //ShowMessage('id: '+(nodeList.Items[i] as cPathFindingNode).id);
     if (nodeList.Items[i] as cPathFindingNode).nodeType = EndNode then
       begin
         Result := (nodeList.Items[i] as cPathFindingNode);
         break;
       end;
   end;
end;

function cPathFindingManager.northWestNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  northWestRow, northWestColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  //ShowMessage('cPathFindingManager.northWestNode row '+IntToStr(row));
  //ShowMessage('cPathFindingManager.northWestNode column '+IntToStr(column));
  if (row > 0) then
    northWestRow := row - 1;
  if (column > 0) then
    northWestColumn := column - 1;
  if ((row = 0) OR (column = 0)) then
    Result := nil
  else
    Result := getNode(northWestRow, northWestColumn);
  //ShowMessage('northWestNode '+Result.id);
end;

function cPathFindingManager.northNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  northRow: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (row > 0) then
    northRow := row - 1;
  if (row = 0) then
    Result := nil
  else
    Result := getNode(northRow, column);
  //ShowMessage('northNode '+Result.id);
end;

function cPathFindingManager.northEastNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  northEastRow, northEastColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (row > 0) then
    northEastRow := row - 1;
  if (column < numberOfColumns) then
    northEastColumn := column + 1;
  if ((row = 0) OR (columN = numberOfColumns)) then
    Result := nil
  else
    Result := getNode(northEastRow, northEastColumn);
  //ShowMessage('northEastNode '+Result.id);
end;

function cPathFindingManager.westNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  westColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (column > 0) then
    westColumn := column - 1;
  if (column = 0) then
    Result := nil
  else
    Result := getNode(row, westColumn);
  //ShowMessage('westNode '+Result.id);
end;

function cPathFindingManager.eastNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  eastColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (column < numberOfColumns) then
    eastColumn := column + 1;
  if (column = numberOfColumns) then
    Result := nil
  else
    Result := getNode(row, eastColumn);
  //ShowMessage('eastNode '+Result.id);
end;

function cPathFindingManager.southWestNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  southWestRow, southWestColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (row < numberOfRows) then
    southWestRow := row + 1;
  if (column > 0) then
    southWestColumn := column - 1;
  if ((row = numberOfRows) OR (column = 0)) then
    Result := nil
  else
    Result := getNode(southWestRow, southWestColumn);
  //ShowMessage('southWestNode '+Result.id);
end;

function cPathFindingManager.southNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  southRow: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (row < numberOfRows) then
    southRow := row + 1;
  if (row = numberOfRows) then
    Result := nil
  else
    Result := getNode(southRow, column);
  //ShowMessage('southNode '+Result.id);
end;

function cPathFindingManager.southEastNode(centralNode: cPathFindingNode): cPathFindingNode;
var
  row, column: integer;
  southEastRow, southEastColumn: integer;
begin
  getRowAndColumFromId(centralNode.id, row, column);
  if (row < numberOfRows) then
    southEastRow := row + 1;
  if (column < numberOfColumns) then
    southEastColumn := column + 1;
  if ((row = numberOfRows) OR (column = numberOfColumns)) then
    Result := nil
  else
    Result := getNode(southEastRow, southEastColumn);
  //ShowMessage('southEastNode '+Result.id);
end;

procedure cPathFindingManager.fillNeighbourList(var neighbourList: TObjectList);
var
  i: integer;
  nWestNode, nNode, nEastNode, wNode, eNode, sWestNode, sNode, sEastNode: cPathFindingNode;
begin
  //ShowMessage('currentNode.id: '+ currentNode.id);
  for i:= 0 to neighbourList.Count - 1 do
    neighbourList.Extract(neighbourList.Items[0]);
  //ShowMessage('currentNode.id: '+ currentNode.id);
  nWestNode := northWestNode(currentNode);
  if (nWestNode <> nil) AND (nWestNode.nodeType <> WallNode) AND
     (northNode(currentNode).nodeType <> WallNode) AND (westNode(currentNode).nodeType <> WallNode) then
    neighbourList.Add(nWestNode);
  nNode := northNode(currentNode);
  if (nNode <> nil) AND (nNode.nodeType <> WallNode) then
    neighbourList.Add(nNode);
  nEastNode := northEastNode(currentNode);
  if (nEastNode <> nil) AND (nEastNode.nodeType <> WallNode) AND
     (northNode(currentNode).nodeType <> WallNode) AND (eastNode(currentNode).nodeType <> WallNode) then
    neighbourList.Add(nEastNode);
  wNode := westNode(currentNode);
  if (wNode <> nil) AND (wNode.nodeType <> WallNode) then
    neighbourList.Add(wNode);
  eNode := eastNode(currentNode);
  if (eNode <> nil) AND (eNode.nodeType <> WallNode) then
    neighbourList.Add(eNode);
  sWestNode := southWestNode(currentNode);
  if (sWestNode <> nil) AND (sWestNode.nodeType <> WallNode) AND
     (southNode(currentNode).nodeType <> WallNode) AND (westNode(currentNode).nodeType <> WallNode) then
    neighbourList.Add(sWestNode);
  sNode := southNode(currentNode);
  if (sNode <> nil) AND (sNode.nodeType <> WallNode) then
    neighbourList.Add(sNode);
  sEastNode := southEastNode(currentNode);
  if (sEastNode <> nil) AND (sEastNode.nodeType <> WallNode) AND
     (southNode(currentNode).nodeType <> WallNode) AND (eastNode(currentNode).nodeType <> WallNode) then
    neighbourList.Add(sEastNode);
end;

procedure cPathFindingManager.getRowAndColumFromId(id: String; var row: integer; var column: integer);
begin
  //ShowMessage('getRowAndColumFromId: '+id);
  row := StrToInt(Copy(id, 0, Pos('_', id) - 1));
  column := StrToInt(Copy(id, Pos('_', id) + 1, Length(id) - Pos('_', id)));
end;

function cPathFindingManager.getNode(row: integer; column: integer): cPathFindingNode;
var
  id: String;
  i: integer;
  nodeFound: boolean;
begin
  nodeFound := false;
  id := IntToStr(row) + '_' + IntToStr(column);
  for i:= 0 to nodeList.Count - 1 do
    if (nodeList.Items[i] as cPathFindingNode).id = id then
      begin
        Result := (nodeList.Items[i] as cPathFindingNode);
        nodeFound := true;
        break;
      end;
  if (nodeFound = false) then
      Result := nil;
end;

// Busca de entre la lista Abierta aquel vértice con menor valor F, para acto seguido hacerlo vértice actual.
// Con esta acción el vértice es eliminado de la lista Abierta y pasado a la lista Cerrada.
function cPathFindingManager.getCurrentNode: cPathFindingNode;
var
  i: integer;
  _currentNode: cPathFindingNode;
  minimumF: integer;
begin
  _currentNode := (openList.Items[0] as cPathFindingNode);
  minimumF := _currentNode.F;
  for i := 1 to openList.Count - 1 do
    begin
      if ((openList.Items[i] as cPathFindingNode).F < minimumF) then
        begin
          _currentNode := (openList.Items[i] as cPathFindingNode);
          minimumF := _currentNode.F;
        end;
    end;
  //ShowMessage('minimumF: '+IntToStr(minimumF) + ' current node id: ' + _currentNode.id);
  openList.Extract(_currentNode);
  closeList.Add(_currentNode);
  Result := _currentNode;
end;

function cPathFindingManager.existsInOpenList(node :cPathFindingNode): boolean;
var
  returnValue: boolean;
  i: integer;
begin
  returnValue := false;
  for i := 0 to openList.Count - 1 do
    begin
      if ((openList.Items[i]) as cPathFindingNode) = node then
        begin
          returnValue := true;
          break;
        end;
    end;
  Result := returnValue;
end;

function cPathFindingManager.existsInCloseList(node :cPathFindingNode): boolean;
var
  returnValue: boolean;
  i: integer;
begin
  returnValue := false;
  for i := 0 to closeList.Count - 1 do
    begin
      if ((closeList.Items[i]) as cPathFindingNode) = node then
        begin
          returnValue := true;
          break;
        end;
    end;
  Result := returnValue;
end;

function cPathFindingManager.computeG(adyacentNode :cPathFindingNode): integer;
begin
  if (movementDiagonal(adyacentNode) = true) then
    Result := currentNode.G + 14
  else
    Result := currentNode.G + 10;
  //ShowMessage('computeG: '+IntToStr(Result));
end;

function cPathFindingManager.computeH(adyacentNode :cPathFindingNode): integer;
var
  adyacentNodeRow, adyacentNodeColumn: integer;
  targetNodeRow, targetNodeColumn: integer;
begin
  getRowAndColumFromId(adyacentNode.id, adyacentNodeRow, adyacentNodeColumn);
  //getRowAndColumFromId(currentNode.id, targetNodeRow, targetNodeColumn);
  getRowAndColumFromId(getEndNode.id, targetNodeRow, targetNodeColumn);
  if (targetNodeRow > adyacentNodeRow) AND (targetNodeColumn > adyacentNodeColumn) then
    Result := (targetNodeRow - adyacentNodeRow) + (targetNodeColumn - adyacentNodeColumn)
  else if (targetNodeRow < adyacentNodeRow) AND (targetNodeColumn > adyacentNodeColumn) then
    Result := (adyacentNodeRow - targetNodeRow) + (targetNodeColumn - adyacentNodeColumn)
  else if (targetNodeRow > adyacentNodeRow) AND (targetNodeColumn < adyacentNodeColumn) then
    Result := (targetNodeRow - adyacentNodeRow) + (adyacentNodeColumn - targetNodeColumn)
  else if (targetNodeRow < adyacentNodeRow) AND (targetNodeColumn < adyacentNodeColumn) then
    Result := (adyacentNodeRow - targetNodeRow) + (adyacentNodeColumn - targetNodeColumn)
  else if (targetNodeRow = adyacentNodeRow) AND (targetNodeColumn > adyacentNodeColumn) then
    Result := targetNodeColumn - adyacentNodeColumn
  else if (targetNodeRow = adyacentNodeRow) AND (targetNodeColumn < adyacentNodeColumn) then
    Result := adyacentNodeColumn - targetNodeColumn
  else if (targetNodeRow > adyacentNodeRow) AND (targetNodeColumn = adyacentNodeColumn) then
    Result := targetNodeRow - adyacentNodeRow
  else if (targetNodeRow < adyacentNodeRow) AND (targetNodeColumn = adyacentNodeColumn) then
    Result := adyacentNodeRow - targetNodeRow;
  Result := 10 * Result;
  //ShowMessage('computeH: '+IntToStr(Result));
end;

function cPathFindingManager.movementDiagonal(adyacentNode :cPathFindingNode): boolean;
var
  adyacentNodeRow, adyacentNodeColumn: integer;
  currentNoderow, currentNodeColumn: integer;
  returnValue: boolean;
begin
  returnValue := false;
  getRowAndColumFromId(adyacentNode.id, adyacentNodeRow, adyacentNodeColumn);
  getRowAndColumFromId(currentNode.id, currentNoderow, currentNodeColumn);
  if (adyacentNodeRow <> currentNoderow) AND (adyacentNodeColumn <> currentNodeColumn) then
    returnValue := true;
  Result := returnValue;
end;

// Hemos de comprobar si el camino para este vértice es mejor usando el coste G como baremo.
// Un coste G menor significa que este será un mejor camino.
function cPathFindingManager.isBetterPath(node :cPathFindingNode; var g: integer; var h: integer): boolean;
var
  returnValue: boolean;
begin
  returnValue := false;
  //ShowMessage('node.id: ' + node.id + ' node.G: ' + IntToStr(node.G) + ' computeG(node): ' + IntToStr(computeG(node)));
  if (node.G > computeG(node)) then
    begin
      returnValue := true;
      g := computeG(node);
      h := node.H;
    end;
  Result := returnValue;
end;

function cPathFindingManager.startNodeIsSelected: boolean;
var
  i: integer;
  returnValue: boolean;
begin
  returnValue := False;
  for i:= 0 to nodeList.Count - 1 do
    if (nodeList[i] as cPathFindingNode).nodeType = StartNode then
      begin
        returnValue := True;
        break;
      end;
  Result := returnValue;
end;

function cPathFindingManager.endNodeIsSelected: boolean;
var
  i: integer;
  returnValue: boolean;
begin
  returnValue := False;
  for i:= 0 to nodeList.Count - 1 do
    if (nodeList[i] as cPathFindingNode).nodeType = EndNode then
      begin
        returnValue := True;
        break;
      end;
  Result := returnValue;
end;

function cPathFindingManager.texto: string;
var
  i: integer;
  returnValue: String;
begin
  returnValue := 'numberOfColumns: ' + IntToStr(numberOfColumns) + ' numberOfRows: ' + IntToStr(numberOfRows) + #13#10;
  for i:= 0 to nodeList.Count - 1 do
    begin
      returnValue := returnValue + (nodeList.Items[i] as cPathFindingNode).texto + #13#10;
    end;
  Result := returnValue;
end;

end.

