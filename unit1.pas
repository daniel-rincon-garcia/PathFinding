unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GLScene, GLObjects, GLNavigator, GLCadencer, GLVectorFileObjects,
  GLGeomObjects, GLSLShader, GLLCLViewer, BaseClasses, KeyBoard, GlKeyboard,
  Contnrs, PathFindingNode, PathFindingManager, GLColor,SceneLoader;

const
  numberOfColumns = 38;
  numberOfRows = 28;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GLArrowLine1: TGLArrowLine;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Avion: TGLFreeForm;
    GLSphere1: TGLSphere;
    Modelo: TGLFreeForm;
    GLLines1: TGLLines;
    Satellite: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLNavigator1: TGLNavigator;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLUserInterface1: TGLUserInterface;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  //Satellite: TGLCube;
  //Avion: TGLCube;
  longitude: single;
  moveCameraByMouse: boolean;

  //player: TGLCube;
  player: TGLArrowLine;

  pathFindingManagerInstance: cPathFindingManager;
  scnLoader: TSceneLoader;
implementation

{$R *.lfm}

uses VectorGeometry, SceneParser;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  gridAltitude = 0{35};
var
  lon: single;
  lat: single;
  k, l: integer;
  Cubo: TGLCube;
  //Cubo: TGLPoints;
  Cube: cPathFindingNode;
  m: integer;
  polygon: TGLPolygon;
  upVector: TAffineVector;

  //Documento: TXMLDocument;
  //Hijo: TDOMNode;
  //wall: TDOMNode;
  //GLWallObject, CB: cPathFindingNode;
  //{i, j,}items: Integer;
  //lista: TStringList;
  scnParser: cSceneParser;
begin
  { TODO -oDani : Crear varios mundos }
  Player := TGLArrowLine(GLScene1.Objects.AddNewChild(TGLArrowLine));
  //pathFindingManagerInstance := cPathFindingManager.Create(numberOfColumns, numberOfRows);

  moveCameraByMouse := true;
{  lon := 0;
  repeat
   begin
    lat := 0;
    repeat
     begin
      Cubo := TGLCube(GLScene1.Objects.AddNewChild(TGLCube));
      //Cubo := TGLPoints(GLScene1.Objects.AddNewChild(TGLPoints));
      //Cubo.Pickable := False;
      //Cubo.size := 5;
      Cubo.Position.X := (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat))*cos(DegToRad(lon));
      Cubo.Position.Y := (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat))*sin(DegToRad(lon));
      Cubo.Position.Z := (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat));
      //Cubo.Position.X := (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat))*cos(DegToRad(lon));
      //Cubo.Position.Y := (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat));
      //Cubo.Position.Z := (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat))*sin(DegToRad(lon));

      {polygon := TGLPolygon(GLScene1.Objects.AddNewChild(TGLPolygon));
      polygon.AddNode((GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat - 0.05))*cos(DegToRad(lon - 0.05)), (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat - 0.05))*sin(DegToRad(lon - 0.05)), (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat - 0.05)));
      polygon.AddNode((GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat - 0.05))*cos(DegToRad(lon + 0.05)), (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat - 0.05))*sin(DegToRad(lon + 0.05)), (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat - 0.05)));
      polygon.AddNode((GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat + 0.05))*cos(DegToRad(lon + 0.05)), (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat + 0.05))*sin(DegToRad(lon + 0.05)), (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat + 0.05)));
      polygon.AddNode((GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat + 0.05))*cos(DegToRad(lon - 0.05)), (GLSphere1.Radius + gridAltitude)*sin(DegToRad(lat + 0.05))*sin(DegToRad(lon - 0.05)), (GLSphere1.Radius + gridAltitude)*cos(DegToRad(lat + 0.05)));
      }upVector[0] := Cubo.Position.X{ / value};
      upVector[1] := Cubo.Position.Y{ / value};
      upVector[2] := Cubo.Position.Z{ / value};
      Cubo.Up.AsAffineVector := upVector;

      lat := lat + 5;
     end;
    until lat = 180;
    lon := lon + 10;
   end;
  until lon = 360;
  //Satellite := TGLCube(GLScene1.Objects.AddNewChild(TGLCube));
  longitude := 0;   }
  GLUserInterface1.MouseLookActivate;
  scnParser := cSceneParser.Create;
  scnParser.parseScene;
{lista := TStringList.Create;

try
ReadXMLFile(Documento, 'scene.xml');
// usando las propiedades FirstChild y NextSibling
Hijo := Documento.DocumentElement.FirstChild;
while Assigned(Hijo) do
begin
if 'cloud' = Hijo.NodeName then
  begin

  end
else
  if ('island' = Hijo.NodeName) and Hijo.HasChildNodes then
    begin
      //showmessage('voy a crear pathFindingManagerInstance');
      //pathFindingManagerInstance := cPathFindingManager.Create(StrToInt(Hijo.Attributes.GetNamedItem('columns').NodeValue), StrToInt(Hijo.Attributes.GetNamedItem('rows').NodeValue));
      with Hijo.ChildNodes do
      try
        with Hijo.FirstChild.ChildNodes do
        try
          for items := 0 to (count - 1) do
            begin
              lista.Add(item[items].Attributes.GetNamedItem('row').NodeValue + '_' + item[items].Attributes.GetNamedItem('column').NodeValue);
            end;
        finally
          ShowMessage('voy a borrar los nodos hijo de ' + Hijo.FirstChild.NodeName);
          //Hijo.FirstChild.Free; // free memory for walls
          Free;
        end;
        for k:= 0 to StrToInt(Hijo.Attributes.GetNamedItem('columns').NodeValue) do
          for l:= 0 to StrToInt(Hijo.Attributes.GetNamedItem('rows').NodeValue) do
            begin
              Cube := cPathFindingNode(GLScene1.Objects.AddNewChild(cPathFindingNode));
              Cube.id := IntToStr(l) + '_' + IntToStr(k);
              Cube.Name := 'Cube' + Cube.id;
              Cube.Position.X  := StrToInt(Hijo.Attributes.GetNamedItem('X').NodeValue) + k;
              if lista.IndexOf(Cube.id) = -1 then
                Cube.Position.Y  := StrToInt(Hijo.Attributes.GetNamedItem('Y').NodeValue) - 70
              else
                Cube.Position.Y  := StrToInt(Hijo.Attributes.GetNamedItem('Y').NodeValue) - 69;
              Cube.Position.Z  := StrToInt(Hijo.Attributes.GetNamedItem('Z').NodeValue) + l;
              if lista.IndexOf(Cube.id) = -1 then
                Cube.nodeType := MidleNode
              else
                begin
                  Cube.nodeType := WallNode;
                  Cube.Pickable := False;
                end;
              pathFindingManagerInstance.AddNode(Cube);
            end;
      finally
        ShowMessage('voy a borrar los nodos hijo de ' + Hijo.NodeName);
        Free; // free memory for wall
      end;
    end;
Hijo := Hijo.NextSibling;
end;
finally
ShowMessage('voy a borrar los nodos hijo de ' + Documento.DocumentElement.NodeName);
Documento.DocumentElement.ChildNodes.Free; // free memory for cloud and island
ShowMessage('voy a borrar ' + Documento.DocumentElement.NodeName);
Documento.DocumentElement.Free; // free memory for world
Documento.Free;
lista.Free;
//wall.Free;
//Hijo.Free;
end;}

  //nodeList := TObjectList.create;
  {for k:= 0 to numberOfColumns do
      for l:= 0 to numberOfRows do
        begin
           Cube := cPathFindingNode(GLScene1.Objects.AddNewChild(cPathFindingNode));
           Cube.id := IntToStr(l) + '_' + IntToStr(k);
           Cube.Name := 'ube' + Cube.id;
           Cube.Position.X  := k;
           Cube.Position.Y  := -40;
           Cube.Position.Z  := l;
           //Cube.Pickable := False;
           Cube.nodeType := MidleNode;
           if ((k = 4) AND (l = 2)) OR
              ((k = 5) AND (l = 2)) OR
              ((k = 6) AND (l = 2)) OR
              ((k = 7) AND (l = 2)) OR
              ((k = 8) AND (l = 2)) OR
              ((k = 9) AND (l = 2)) OR
              ((k = 10) AND (l = 2)) OR
              ((k = 11) AND (l = 2)) OR
              ((k = 12) AND (l = 2)) OR
              ((k = 13) AND (l = 2)) OR
              ((k = 14) AND (l = 2)) OR
              ((k = 15) AND (l = 2)) OR
              ((k = 16) AND (l = 2)) OR
              ((k = 17) AND (l = 2)) OR
              ((k = 18) AND (l = 2)) OR
              ((k = 19) AND (l = 2)) OR
              ((k = 20) AND (l = 2)) OR
              ((k = 21) AND (l = 2)) OR
              ((k = 22) AND (l = 2)) OR
              ((k = 23) AND (l = 2)) OR
              ((k = 24) AND (l = 2)) OR
              ((k = 25) AND (l = 2)) OR
              ((k = 26) AND (l = 2)) OR
              ((k = 27) AND (l = 2)) OR
              ((k = 28) AND (l = 2)) OR
              ((k = 29) AND (l = 2)) OR
              ((k = 30) AND (l = 2)) OR
              ((k = 31) AND (l = 2)) OR

              ((k = 8) AND (l = 4)) OR
              ((k = 8) AND (l = 5)) OR
              ((k = 8) AND (l = 6)) OR
              ((k = 8) AND (l = 7)) OR
              ((k = 8) AND (l = 8)) OR
              ((k = 8) AND (l = 9)) OR
              ((k = 8) AND (l = 10)) OR
              ((k = 8) AND (l = 11)) OR
              ((k = 8) AND (l = 12)) OR
              ((k = 8) AND (l = 13)) OR
              ((k = 8) AND (l = 14)) OR
              ((k = 8) AND (l = 15)) OR
              ((k = 8) AND (l = 16)) OR
              ((k = 8) AND (l = 17)) OR
              ((k = 8) AND (l = 18)) OR
              ((k = 8) AND (l = 19)) OR
              ((k = 8) AND (l = 20)) OR
              ((k = 8) AND (l = 21)) OR

              ((k = 5) AND (l = 25)) OR
              ((k = 6) AND (l = 25)) OR
              ((k = 7) AND (l = 25)) OR
              ((k = 8) AND (l = 25)) OR
              ((k = 9) AND (l = 25)) OR
              ((k = 10) AND (l = 25)) OR
              ((k = 11) AND (l = 25)) OR
              ((k = 12) AND (l = 25)) OR
              ((k = 13) AND (l = 25)) OR

              ((k = 12) AND (l = 8)) OR
              ((k = 13) AND (l = 8)) OR
              ((k = 14) AND (l = 8)) OR
              ((k = 15) AND (l = 8)) OR
              ((k = 16) AND (l = 8)) OR
              ((k = 17) AND (l = 8)) OR
              ((k = 18) AND (l = 8)) OR
              ((k = 19) AND (l = 8)) OR
              ((k = 20) AND (l = 8)) OR
              ((k = 21) AND (l = 8)) OR
              ((k = 22) AND (l = 8)) OR
              ((k = 23) AND (l = 8)) OR
              ((k = 24) AND (l = 8)) OR
              ((k = 25) AND (l = 8)) OR

              ((k = 22) AND (l = 12)) OR
              ((k = 22) AND (l = 13)) OR
              ((k = 22) AND (l = 14)) OR
              ((k = 22) AND (l = 15)) OR
              ((k = 22) AND (l = 16)) OR
              ((k = 22) AND (l = 17)) OR
              ((k = 22) AND (l = 18)) OR
              ((k = 22) AND (l = 19)) OR
              ((k = 22) AND (l = 20)) OR
              ((k = 22) AND (l = 21)) OR
              ((k = 22) AND (l = 22)) OR
              ((k = 22) AND (l = 23)) OR
              ((k = 22) AND (l = 24)) OR

              ((k = 33) AND (l = 18)) OR
              ((k = 32) AND (l = 18)) OR
              ((k = 31) AND (l = 18)) OR
              ((k = 30) AND (l = 18)) OR
              ((k = 29) AND (l = 18)) OR
              ((k = 28) AND (l = 18)) OR
              ((k = 27) AND (l = 18)) OR

              ((k = 30) AND (l = 5)) OR
              ((k = 30) AND (l = 6)) OR
              ((k = 30) AND (l = 7)) OR
              ((k = 30) AND (l = 8)) OR
              ((k = 30) AND (l = 9)) OR
              ((k = 30) AND (l = 10)) OR
              ((k = 30) AND (l = 11)) OR
              ((k = 30) AND (l = 12)) OR
              ((k = 30) AND (l = 13)) OR
              ((k = 30) AND (l = 14)) then
             begin
               Cube.Position.Y := -39;
               Cube.nodeType := WallNode;
               Cube.Pickable := False;
             end;
           if (k = 7) AND (l = 0) then
             begin
               //Cube.Position.Y := -41;
               //Cube.nodeType := StartNode;
               //Cube.Material.FrontProperties.Emission.Color := clrgreen;
             end;
           if (k = 25) AND (l = 22) then
             begin
               //Cube.Position.Y := -41;
               //Cube.nodeType := EndNode;
             end;
           //nodeList.Add(Cube);
           //pathFindingManagerInstance.AddNode(Cube);
        end;}
  scnLoader := TSceneLoader.create(GLScene1);
  scnLoader.loadScene(pathFindingManagerInstance);


























  {for m := 0 to nodeList.Count - 1 do
    begin
      ShowMessage('id: '+(nodeList.Items[m] as cPathFindingNode).id);
      //if (nodeList.Items[i] as cPathFindingNode).nodeType = EndNode then
      //  Result := (nodeList.Items[i] as cPathFindingNode);
    end;}

  // Aplicar hilos para no bloquear la interfaz gráfica de usuario.
  // Crear clase pathfinding manager que haga uso de la clase cPathFindingNode
  //calculatesPath;

  {ShowMessage('getStartNode '+getStartNode.id);
  ShowMessage('northWestNode '+northWestNode(getStartNode).id);
  ShowMessage('northNode '+northNode(getStartNode).id);
  ShowMessage('northEastNode '+northEastNode(getStartNode).id);
  ShowMessage('westNode '+westNode(getStartNode).id);
  ShowMessage('eastNode '+eastNode(getStartNode).id);
  ShowMessage('southWestNode '+southWestNode(getStartNode).id);
  ShowMessage('southNode '+southNode(getStartNode).id);
  ShowMessage('southEastNode '+southEastNode(getStartNode).id);}

  Satellite.LoadFromFile('Models\Assets\Satelite.obj');
  Avion.LoadFromFile('Models\Assets\Avion.obj');
  Modelo.LoadFromFile('Models\Assets\Modelo.obj');


end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  pathFindingManagerInstance.calculatesPath(Player);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (moveCameraByMouse) then
     moveCameraByMouse := false
  else
     moveCameraByMouse := true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
const
  satelliteAltitude = 5;
  avionAltitude = 2;
  modeloAltitude= 12;
var
  velocidad: real;
  value: real;
  upVector: {TVector}TAffineVector;
  upVector2:TAffineVector;
  upVector3:TAffineVector;

begin
  if (moveCameraByMouse = true) then
     begin
          GLUserInterface1.MouseLookActivate;
          GLUserInterface1.MouseUpdate;
          GLUserInterface1.MouseLook;
     end
  else
      GLUserInterface1.MouseLookDeactivate;
  velocidad := 50;
  if IsKeyDown('w') then
   GLNavigator1.MoveForward(velocidad*deltaTime);
  if IsKeyDown('s') then
   GLNavigator1.MoveForward(-velocidad*deltaTime);
  if IsKeyDown('d') then
   GLNavigator1.StrafeHorizontal(velocidad*deltaTime);
  if IsKeyDown('a') then
   GLNavigator1.StrafeHorizontal(-velocidad*deltaTime);

  Satellite.Position.X := (GLSphere1.Radius + satelliteAltitude)*sin(DegToRad(90.0))*cos(DegToRad(longitude));
  Satellite.Position.Y := (GLSphere1.Radius + satelliteAltitude)*sin(DegToRad(90.0))*sin(DegToRad(longitude));
  Satellite.Position.Z := (GLSphere1.Radius + satelliteAltitude)*cos(DegToRad(90.0));


  Avion.Position.X := (GLSphere1.Radius + avionAltitude)*sin(DegToRad(45.0))*cos(DegToRad(longitude));
  Avion.Position.Y := (GLSphere1.Radius + avionAltitude)*sin(DegToRad(45.0))*sin(DegToRad(longitude));
  Avion.Position.Z := (GLSphere1.Radius + avionAltitude)*cos(DegToRad(45.0));

  Modelo.Position.X := (GLSphere1.Radius + modeloAltitude)*sin(DegToRad(170.0))*cos(DegToRad(longitude));
  Modelo.Position.Y := (GLSphere1.Radius + modeloAltitude)*sin(DegToRad(170.0))*sin(DegToRad(longitude));
  Modelo.Position.Z := (GLSphere1.Radius + modeloAltitude)*cos(DegToRad(170.0));



  {if Assigned(Player) then
    begin
      GLCamera1.Position.X := Player.Position.X;
      GLCamera1.Position.Y := Player.Position.Y;
      GLCamera1.Position.Z := Player.Position.Z;

      GLCamera1.Direction := Player.Direction;
    end;}

  longitude := longitude + 0.06;
  // No es necesaria la normalización del vector, lo hace automáticamente
  //value := sqrt(sqr(Satellite.Position.X) + sqr(Satellite.Position.Y) + sqr(Satellite.Position.Z));
  upVector[0] := Satellite.Position.X{ / value};
  upVector[1] := Satellite.Position.Y{ / value};
  upVector[2] := Satellite.Position.Z{ / value};
  Satellite.Up.AsAffineVector := upVector;


  upVector2[0] := Avion.Position.X;
  upVector2[1] := Avion.Position.Y;
  upVector2[2] := Avion.Position.Z;
  Avion.Up.AsAffineVector := upVector2;


  upVector3[0] := Modelo.Position.X;
  upVector3[1] := Modelo.Position.Y;
  upVector3[2] := Modelo.Position.Z;
  Modelo.Up.AsAffineVector := upVector3;


  //http://www.taringa.net/posts/ciencia-educacion/6628940/Planos-Tangentes-a-un-punto-de-una-superficie_.html
  //http://es.wikihow.com/normalizar-un-vector
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  { Declaring a TGLCustomSceneObject allow manipulate any objects created;
  with a TGLCube we only can manipulate cubes, with TGLCone only cones, etc. }
  NewSelection : TGLCustomSceneObject;
  //NewSelection : cPathFindingNode;
begin
  { GetPickedObject return the object under the coordinates (x, y)
  of viewer; in as TGLCustomSceneObject we make an typecast,
  a convertion of type, because this function return an object of the type TGLBaseSceneObject. }
  // GLSelection.pas MAX_OBJECT_STACK_DEPTH = 512; // constant has been changed by 20480
  NewSelection := GLSceneViewer1.Buffer.GetPickedObject (x, y) as TGLCustomSceneObject;

  { If in position (x, y) not have nothing, newSelection return
  value None, therefore, before try change the color
  of object, we to prove that, really, has selected something. }

//Memo1.Lines.Append(NewSelection.Name);
  if (Assigned (NewSelection)) AND (Pos('Tile', NewSelection.Name) <> 0) then
     begin
       //Memo1.Lines.Append(NewSelection.Name);
       if (pathFindingManagerInstance.startNodeIsSelected) then
         begin
           if (pathFindingManagerInstance.endNodeIsSelected) then
             begin
               ShowMessage('The path is covered yet');
               pathFindingManagerInstance.resetPath;
               (NewSelection as cPathFindingNode).nodeType := StartNode;
               NewSelection.Material.FrontProperties.Emission.Color := clrCoral;
             end
           else
             begin
               if (pathFindingManagerInstance.getStartNode.id <> (NewSelection as cPathFindingNode).id) and
                  ((NewSelection as cPathFindingNode).nodeType <> WallNode) then
                 begin
                   (NewSelection as cPathFindingNode).nodeType := EndNode;
                   (NewSelection as cPathFindingNode).Material.FrontProperties.Emission.Color := clrCoral;
                   pathFindingManagerInstance.calculatesPath(Player);
                 end;
             end;
         end
       else
         begin
           if (NewSelection as cPathFindingNode).nodeType <> WallNode then
             begin
               (NewSelection as cPathFindingNode).nodeType := StartNode;
               (NewSelection as cPathFindingNode).Material.FrontProperties.Emission.Color := clrCoral;
             end;
         end;
     end;
end;



//Make floating point operations more accurate on a x86-CPU

{$IFDEF FPC}
  {$IFDEF CPU386}
  {$ASMMODE intel}

  {$ENDIF}
{$ENDIF}

initialization

{$IFDEF CPU386}
  Set8087CW($133F);
{$ENDIF}

finalization
  { Reset the FPU to the previous state }
  Set8087CW($1332);

end.

