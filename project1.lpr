program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, glscene_runtime, glscene_designtime, PathFindingNode,
  PathFindingManager, SceneParser, DataModel, SceneLoader, Cloud, Island, Tile,
  Position
  { you can add units after this };

const MCW_EM = DWord($133f);

{$R *.res}

begin
  //Set8087CW($133F);
  Set8087CW(MCW_EM);
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

