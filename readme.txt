1.- Bajar Lazarus de http://switch.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2032%20bits/Lazarus%200.9.30.2/lazarus-0.9.30.2-fpc-2.4.4-win32.exe --> lazarus-0.9.30.2-fpc-2.4.4-win32.exe
2.- Bajar GLScene de https://glscene.svn.sourceforge.net/svnroot/glscene/trunk --> GLScene_v1.1_November_2011_SVN_revision_5991.zip
3.- Copiar la carpeta GLScene dentro de C:\lazarus\components
4.- Abrir el archivo glscene_designtime.lpk
5.- Pulsar en "Compilar"
6.- Pulsar en "Instalar"
7.- Ir a Herramientas --> Configurar "Construir Lazarus" ... marcar todo en Limpiar+Construir y pulsar en Reconstruir.
8.- Añadir la ruta ..\Source\Base en "Opciones del Compilador" --> "Archivos de Inclusión (-Fi):" si no encuentra la unidad GLContext.pas
http://www.youtube.com/watch?v=Zw0ztdNEXCQ
