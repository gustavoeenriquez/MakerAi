// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// Demo: TAiTextEditorTool — edición de archivos con I/O real en disco
// Prueba todos los comandos: view, create, str_replace, insert, apply_diff.
program demo_text_editor;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  uMakerAi.Tools.TextEditor;

const
  TEST_FILE = 'demo_editor_test.txt';

procedure PrintSep(const Title: string);
begin
  WriteLn('');
  WriteLn('=== ', Title, ' ===');
end;

procedure Check(const TestName, Result, Expected: string);
begin
  if Pos(Expected, Result) > 0 then
    WriteLn('  [OK] ', TestName)
  else
  begin
    WriteLn('  [FAIL] ', TestName);
    WriteLn('    Esperado : ', Expected);
    WriteLn('    Obtenido : ', Result);
  end;
end;

var
  Editor: TAiTextEditorTool;
  Res: string;
begin
  WriteLn('==============================================');
  WriteLn('  demo_text_editor — TAiTextEditorTool test');
  WriteLn('==============================================');

  // Limpiar archivo de prueba si existe de un run anterior
  if FileExists(TEST_FILE) then
    DeleteFile(TEST_FILE);

  Editor := TAiTextEditorTool.Create(nil);
  try

    // -----------------------------------------------------------------------
    PrintSep('1. create — crear archivo nuevo');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"create","path":"' + TEST_FILE + '",' +
        '"file_text":"linea uno\nlinea dos\nlinea tres\n"}');
    Check('create OK', Res, 'exitosamente');
    WriteLn('    ', Res);

    // -----------------------------------------------------------------------
    PrintSep('2. view — ver todo el archivo');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"view","path":"' + TEST_FILE + '"}');
    Check('view contiene linea uno', Res, 'linea uno');
    Check('view contiene linea tres', Res, 'linea tres');
    WriteLn('    Contenido: [', Trim(Res), ']');

    // -----------------------------------------------------------------------
    PrintSep('3. view — rango de líneas');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"view","path":"' + TEST_FILE + '",' +
        '"view_range":[2,3]}');
    Check('view rango contiene linea dos', Res, 'linea dos');
    WriteLn('    Rango [2,3]: [', Trim(Res), ']');

    // -----------------------------------------------------------------------
    PrintSep('4. str_replace — reemplazo único');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"str_replace","path":"' + TEST_FILE + '",' +
        '"old_str":"linea dos","new_str":"LINEA DOS REEMPLAZADA"}');
    Check('str_replace OK', Res, 'exactly one location');
    WriteLn('    ', Res);

    // Verificar el reemplazo
    Res := Editor.Execute('{"command":"view","path":"' + TEST_FILE + '"}');
    Check('reemplazo aplicado', Res, 'LINEA DOS REEMPLAZADA');

    // -----------------------------------------------------------------------
    PrintSep('5. str_replace — error: no match');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"str_replace","path":"' + TEST_FILE + '",' +
        '"old_str":"texto que no existe","new_str":"nada"}');
    Check('no match detectado', Res, 'No match found');
    WriteLn('    ', Res);

    // -----------------------------------------------------------------------
    PrintSep('6. insert — insertar línea');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"insert","path":"' + TEST_FILE + '",' +
        '"insert_line":1,"insert_text":"LINEA INSERTADA EN POS 1"}');
    Check('insert OK', Res, 'exitosamente');
    WriteLn('    ', Res);

    Res := Editor.Execute('{"command":"view","path":"' + TEST_FILE + '"}');
    Check('linea insertada visible', Res, 'LINEA INSERTADA EN POS 1');
    WriteLn('    Estado actual:');
    WriteLn('    ', StringReplace(Trim(Res), LineEnding, ' | ', [rfReplaceAll]));

    // -----------------------------------------------------------------------
    PrintSep('7. apply_diff — parche unificado');
    // -----------------------------------------------------------------------
    // El archivo tiene ahora: LINEA INSERTADA... / linea uno / LINEA DOS... / linea tres
    // Vamos a reemplazar "linea uno" por "linea 1 via diff" con un diff
    Res := Editor.Execute(
        '{"command":"apply_diff","path":"' + TEST_FILE + '",' +
        '"diff_text":"--- a\n+++ b\n@@ -2,1 +2,1 @@\n-linea uno\n+linea 1 via diff\n"}');
    // apply_diff puede devolver éxito o error según el contexto exacto
    WriteLn('    apply_diff result: ', Res);

    // -----------------------------------------------------------------------
    PrintSep('8. create — error: archivo ya existe');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"create","path":"' + TEST_FILE + '",' +
        '"file_text":"no debería crearse"}');
    Check('create duplicado detectado', Res, 'ya existe');
    WriteLn('    ', Res);

    // -----------------------------------------------------------------------
    PrintSep('9. view — error: archivo no existe');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"view","path":"archivo_inexistente.txt"}');
    Check('archivo no existe detectado', Res, 'no existe');
    WriteLn('    ', Res);

    // -----------------------------------------------------------------------
    PrintSep('10. Comando desconocido');
    // -----------------------------------------------------------------------
    Res := Editor.Execute(
        '{"command":"borrar","path":"' + TEST_FILE + '"}');
    Check('comando desconocido detectado', Res, 'desconocido');
    WriteLn('    ', Res);

    // Limpiar archivo de prueba
    DeleteFile(TEST_FILE);

    WriteLn('');
    WriteLn('==============================================');
    WriteLn('  TAiTextEditorTool tests completados.');
    WriteLn('==============================================');

  finally
    Editor.Free;
  end;
end.
