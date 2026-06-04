program PPMDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.JSON,
  uMakerAi.Prompts;

procedure MostrarTemplateLocal(Prompts: TAiPrompts);
var
  S: string;
begin
  WriteLn('--- Templates locales ---');
  Prompts.AddString('reporte',
    'REPORTE DE VENTAS' + sLineBreak +
    'Cliente : <#Cliente>'  + sLineBreak +
    'Periodo : <#Periodo>'  + sLineBreak +
    'Total   : <#Total>');

  S := Prompts.GetTemplate('reporte',
    ['Cliente=Acme Corp', 'Periodo=Mayo 2026', 'Total=$14,500']);
  WriteLn(S);
  WriteLn;
end;

procedure BuscarEnRegistry(Prompts: TAiPrompts);
var
  SearchResult: TJSONObject;
  Results     : TJSONValue;
  Pkg         : TJSONObject;
  Name, Desc  : string;
  I, Count    : Integer;
begin
  WriteLn('--- Busqueda en el registry PPM ---');
  WriteLn('Registry: ', Prompts.PPMRegistryUrl);
  SearchResult := Prompts.SearchPPM('commit', 'prompt');
  if not Assigned(SearchResult) then
  begin
    WriteLn('No se pudo conectar al registry.');
    WriteLn;
    Exit;
  end;
  try
    Results := SearchResult.FindValue('packages');
    if Assigned(Results) and (Results is TJSONArray) then
    begin
      Count := TJSONArray(Results).Count;
      WriteLn('Paquetes encontrados: ', Count);
      for I := 0 to Count - 1 do
      begin
        if I >= 5 then Break;
        Pkg  := TJSONArray(Results).Items[I] as TJSONObject;
        Name := ''; Desc := '';
        Pkg.TryGetValue<String>('name', Name);
        Pkg.TryGetValue<String>('description', Desc);
        WriteLn('  ', I + 1, '. ', Name, ' -- ', Desc);
      end;
    end
    else
      WriteLn(SearchResult.ToJSON);
  finally
    SearchResult.Free;
  end;
  WriteLn;
end;

procedure DescargarPrompt(Prompts: TAiPrompts);
var
  Item: TAiPromptItem;
  S   : string;
begin
  WriteLn('--- LoadFromPPM ---');
  Item := Prompts.LoadFromPPM('commit-message');
  if not Assigned(Item) then
  begin
    WriteLn('No se encontro "commit-message" en el registry.');
    WriteLn;
    Exit;
  end;
  WriteLn('Descargado : ', Item.Nombre);
  WriteLn('Descripcion: ', Item.Descripcion);
  S := Prompts.GetTemplate('commit-message',
    ['commit_type=feat', 'scope=clientes',
     'change_summary=agrega busqueda en lista de clientes']);
  WriteLn;
  WriteLn('Prompt con variables:');
  WriteLn(S);
  WriteLn;
end;

var
  Prompts: TAiPrompts;
begin
  WriteLn('=============================================');
  WriteLn('  Cap. 31 -- PPM: Gestor de Paquetes MakerAI');
  WriteLn('=============================================');
  WriteLn;

  Prompts := TAiPrompts.Create(nil);
  try
    MostrarTemplateLocal(Prompts);
    BuscarEnRegistry(Prompts);
    DescargarPrompt(Prompts);
  finally
    Prompts.Free;
  end;

  WriteLn('Demo completado. Presiona Enter para salir.');
  ReadLn;
end.
