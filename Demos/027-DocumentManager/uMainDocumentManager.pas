// MIT License
//
// Copyright (c) 2013 Gustavo Enriquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMainDocumentManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.JSON, System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Memo,
  FMX.Edit, FMX.Memo.Types, FMX.ScrollBox,
  uMakerAi.Chat, uMakerAi.Chat.Ollama,
  uMakerAi.Embeddings.core,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Graph.Documents;

type
  TFormDocManager = class(TForm)
    BtnAbrir: TButton;
    BtnGenerarJson: TButton;
    BtnAgregarGrafo: TButton;
    BtnBuscar: TButton;
    BtnExtraerEntidades: TButton;
    BtnExtraerRelaciones: TButton;
    BtnAgregarRelacion: TButton;
    LblArchivo: TLabel;
    LblDocumento: TLabel;
    LblJson: TLabel;
    LblResultados: TLabel;
    MemoDocumento: TMemo;
    MemoJson: TMemo;
    MemoResultados: TMemo;
    EditBuscar: TEdit;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnAbrirClick(Sender: TObject);
    procedure BtnGenerarJsonClick(Sender: TObject);
    procedure BtnAgregarGrafoClick(Sender: TObject);
    procedure BtnBuscarClick(Sender: TObject);
    procedure BtnExtraerEntidadesClick(Sender: TObject);
    procedure BtnExtraerRelacionesClick(Sender: TObject);
    procedure BtnAgregarRelacionClick(Sender: TObject);
  private
    FOllamaEmbeddings: TAiOllamaEmbeddings;
    FRagGraph: TAiRagGraph;
    FDocManager: TAiRagDocumentManager;
    FOllamaChat: TAiOllamaChat;
    procedure ChatError(Sender: TObject; const ErrorMsg: string;
      Exception: Exception; const AResponse: IHTTPResponse);
  end;

var
  FormDocManager: TFormDocManager;

implementation

{$R *.fmx}

procedure TFormDocManager.FormCreate(Sender: TObject);
begin
  // Embeddings locales via Ollama
  FOllamaEmbeddings := TAiOllamaEmbeddings.Create(Self);
  FOllamaEmbeddings.Model := 'mxbai-embed-large:latest';
  FOllamaEmbeddings.Dimensions := 1024;
  FOllamaEmbeddings.Url := 'http://localhost:11434/';

  // Grafo RAG en memoria
  FRagGraph := TAiRagGraph.Create(Self);
  FRagGraph.Embeddings := FOllamaEmbeddings;

  // Document Manager
  FDocManager := TAiRagDocumentManager.Create(Self);
  FDocManager.Graph := FRagGraph;
  FDocManager.ChunkSize := 1000;

  // Chat LLM local para generacion de JSON y extraccion de relaciones
  FOllamaChat := TAiOllamaChat.Create(Self);
  FOllamaChat.Model := 'gpt-oss:20b';
  FOllamaChat.Asynchronous := False;
  FOllamaChat.OnError := ChatError;
end;

procedure TFormDocManager.FormDestroy(Sender: TObject);
begin
  FDocManager.Free;
  FOllamaChat.Free;
  FRagGraph.Free;
  FOllamaEmbeddings.Free;
end;

procedure TFormDocManager.ChatError(Sender: TObject; const ErrorMsg: string;
  Exception: Exception; const AResponse: IHTTPResponse);
begin
  ShowMessage('Error del LLM: ' + ErrorMsg);
end;

procedure TFormDocManager.BtnAbrirClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Text files|*.txt|All files|*.*';
  if OpenDialog1.Execute then
  begin
    MemoDocumento.Lines.LoadFromFile(OpenDialog1.FileName);
    LblArchivo.Text := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TFormDocManager.BtnGenerarJsonClick(Sender: TObject);
var
  Prompt, Res: string;
begin
  if MemoDocumento.Lines.Text.Trim.IsEmpty then
  begin
    ShowMessage('Primero abra un documento de texto.');
    Exit;
  end;

  Prompt :=
    'Analiza el siguiente documento de texto y genera un JSON jer' + #225 + 'rquico con esta estructura exacta:' + sLineBreak +
    '{' + sLineBreak +
    '  "name": "<nombre descriptivo del documento>",' + sLineBreak +
    '  "label": "Document",' + sLineBreak +
    '  "text": "<resumen breve del documento completo>",' + sLineBreak +
    '  "properties": {' + sLineBreak +
    '    "tipo": "<tipo de documento detectado>",' + sLineBreak +
    '    "idioma": "<idioma del documento>"' + sLineBreak +
    '  },' + sLineBreak +
    '  "parts": [' + sLineBreak +
    '    {' + sLineBreak +
    '      "name": "<nombre de la secci' + #243 + 'n>",' + sLineBreak +
    '      "label": "Seccion",' + sLineBreak +
    '      "text": "<texto de esta secci' + #243 + 'n>",' + sLineBreak +
    '      "parts": []' + sLineBreak +
    '    }' + sLineBreak +
    '  ]' + sLineBreak +
    '}' + sLineBreak +
    sLineBreak +
    'Reglas:' + sLineBreak +
    '- Identifica las secciones naturales del texto (t' + #237 + 'tulos, temas, p' + #225 + 'rrafos tem' + #225 + 'ticos)' + sLineBreak +
    '- Cada "part" debe tener "name", "label" y "text" obligatoriamente' + sLineBreak +
    '- Si una secci' + #243 + 'n tiene subsecciones, usa "parts" recursivamente' + sLineBreak +
    '- El campo "text" de cada part debe contener el texto original, no un resumen' + sLineBreak +
    '- Responde SOLO con el JSON, sin explicaciones adicionales ni markdown' + sLineBreak +
    sLineBreak +
    'DOCUMENTO:' + sLineBreak +
    MemoDocumento.Lines.Text;

  Cursor := crHourGlass;
  try
    FOllamaChat.Messages.Clear;
    Res := FOllamaChat.AddMessageAndRun(Prompt, 'user', []);
    MemoJson.Lines.Text := Res;
  finally
    Cursor := crDefault;
  end;
end;

procedure TFormDocManager.BtnAgregarGrafoClick(Sender: TObject);
var
  DocNode: TAiRagGraphNode;
  Parts: TArray<TDocumentPart>;
  I: Integer;
begin
  if MemoJson.Lines.Text.Trim.IsEmpty then
  begin
    ShowMessage('Primero genere el JSON con IA.');
    Exit;
  end;

  Cursor := crHourGlass;
  try
    try
      DocNode := FDocManager.AddDocumentFromJSON(MemoJson.Lines.Text);

      Parts := FDocManager.GetDocumentParts(DocNode);

      MemoResultados.Lines.Clear;
      MemoResultados.Lines.Add('Documento agregado: ' + DocNode.Name);
      MemoResultados.Lines.Add('Chunks creados: ' + IntToStr(Length(Parts)));
      MemoResultados.Lines.Add('');
      MemoResultados.Lines.Add('--- Partes del documento ---');

      for I := 0 to High(Parts) do
      begin
        MemoResultados.Lines.Add(Format('[%d] %s (%s) - Profundidad: %d - Path: %s',
          [Parts[I].Order, Parts[I].Name, Parts[I].PartLabel,
           Parts[I].Depth, Parts[I].Path]));
      end;
    except
      on E: Exception do
        ShowMessage('Error al agregar documento: ' + E.Message);
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TFormDocManager.BtnBuscarClick(Sender: TObject);
var
  Res: string;
begin
  if EditBuscar.Text.Trim.IsEmpty then
  begin
    ShowMessage('Escriba un texto de b' + #250 + 'squeda.');
    Exit;
  end;

  Cursor := crHourGlass;
  try
    try
      Res := FDocManager.SearchDocumentsText(EditBuscar.Text, 5, 0.3);

      MemoResultados.Lines.Clear;
      if Res.Trim.IsEmpty then
        MemoResultados.Lines.Add('No se encontraron resultados.')
      else
        MemoResultados.Lines.Text := Res;
    except
      on E: Exception do
        ShowMessage('Error en b' + #250 + 'squeda: ' + E.Message);
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TFormDocManager.BtnExtraerEntidadesClick(Sender: TObject);
var
  Docs: TArray<TAiRagGraphNode>;
  TotalEnts, Ents: Integer;
  Doc: TAiRagGraphNode;
  Entities: TArray<TAiRagGraphNode>;
  I: Integer;
begin
  Docs := FDocManager.GetAllDocuments;
  if Length(Docs) = 0 then
  begin
    ShowMessage('No hay documentos en el grafo. Agregue uno primero.');
    Exit;
  end;

  Cursor := crHourGlass;
  try
    try
      FDocManager.Chat := FOllamaChat;
      TotalEnts := 0;

      for Doc in Docs do
      begin
        Ents := FDocManager.ExtractEntitiesOnly(Doc);
        TotalEnts := TotalEnts + Ents;
      end;

      MemoResultados.Lines.Clear;
      MemoResultados.Lines.Add(Format('Fase 1 completada - Entidades extra' + #237 + 'das: %d', [TotalEnts]));
      MemoResultados.Lines.Add('');

      // Listar entidades encontradas
      for Doc in Docs do
      begin
        Entities := FDocManager.GetDocumentEntities(Doc);
        MemoResultados.Lines.Add('--- Entidades de: ' + Doc.Name + ' ---');
        for I := 0 to High(Entities) do
          MemoResultados.Lines.Add(Format('  [%d] %s (%s)', [I + 1, Entities[I].Name, Entities[I].NodeLabel]));
        MemoResultados.Lines.Add('');
      end;
    except
      on E: Exception do
        ShowMessage('Error extrayendo entidades: ' + E.Message);
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TFormDocManager.BtnExtraerRelacionesClick(Sender: TObject);
var
  Docs: TArray<TAiRagGraphNode>;
  TotalRels, Rels: Integer;
  Doc: TAiRagGraphNode;
  Context: TDocumentContext;
begin
  Docs := FDocManager.GetAllDocuments;
  if Length(Docs) = 0 then
  begin
    ShowMessage('No hay documentos en el grafo. Agregue uno primero.');
    Exit;
  end;

  Cursor := crHourGlass;
  try
    try
      FDocManager.Chat := FOllamaChat;
      TotalRels := 0;

      for Doc in Docs do
      begin
        Rels := FDocManager.ExtractRelationshipsWithContext(Doc);
        TotalRels := TotalRels + Rels;
      end;

      MemoResultados.Lines.Clear;
      MemoResultados.Lines.Add(Format('Fase 2 completada - Relaciones extra' + #237 + 'das: %d', [TotalRels]));
      MemoResultados.Lines.Add('');

      // Mostrar contexto completo del primer documento
      if Length(Docs) > 0 then
      begin
        Context := FDocManager.GetDocumentContext(Docs[0]);
        MemoResultados.Lines.Add('--- Contexto del documento ---');
        MemoResultados.Lines.Add(Context.GetContextText);
      end;
    except
      on E: Exception do
        ShowMessage('Error extrayendo relaciones: ' + E.Message);
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TFormDocManager.BtnAgregarRelacionClick(Sender: TObject);
var
  FromName, ToName, EdgeLabel: string;
  FromNode, ToNode, Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  I: Integer;
begin
  // Solicitar datos al usuario
  FromName := '';
  if not InputQuery('Agregar Edge', 'Nombre del nodo origen (From):', FromName) then
    Exit;
  if FromName.Trim.IsEmpty then Exit;

  ToName := '';
  if not InputQuery('Agregar Edge', 'Nombre del nodo destino (To):', ToName) then
    Exit;
  if ToName.Trim.IsEmpty then Exit;

  EdgeLabel := '';
  if not InputQuery('Agregar Edge', 'Tipo de relaci' + #243 + 'n (label del edge):', EdgeLabel) then
    Exit;
  if EdgeLabel.Trim.IsEmpty then Exit;

  // Buscar nodos por nombre (case-insensitive en el grafo)
  FromNode := nil;
  ToNode := nil;
  for I := 0 to FRagGraph.Nodes.Items.Count - 1 do
  begin
    Node := TAiRagGraphNode(FRagGraph.Nodes.Items[I]);
    if SameText(Node.Name, FromName) then
      FromNode := Node;
    if SameText(Node.Name, ToName) then
      ToNode := Node;
    if (FromNode <> nil) and (ToNode <> nil) then
      Break;
  end;

  if FromNode = nil then
  begin
    ShowMessage('No se encontr' + #243 + ' el nodo origen: ' + FromName);
    Exit;
  end;

  if ToNode = nil then
  begin
    ShowMessage('No se encontr' + #243 + ' el nodo destino: ' + ToName);
    Exit;
  end;

  try
    Edge := FDocManager.AddEdge(FromNode, ToNode, EdgeLabel);
    MemoResultados.Lines.Add(Format('Edge creado: %s -[%s]-> %s',
      [FromNode.Name, Edge.EdgeLabel, ToNode.Name]));
  except
    on E: Exception do
      ShowMessage('Error al crear edge: ' + E.Message);
  end;
end;

end.
