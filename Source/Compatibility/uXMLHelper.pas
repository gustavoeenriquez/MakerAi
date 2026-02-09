// =========================================================================
// uXMLHelper.pas - Helper XML Multiplataforma
// =========================================================================
// Abstrae las diferencias entre Delphi (Xml.XMLDoc/IXMLDocument) y
// FPC (DOM/TXMLDocument) para operaciones XML como exportación GraphML.
//
// Uso:
//   var Helper: TXMLDocumentHelper;
//   Helper := TXMLDocumentHelper.Create;
//   try
//     Root := Helper.CreateElement('graphml');
//     Helper.AppendChild(Root);
//     Child := Helper.CreateElement('node');
//     Helper.SetAttribute(Child, 'id', '1');
//     Helper.AppendChildTo(Root, Child);
//     Helper.SaveToFile('output.graphml');
//   finally
//     Helper.Free;
//   end;
// =========================================================================
unit uXMLHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF FPC}
  DOM, XMLRead, XMLWrite
  {$ELSE}
  Xml.XMLDoc, Xml.XMLDom, Xml.XMLIntf
  {$ENDIF};

type
  // Tipos abstractos para nodos XML
  {$IFDEF FPC}
  TXMLNodeHandle = TDOMElement;
  TXMLCommentHandle = TDOMComment;
  {$ELSE}
  TXMLNodeHandle = IXMLNode;
  TXMLCommentHandle = IXMLNode;
  {$ENDIF}

  /// <summary>
  /// Helper multiplataforma para crear y manipular documentos XML.
  /// Abstrae las diferencias entre Delphi IXMLDocument y FPC TXMLDocument.
  /// </summary>
  TXMLDocumentHelper = class
  private
    {$IFDEF FPC}
    FDoc: TXMLDocument;
    {$ELSE}
    FDoc: IXMLDocument;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    
    // Creación de elementos
    function CreateElement(const AName: string): TXMLNodeHandle;
    function CreateComment(const AText: string): TXMLCommentHandle;
    
    // Manipulación de nodos
    procedure AppendChild(ANode: TXMLNodeHandle);
    procedure AppendChildTo(AParent, AChild: TXMLNodeHandle); overload;
    procedure AppendCommentTo(AParent: TXMLNodeHandle; AComment: TXMLCommentHandle);
    
    // Atributos
    procedure SetAttribute(ANode: TXMLNodeHandle; const AName, AValue: string);
    
    // Contenido de texto
    procedure SetText(ANode: TXMLNodeHandle; const AText: string);
    
    // Persistencia
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
  end;

implementation

{ TXMLDocumentHelper }

constructor TXMLDocumentHelper.Create;
begin
  inherited Create;
  {$IFDEF FPC}
  FDoc := TXMLDocument.Create;
  {$ELSE}
  FDoc := TXMLDocument.Create(nil);
  FDoc.Active := True;
  FDoc.Version := '1.0';
  FDoc.Encoding := 'UTF-8';
  FDoc.Options := [doNodeAutoIndent];
  {$ENDIF}
end;

destructor TXMLDocumentHelper.Destroy;
begin
  {$IFDEF FPC}
  FDoc.Free;
  {$ELSE}
  FDoc := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TXMLDocumentHelper.CreateElement(const AName: string): TXMLNodeHandle;
begin
  {$IFDEF FPC}
  Result := FDoc.CreateElement(AName);
  {$ELSE}
  // En Delphi, creamos un nodo hijo temporal y lo devolvemos
  // El nodo se añadirá al documento cuando se llame AppendChild
  if FDoc.DocumentElement = nil then
    Result := FDoc.AddChild(AName)
  else
    Result := FDoc.CreateElement(AName, '');
  {$ENDIF}
end;

function TXMLDocumentHelper.CreateComment(const AText: string): TXMLCommentHandle;
begin
  {$IFDEF FPC}
  Result := FDoc.CreateComment(AText);
  {$ELSE}
  Result := FDoc.CreateNode(AText, ntComment);
  {$ENDIF}
end;

procedure TXMLDocumentHelper.AppendChild(ANode: TXMLNodeHandle);
begin
  {$IFDEF FPC}
  FDoc.AppendChild(ANode);
  {$ELSE}
  // En Delphi, el primer AddChild ya establece DocumentElement
  // Si ya existe, esto añade al documento
  if FDoc.DocumentElement = nil then
    FDoc.DocumentElement := ANode as IXMLNode;
  {$ENDIF}
end;

procedure TXMLDocumentHelper.AppendChildTo(AParent, AChild: TXMLNodeHandle);
begin
  {$IFDEF FPC}
  AParent.AppendChild(AChild);
  {$ELSE}
  AParent.ChildNodes.Add(AChild);
  {$ENDIF}
end;

procedure TXMLDocumentHelper.AppendCommentTo(AParent: TXMLNodeHandle; AComment: TXMLCommentHandle);
begin
  {$IFDEF FPC}
  AParent.AppendChild(AComment);
  {$ELSE}
  AParent.ChildNodes.Add(AComment);
  {$ENDIF}
end;

procedure TXMLDocumentHelper.SetAttribute(ANode: TXMLNodeHandle; const AName, AValue: string);
begin
  {$IFDEF FPC}
  ANode.SetAttribute(AName, AValue);
  {$ELSE}
  ANode.Attributes[AName] := AValue;
  {$ENDIF}
end;

procedure TXMLDocumentHelper.SetText(ANode: TXMLNodeHandle; const AText: string);
begin
  {$IFDEF FPC}
  ANode.TextContent := AText;
  {$ELSE}
  ANode.Text := AText;
  {$ENDIF}
end;

procedure TXMLDocumentHelper.SaveToFile(const AFileName: string);
begin
  {$IFDEF FPC}
  WriteXMLFile(FDoc, AFileName);
  {$ELSE}
  FDoc.SaveToFile(AFileName);
  {$ENDIF}
end;

procedure TXMLDocumentHelper.SaveToStream(AStream: TStream);
begin
  {$IFDEF FPC}
  WriteXMLFile(FDoc, AStream);
  {$ELSE}
  FDoc.SaveToStream(AStream);
  {$ENDIF}
end;
end.

